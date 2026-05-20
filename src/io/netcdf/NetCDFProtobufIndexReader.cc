/*
 * (C) Copyright 2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

#include "NetCDFProtobufIndexReader.h"

#include "NetCDFProtobufIndexWriter.h" // for NETCDF_PROTO_INDEX_MAGIC

#include "fimex/ChunkReader.h"
#include "fimex/Logger.h"
#include "fimex/ProtobufCDM.h"

#include "netcdf_index.pb.h"

#include <algorithm>
#include <cstring>
#include <stdexcept>

namespace MetNoFimex {

namespace {

Logger_p logger = getLogger("fimex.NetCDFProtobufIndexReader");

// Limits to prevent malformed index files from causing OOM or other security issues.
// These are well above any realistic NetCDF file while still bounding allocations.
constexpr int MAX_NC_DIMS = 64;                         // NetCDF classic max is 1024; HDF5 unlimited
constexpr int MAX_CHUNKS_PER_VAR = 1 << 24;             // 16M chunks per variable
constexpr uint64_t MAX_CHUNK_BYTES = uint64_t(1) << 30; // 1 GiB raw (compressed) per chunk
constexpr int MAX_FILES = 1 << 16;                      // 64K source files per index

void parseNetCDFIndex(NetCDFIndexed& indexed, const fimex_index::NetCDFIndex& nix)
{
    // ---- populate files list ------------------------------------------------
    // files[0] must always be the empty sentinel.
    if (nix.files_size() > MAX_FILES)
        throw std::runtime_error("index contains " + std::to_string(nix.files_size()) + " files, exceeding limit");
    if (nix.files_size() < 2)
        throw std::runtime_error("index contains " + std::to_string(nix.files_size()) + " files, need at least 1+1");
    indexed.files.reserve(static_cast<size_t>(nix.files_size()));
    for (const auto& f : nix.files())
        indexed.files.push_back(f);
    // Enforce the sentinel invariant: files[0] must be empty.
    if (!indexed.files[0].empty())
        throw std::runtime_error("index files[0] is not empty — corrupt index");

    for (const auto& fv : nix.variables()) {
        const int ndims = fv.chunk_shape_size();
        if (ndims > MAX_NC_DIMS)
            throw std::runtime_error("index variable '" + fv.name() + "' has " + std::to_string(ndims) + " dimensions, exceeding limit");

        if (fv.chunks_size() > MAX_CHUNKS_PER_VAR)
            throw std::runtime_error("index variable '" + fv.name() + "' has " + std::to_string(fv.chunks_size()) + " chunks, exceeding limit");

        NetCDFVarChunks& vc = indexed.vars[fv.name()];

        vc.chunk_shape.reserve(static_cast<size_t>(ndims));
        for (auto cs : fv.chunk_shape())
            vc.chunk_shape.push_back(static_cast<uint64_t>(cs));

        vc.big_endian = (fv.byte_order() == fimex_index::BYTE_ORDER_BE);

        vc.filters.reserve(static_cast<size_t>(fv.filters_size()));
        for (const auto& pf : fv.filters()) {
            NetCDFFilter f;
            switch (pf.kind_case()) {
            case fimex_index::Filter::kShuffle:
                f.kind = NetCDFFilter::Kind::Shuffle;
                break;
            case fimex_index::Filter::kDeflate:
                f.kind = NetCDFFilter::Kind::Deflate;
                break;
            case fimex_index::Filter::kSzip:
                f.kind = NetCDFFilter::Kind::Szip;
                f.szip_nn_coding = pf.szip().nn_coding();
                f.szip_pixels_per_block = pf.szip().pixels_per_block();
                f.szip_bits_per_pixel = pf.szip().bits_per_pixel();
                f.szip_pixels_per_scanline = pf.szip().pixels_per_scanline();
                break;
            case fimex_index::Filter::kGeneric:
                f.kind = NetCDFFilter::Kind::Generic;
                f.generic_filter_id = pf.generic().filter_id();
                for (auto cv : pf.generic().cd_values())
                    f.generic_cd_values.push_back(cv);
                break;
            default:
                throw std::runtime_error("index variable '" + fv.name() + "': unknown filter kind");
            }
            vc.filters.push_back(std::move(f));
        }

        vc.chunks.reserve(static_cast<size_t>(fv.chunks_size()));
        for (const auto& fc : fv.chunks()) {
            // Each chunk must carry exactly one offset coordinate per dimension.
            if (fc.offset_size() != ndims)
                throw std::runtime_error("index variable '" + fv.name() + "': chunk offset rank " + std::to_string(fc.offset_size()) + " != ndims " +
                                         std::to_string(ndims));
            // Cap the raw chunk size to prevent OOM in decodeChunk.
            if (fc.size() > MAX_CHUNK_BYTES)
                throw std::runtime_error("index variable '" + fv.name() + "': chunk size " + std::to_string(fc.size()) + " exceeds limit");

            NetCDFChunkInfo ci;
            ci.addr = fc.addr();
            ci.size = fc.size();
            // Validate file_index: must be within bounds of the files array.
            // Index 0 is the reserved sentinel for "missing/invalid"; it is
            // stored as-is and handled at read time (fill value returned).
            const uint32_t fi = fc.file_index();
            if (fi >= static_cast<uint32_t>(indexed.files.size()))
                throw std::runtime_error("index variable '" + fv.name() + "': chunk file_index " + std::to_string(fi) + " out of bounds (files size " +
                                         std::to_string(indexed.files.size()) + ")");
            ci.file_index = fi;
            ci.offset.reserve(static_cast<size_t>(ndims));
            for (auto off : fc.offset())
                ci.offset.push_back(static_cast<uint64_t>(off));
            vc.chunks.push_back(std::move(ci));
        }

        // Sort chunks by (offset[ndims-1], …, offset[0]) — slowest CDM dimension
        // first.  This ordering lets getDataSlice use std::lower_bound to skip to
        // the first potentially-intersecting chunk and break early once the slowest
        // dimension is past the requested slice end.
        // offset.size() == ndims is guaranteed by the validation above.
        std::sort(vc.chunks.begin(), vc.chunks.end(), [ndims](const NetCDFChunkInfo& a, const NetCDFChunkInfo& b) {
            for (int d = ndims - 1; d >= 0; --d) {
                if (a.offset[d] != b.offset[d])
                    return a.offset[d] < b.offset[d];
            }
            return false;
        });
    }
}

} // anonymous namespace

size_t getNetCDFProtobufIndexMagicSize()
{
    return sizeof(FIMEX_PROTO_INDEX_MAGIC) + sizeof(NETCDF_PROTO_INDEX_MAGIC);
}

bool checkNetCDFProtobufIndexMagic(const char* magic, size_t magic_size)
{
    if (magic_size < getNetCDFProtobufIndexMagicSize())
        return false;
    if (std::strncmp(FIMEX_PROTO_INDEX_MAGIC, magic, sizeof(FIMEX_PROTO_INDEX_MAGIC)) != 0)
        return false;
    if (std::strncmp(NETCDF_PROTO_INDEX_MAGIC, magic + sizeof(FIMEX_PROTO_INDEX_MAGIC), sizeof(NETCDF_PROTO_INDEX_MAGIC)) != 0)
        return false;
    return true;
}

void readNetCDFProtobufIndex(ChunkReader& reader, const std::string& url, MetNoFimex::CDM& cdm, NetCDFIndexed& indexed)
{
    const size_t magic_size = getNetCDFProtobufIndexMagicSize();
    const size_t total_size = reader.size();

    if (total_size < magic_size)
        throw std::runtime_error("file too small to be a NetCDF protobuf index: " + url);

    // ---- verify magic -------------------------------------------------------
    std::vector<char> magic_buf(magic_size);
    reader.read(0, magic_size, reinterpret_cast<unsigned char*>(magic_buf.data()));

    if (!checkNetCDFProtobufIndexMagic(magic_buf.data(), magic_size))
        throw std::runtime_error("magic mismatch in NetCDF protobuf index: " + url);

    // ---- load protobuf body into memory and parse ---------------------------
    // Protobuf indices are typically small (metadata only).  Loading the whole
    // body avoids needing a seekable std::istream wrapper over ChunkReader.
    const size_t body_size = total_size - magic_size;
    std::vector<uint8_t> body(body_size);
    if (body_size > 0)
        reader.read(magic_size, body_size, body.data());

    fimex_index::NetCDFIndex nix;
    if (!nix.ParseFromArray(body.data(), static_cast<int>(body_size)))
        throw std::runtime_error("cannot parse NetCDF protobuf index: " + url);

    ProtobufCDM::readCDM(cdm, nix.cdm());
    parseNetCDFIndex(indexed, nix);
}

} // namespace MetNoFimex
