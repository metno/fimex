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

#ifndef FIMEX_NETCDFPROTOBUFINDEXREADER_H_
#define FIMEX_NETCDFPROTOBUFINDEXREADER_H_

#include "fimex/ChunkReader.h"

#include <cstdint>
#include <map>
#include <string>
#include <vector>

namespace MetNoFimex {

class CDM;

// ---------------------------------------------------------------------------
// Runtime representation of the parsed index (format-specific part only).
// ---------------------------------------------------------------------------

/// One HDF5 chunk record in CDM dimension order (dim 0 = fastest varying).
struct NetCDFChunkInfo
{
    uint64_t addr = 0;            ///< Byte offset of the chunk in the source file.
    uint64_t size = 0;            ///< Storage size in bytes (may be compressed).
    uint32_t file_index = 1;      ///< Index into NetCDFIndexed::files; 0 = invalid/fill.
    std::vector<uint64_t> offset; ///< Dataspace origin in CDM dim order.
};

/// One decoded filter in a variable's HDF5 filter pipeline.
/// Filters are stored in HDF5 application (compression) order;
/// decompression applies them in reverse.
struct NetCDFFilter
{
    enum class Kind { Shuffle, Deflate, Szip, Generic };
    Kind kind = Kind::Shuffle;

    // Szip parameters (valid when kind == Szip):
    uint32_t szip_bits_per_pixel = 0;
    uint32_t szip_pixels_per_block = 0;
    uint32_t szip_pixels_per_scanline = 0;
    bool szip_nn_coding = false;

    // Generic filter parameters (valid when kind == Generic):
    uint32_t generic_filter_id = 0;
    std::vector<uint32_t> generic_cd_values;
};

/// Chunk-layout metadata for one variable.
struct NetCDFVarChunks
{
    std::vector<uint64_t> chunk_shape; ///< Tile shape in CDM dim order.
    bool big_endian = false;           ///< True if chunk data bytes are big-endian.
    std::vector<NetCDFFilter> filters; ///< HDF5 filter pipeline, compression order.
    std::vector<NetCDFChunkInfo> chunks;

    // Optional flat grid for O(intersecting) chunk lookup.
    //
    // grid_dims[d] is the number of chunk tiles in CDM dimension d.
    // chunk_grid is indexed by g_0 + grid_dims[0]*(g_1 + grid_dims[1]*(...)),
    // where g_d = offset[d]/chunk_shape[d] — dimension 0 is fastest.
    // A value of -1 means the cell has no valid chunk (fill or absent).
    // chunk_grid is empty when the grid was not built (sparse dataset fallback).
    std::vector<size_t>  grid_dims;
    std::vector<int32_t> chunk_grid;
};

/// Complete parsed index for one or more NetCDF source files.
struct NetCDFIndexed
{
    /// Source filenames.  files[0] is always the empty-string sentinel; real files
    /// start at index 1.
    std::vector<std::string> files;
    std::map<std::string, NetCDFVarChunks> vars; ///< Keyed by CDM variable name.
};

// ---------------------------------------------------------------------------
// Magic-byte helpers (mirrors the GRIB protobuf index interface).
// ---------------------------------------------------------------------------

size_t getNetCDFProtobufIndexMagicSize();
bool checkNetCDFProtobufIndexMagic(const char* magic, size_t magic_size);

// ---------------------------------------------------------------------------
// Main entry point: parse the NetCDF protobuf index from @p reader and
// populate @p cdm and @p indexed.  @p url is used only for error messages.
// ---------------------------------------------------------------------------
void readNetCDFProtobufIndex(ChunkReader& reader, const std::string& url, MetNoFimex::CDM& cdm, NetCDFIndexed& indexed);

} // namespace MetNoFimex

#endif /* FIMEX_NETCDFPROTOBUFINDEXREADER_H_ */
