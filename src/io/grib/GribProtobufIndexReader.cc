/*
 * (C) Copyright 2024-2026, met.no
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

#include "GribProtobufIndexReader.h"

#include "fimex/CDMAttribute.h"
#include "fimex/CDMDataType.h"
#include "fimex/CDMVariable.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/ProtobufCDM.h"
#include "fimex/SharedArray.h"

#include "GribProtobufIndexWriter.h" // for GRIB_PROTO_INDEX_MAGIC

#include <cstring>
#include <fstream>

#include "grib_index.pb.h"

namespace MetNoFimex {

namespace {

Logger_p logger = getLogger("fimex.GribProtobufIndexWriter");

void readGribIndex(GribCDMIndexer::grib_indexed& grib_indexed, const fimex_index::GribIndex& fgrib_index)
{
    grib_indexed.grib_files.reserve(fgrib_index.files_size());
    for (const auto& fgf : fgrib_index.files()) {
        grib_indexed.grib_files.push_back(fgf);
    }
#if 0
    std::cerr << "added " << grib_indexed.grib_files.size() << " grib files" << std::endl;
#endif

    for (const auto& fgv : fgrib_index.variables()) {
        auto& gv = grib_indexed.grib_vars[fgv.name()];

        gv.scale_factor = fgv.scale_factor();
        gv.add_offset = fgv.add_offset();

        gv.messages.reserve(fgv.chunks_size());
        for (const auto& fc : fgv.chunks()) {
            gv.messages.push_back({fc.file_index(), fc.message_start(), fc.message_size()});
        }
#if 0
        std::cerr << "added grib var '" << fgv.name() << "' sf=" << gv.scale_factor << " ao=" << gv.add_offset << " #msg=" << gv.messages.size() << std::endl;
#endif
    }
}
} // namespace

size_t getGribProtobufIndexMagicSize()
{
    return sizeof(FIMEX_PROTO_INDEX_MAGIC) + sizeof(GRIB_PROTO_INDEX_MAGIC);
}

bool checkGribProtobufIndexMagic(const char* magic, size_t magic_size)
{
    if (magic_size < getGribProtobufIndexMagicSize()) {
        return false;
    }
    if (strncmp(FIMEX_PROTO_INDEX_MAGIC, magic, sizeof(FIMEX_PROTO_INDEX_MAGIC)) != 0) {
        return false;
    }
    if (strncmp(GRIB_PROTO_INDEX_MAGIC, magic + sizeof(FIMEX_PROTO_INDEX_MAGIC), sizeof(GRIB_PROTO_INDEX_MAGIC)) != 0) {
        return false;
    }
    return true;
}

void readGribProtobufIndex(const std::string& inputfile, MetNoFimex::CDM& cdm, GribCDMIndexer::grib_indexed& grib_indexed)
{
    std::ifstream reader(inputfile, std::ios::binary);
    char magic_idx[getGribProtobufIndexMagicSize()];
    if (!reader.read(magic_idx, sizeof(magic_idx))) {
        throw std::runtime_error("could not read protobuf index magic");
    }
    if (!checkGribProtobufIndexMagic(magic_idx, sizeof(magic_idx))) {
        throw std::runtime_error("protobuf index magic mismatch");
    }

    fimex_index::GribIndex gin;
    if (!gin.ParseFromIstream(&reader)) {
        throw std::runtime_error("could not read protobuf");
    }

    ProtobufCDM::readCDM(cdm, gin.cdm());
    readGribIndex(grib_indexed, gin);
}

} // namespace MetNoFimex
