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

#include "GribProtobufIndexWriter.h"

#include "fimex/Data.h"
#include "fimex/FileUtils.h"
#include "fimex/Logger.h"
#include "fimex/ProtobufCDM.h"

#include <fstream>

namespace MetNoFimex {

namespace {

Logger_p logger = getLogger("fimex.GribProtobufIndexWriter");

}

// G = grib, 3 numbers for version
const char GRIB_PROTO_INDEX_MAGIC[4]{'G', 0, 0, 0};

void GribProtobufIndexWriter::write(const MetNoFimex::CDM& cdm, const GribCDMIndexer::grib_indexed& grib_indexed, const std::string& outputfile)
{
    std::ofstream out(outputfile, std::ios::binary);
    write(cdm, grib_indexed, out);
}

void GribProtobufIndexWriter::write(const MetNoFimex::CDM& cdm, const GribCDMIndexer::grib_indexed& grib_indexed, std::ostream& out)
{
    fimex_index::GribIndex gin;
    ProtobufCDM::writeCDM(gin.mutable_cdm(), cdm);

    writeGribIndex(&gin, grib_indexed);

    out.write(FIMEX_PROTO_INDEX_MAGIC, sizeof(FIMEX_PROTO_INDEX_MAGIC));
    out.write(GRIB_PROTO_INDEX_MAGIC, sizeof(GRIB_PROTO_INDEX_MAGIC));
    if (!gin.SerializeToOstream(&out)) {
        throw std::runtime_error("could not write protobuf");
    }
}

void GribProtobufIndexWriter::writeGribIndex(fimex_index::GribIndex* fgrib_index, const GribCDMIndexer::grib_indexed& grib_indexed)
{
    for (const auto& gs : grib_indexed.grib_files) {
        fgrib_index->add_files(extractFilename(gs));
    }

    for (const auto& gvv : grib_indexed.grib_vars) {
        auto fgv = fgrib_index->add_variables();
        fgv->set_name(gvv.first);

        const auto& gvd = gvv.second;
        fgv->set_scale_factor(gvd.scale_factor);
        fgv->set_add_offset(gvd.add_offset);

        auto fgvc = fgv->mutable_chunks();
        fgvc->Reserve(gvd.messages.size());
        for (const auto& gm : gvd.messages) {
            auto fm = fgvc->Add();
            fm->set_file_index(gm.file_index);
            fm->set_message_start(gm.message_start);
            fm->set_message_size(gm.message_size);
        }
    }
}

} // namespace MetNoFimex
