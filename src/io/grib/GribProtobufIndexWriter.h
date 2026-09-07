/*
 * Fimex, GribCDMReader.h
 *
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

#ifndef FIMEXINDEXWRITER_H_
#define FIMEXINDEXWRITER_H_

#include "fimex/CDM.h"
#include "fimex/StringUtils.h"

#include "GribCDMIndexer.h"

#include "grib_index.pb.h"

namespace MetNoFimex {

class GribProtobufIndexWriter
{
public:
    void write(const MetNoFimex::CDM& cdm, const GribCDMIndexer::grib_indexed& grib_indexed, const std::string& outputfile);
    void write(const MetNoFimex::CDM& cdm, const GribCDMIndexer::grib_indexed& grib_indexed, std::ostream& out);

private:
    void writeGribIndex(fimex_index::GribIndex* fgrib_index, const GribCDMIndexer::grib_indexed& grib_indexed);
    StringListBuilder fcdm_strings;
};

extern const char GRIB_PROTO_INDEX_MAGIC[4];

} // namespace MetNoFimex

#endif /* FIMEXINDEXWRITER_H_ */
