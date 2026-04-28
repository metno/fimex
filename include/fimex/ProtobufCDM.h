/*
 * Fimex, ProtobufCDM.h
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

#ifndef FIMEX_PROTOBUFCDM_H_
#define FIMEX_PROTOBUFCDM_H_

#include "fimex/CDM.h"
#include "fimex/StringUtils.h"

namespace fimex_index {
class CDM;
class CDMAttribute;
class CDMData;
} // namespace fimex_index

namespace MetNoFimex {

extern const char FIMEX_PROTO_INDEX_MAGIC[8];

namespace ProtobufCDM {

void readCDM(MetNoFimex::CDM& cdm, const fimex_index::CDM& fcdm);
void readAttribute(MetNoFimex::CDMAttribute& att, const fimex_index::CDMAttribute& fatt, const fimex_index::CDM& fcdm);
DataPtr readData(const fimex_index::CDMData& fdata, const fimex_index::CDM& fcdm);

void writeCDM(fimex_index::CDM* fcdm, const MetNoFimex::CDM& cdm);
void writeAttribute(fimex_index::CDMAttribute* fatt, StringListBuilder& cdm_strings, const CDMAttribute& att);
void writeData(fimex_index::CDMData* fdata, const MetNoFimex::CDMDataType dt, StringListBuilder& cdm_strings, MetNoFimex::DataPtr data);

} // namespace ProtobufCDM
} // namespace MetNoFimex

#endif /* FIMEX_PROTOBUFCDM_H_ */
