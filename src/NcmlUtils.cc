/*
 * Fimex
 *
 * (C) Copyright 2021-2026, met.no
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

#include "NcmlUtils.h"

#include "fimex/Logger.h"

#include <algorithm>

namespace MetNoFimex {

namespace {
Logger_p logger = getLogger("fimex.NcmlCDMReader");
}

const std::string NCML_UNSIGNED = "_Unsigned";

const NcmlDataType ncml_BYTE = {"byte", false};
const NcmlDataType ncml_SHORT = {"short", false};
const NcmlDataType ncml_INT = {"int", false};
const NcmlDataType ncml_LONG = {"long", false};
const NcmlDataType ncml_UBYTE = {"byte", true};
const NcmlDataType ncml_USHORT = {"short", true};
const NcmlDataType ncml_UINT = {"int", true};
const NcmlDataType ncml_ULONG = {"long", true};
const NcmlDataType ncml_CHAR = {"char", false};
const NcmlDataType ncml_FLOAT = {"float", false};
const NcmlDataType ncml_DOUBLE = {"double", false};
const NcmlDataType ncml_STRING = {"string", false};
const NcmlDataType ncml_STRING_CAP = {"String", false};
const NcmlDataType ncml_extension_STRINGS = {"strings", false}; // fimex extension of ncml for netcdf4 strings

const NcmlDataType ncml_NAT = {std::string(), false};

NcmlDataType::operator bool() const
{
    return !name.empty();
}

const NcmlDataType& datatype_cdm2ncml(CDMDataType dt)
{
    switch (dt) {
    case CDM_CHAR:
        return ncml_BYTE;
    case CDM_SHORT:
        return ncml_SHORT;
    case CDM_INT:
        return ncml_INT;
    case CDM_INT64:
        return ncml_LONG;
    case CDM_UCHAR:
        return ncml_UBYTE;
    case CDM_USHORT:
        return ncml_USHORT;
    case CDM_UINT:
        return ncml_UINT;
    case CDM_UINT64:
        return ncml_ULONG;
    case CDM_FLOAT:
        return ncml_FLOAT;
    case CDM_DOUBLE:
        return ncml_DOUBLE;
    case CDM_STRING:
        return ncml_STRING;
    case CDM_STRINGS:
        return ncml_extension_STRINGS;
    default:
        return ncml_NAT;
    }
}

CDMDataType datatype_ncml2cdm(const NcmlDataType& dt)
{
    if (dt == ncml_BYTE) {
        return CDM_CHAR;
    } else if (dt == ncml_SHORT) {
        return CDM_SHORT;
    } else if (dt == ncml_INT) {
        return CDM_INT;
    } else if (dt == ncml_LONG) {
        return CDM_INT64;
    } else if (dt == ncml_UBYTE) {
        return CDM_UCHAR;
    } else if (dt == ncml_USHORT) {
        return CDM_USHORT;
    } else if (dt == ncml_UINT) {
        return CDM_UINT;
    } else if (dt == ncml_ULONG) {
        return CDM_UINT64;
    } else if (dt == ncml_FLOAT) {
        return CDM_FLOAT;
    } else if (dt == ncml_DOUBLE) {
        return CDM_DOUBLE;
    } else if (dt == ncml_STRING || dt == ncml_STRING_CAP) {
        return CDM_STRING;
    } else if (dt == ncml_extension_STRINGS) {
        return CDM_STRINGS;
    } else {
        return CDM_NAT;
    }
}

CDMDataType datatype_ncml2cdm(const std::string& name, bool is_unsigned)
{
    const NcmlDataType ndt(name, is_unsigned);
    const NcmlDataType* types[]{&ncml_BYTE,  &ncml_SHORT, &ncml_INT,   &ncml_LONG,   &ncml_UBYTE,  &ncml_USHORT,     &ncml_UINT,
                                &ncml_ULONG, &ncml_CHAR,  &ncml_FLOAT, &ncml_DOUBLE, &ncml_STRING, &ncml_STRING_CAP, &ncml_extension_STRINGS};
    const auto it = std::find_if(std::begin(types), std::end(types), [&](const NcmlDataType* f) { return *f == ndt; });
    if (it != std::end(types))
        return datatype_ncml2cdm(**it);
    return CDM_NAT;
}

} // namespace MetNoFimex
