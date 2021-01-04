/*
 * Fimex
 *
 * (C) Copyright 2021, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
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

#ifndef NCML_UTILS_H_
#define NCML_UTILS_H_

#include "fimex/CDMDataType.h"

namespace MetNoFimex {

struct NcmlDataType
{
    const std::string name;
    bool is_unsigned;
    NcmlDataType(const std::string& n, bool u)
        : name(n)
        , is_unsigned(u)
    {
    }
    bool operator==(const NcmlDataType& other) const { return name == other.name && is_unsigned == other.is_unsigned; }
    bool operator!=(const NcmlDataType& other) const { return !(*this == other); }
    operator bool() const;
};

extern const NcmlDataType ncml_BYTE;
extern const NcmlDataType ncml_SHORT;
extern const NcmlDataType ncml_INT;
extern const NcmlDataType ncml_LONG;
extern const NcmlDataType ncml_UBYTE;
extern const NcmlDataType ncml_USHORT;
extern const NcmlDataType ncml_UINT;
extern const NcmlDataType ncml_ULONG;
extern const NcmlDataType ncml_CHAR;
extern const NcmlDataType ncml_FLOAT;
extern const NcmlDataType ncml_DOUBLE;
extern const NcmlDataType ncml_STRING;
extern const NcmlDataType ncml_STRING_CAP;

extern const NcmlDataType ncml_NAT;

const NcmlDataType& datatype_cdm2ncml(CDMDataType dt);

CDMDataType datatype_ncml2cdm(const NcmlDataType& dt);
CDMDataType datatype_ncml2cdm(const std::string& name, bool is_unsigned);

extern const std::string NCML_UNSIGNED;

} // namespace MetNoFimex

#endif /*NCML_UTILS_H_*/
