/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
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

#include "fimex/CDMDataType.h"

#include "fimex/StringUtils.h"

namespace MetNoFimex {

namespace {
const std::string S_DOUBLE  = "double";
const std::string S_FLOAT   = "float";
const std::string S_INT = "int", S_LONG = "long";
const std::string S_SHORT   = "short";
const std::string S_CHAR    = "char";
const std::string S_BYTE    = "byte";
const std::string S_STRING  = "string";
const std::string S_STRING_CAP = "String";
const std::string S_UCHAR   = "uchar";
const std::string S_UBYTE   = "ubyte";
const std::string S_USHORT  = "ushort";
const std::string S_UINT    = "uint";
const std::string S_INT64   = "int64";
const std::string S_UINT64  = "uint64";
const std::string S_STRINGS = "strings";
const std::string S_NAT     = "NAT"; // not a type
} // namespace

CDMDataType string2datatype(const std::string& s)
{
    std::string str(string2lowerCase(s));
    if      (str == S_DOUBLE)  { return CDM_DOUBLE; }
    else if (str == S_FLOAT)   { return CDM_FLOAT; }
    else if (str == S_INT || str == S_LONG)
                               { return CDM_INT; }
    else if (str == S_SHORT)   { return CDM_SHORT; }
    else if (str == S_CHAR || str == S_BYTE)
                               { return CDM_CHAR; }
    else if (str == S_INT64)   { return CDM_INT64; }
    else if (str == S_UINT64)  { return CDM_UINT64; }
    else if (str == S_UINT)    { return CDM_UINT; }
    else if (str == S_USHORT)  { return CDM_USHORT; }
    else if (str == S_UCHAR || str == S_UBYTE)
                               { return CDM_UCHAR; }
    else if (str == S_STRING || str == S_STRING_CAP)
                               { return CDM_STRING; }
    else if (str == S_STRINGS) { return CDM_STRINGS; }
    else { return CDM_NAT; }
}

std::string datatype2string(CDMDataType type)
{
    switch (type) {
    case CDM_DOUBLE:  return S_DOUBLE;
    case CDM_FLOAT:   return S_FLOAT;
    case CDM_INT:     return S_INT;
    case CDM_SHORT:   return S_SHORT;
    case CDM_CHAR:    return S_CHAR;
    case CDM_UCHAR:   return S_UCHAR;
    case CDM_USHORT:  return S_USHORT;
    case CDM_UINT:    return S_UINT;
    case CDM_INT64:   return S_INT64;
    case CDM_UINT64:  return S_UINT64;
    case CDM_STRING:  return S_STRING;
    case CDM_STRINGS: return S_STRINGS;
    case CDM_NAT:
    default:
        return S_NAT; // not a type
    }
}

} // namespace MetNoFimex
