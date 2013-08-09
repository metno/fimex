/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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

#ifndef CDMDATATYPE_H_
#define CDMDATATYPE_H_
#include <string>

namespace MetNoFimex
{

/**
 * @headerfile fimex/CDMDataType.h
 */
/** Be aware that the CDM_CHAR datatype maps to NC_BYTE, while the CDM_SHORT maps to NC_CHAR */
enum CDMDataType {
    CDM_NAT = 0,
    CDM_CHAR,
    CDM_SHORT,
    CDM_INT,
    CDM_FLOAT,
    CDM_DOUBLE,
    CDM_STRING,
    CDM_UCHAR,
    CDM_USHORT,
    CDM_UINT,
    CDM_INT64,
    CDM_UINT64
};

/// translate float/string/... to the appropriate CDMDataType
CDMDataType string2datatype(const std::string& s);
std::string datatype2string(CDMDataType type);

}
#endif /*CDMDATATYPE_H_*/
