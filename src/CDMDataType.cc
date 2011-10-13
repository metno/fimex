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

#include "fimex/CDMDataType.h"
#include "fimex/Utils.h"

namespace MetNoFimex
{

CDMDataType string2datatype(const std::string& s) {
	std::string str(string2lowerCase(s));
	if (str == "double") { return CDM_DOUBLE; }
	else if (str == "float") { return CDM_FLOAT; }
	else if (str == "int") { return CDM_INT; }
    else if (str == "long") { return CDM_INT; }
	else if (str == "short") { return CDM_SHORT; }
	else if (str == "char") { return CDM_CHAR; }
    else if (str == "int64") { return CDM_INT64; }
    else if (str == "uint64") { return CDM_UINT64; }
    else if (str == "uint") { return CDM_UINT; }
    else if (str == "ushort") { return CDM_USHORT; }
    else if (str == "uchar") { return CDM_UCHAR; }
	else if (str == "string") { return CDM_STRING; }
    else if (str == "String") { return CDM_STRING; }
	else { return CDM_NAT; }
}

std::string datatype2string(CDMDataType type) {
	switch(type) {
	   case CDM_DOUBLE: return "double";
	   case CDM_FLOAT: return "float";
	   case CDM_INT: return "int";
	   case CDM_SHORT: return "short";
	   case CDM_CHAR: return "char";
	   case CDM_STRING: return "String";
       case CDM_UCHAR: return "uchar";
       case CDM_USHORT: return  "ushort";
	   case CDM_UINT: return  "uint";
       case CDM_INT64: return  "int64";
       case CDM_UINT64: return  "uint64";
	   default: return "NAT"; // not a type
	}
}


}
