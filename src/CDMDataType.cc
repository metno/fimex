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
	   default: return "NAT"; // not a type
	}
}


}
