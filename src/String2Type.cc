/*
  Fimex, src/String2Type.cc

  Copyright (C) 2019-2026 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://github.com/metno/fimex/wiki

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/


#include "fimex/String2Type.h"

#include "fimex/StringUtils.h"

namespace MetNoFimex {

//! recognize on/true/1 as true, off,0,false as false
template <>
bool string2type<bool>(const std::string& s)
{
    if (s == "0")
        return false;
    else if (s == "1")
        return true;
    const std::string l = string2lowerCase(s);
    if (l == "on" || l == "true")
        return true;
    else if (l == "off" || l == "false")
        return false;

    throw string2type_error(s, "bool", " (accepting on,true,1 as true and off,false,0 as false)");
}

} // namespace MetNoFimex
