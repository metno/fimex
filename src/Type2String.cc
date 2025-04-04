/*
  Fimex, src/Type2String.cc

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

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


#include "fimex/Type2String.h"

#include <iomanip>
#include <limits>

namespace MetNoFimex {

template <>
std::ostream& type2stream<double>(std::ostream& out, double in)
{
    std::ostringstream buffer;
    buffer << std::setprecision(std::numeric_limits<double>::digits10 + 1) << in;
    out << buffer.str();
    return out;
}

template <>
std::ostream& type2stream<float>(std::ostream& out, float in)
{
    std::ostringstream buffer;
    buffer << std::setprecision(std::numeric_limits<float>::digits10 + 1) << in;
    out << buffer.str();
    return out;
}

} // namespace MetNoFimex
