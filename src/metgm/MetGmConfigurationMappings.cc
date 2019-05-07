/*
  Fimex, src/metgm/MetGmConfigurationMappings.cc

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


#include "MetGmConfigurationMappings.h"

#include <algorithm>

namespace MetNoFimex {

std::vector<xml_configuration::const_iterator> sorted_by_pid(const xml_configuration& xc)
{
    std::vector<xml_configuration::const_iterator> sorted;
    sorted.reserve(xc.size());
    for (xml_configuration::const_iterator pIt = xc.begin(); pIt != xc.end(); ++pIt)
        sorted.push_back(pIt);
    std::sort(sorted.begin(), sorted.end(), MetGmConfigurationMappingsByPId());
    return sorted;
}

} // namespace MetNoFimex
