/*
  Fimex, include/fimex/coordSys/verticalTransform/GeopotentialToAltitudeConverter.h

  Copyright (C) 2019 met.no

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

#ifndef GEOPOTENTIALTOALTITUDECONVERTER_H
#define GEOPOTENTIALTOALTITUDECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Use geopotential height (in m) and topography to calculate the
 * height above ground (hg = height - topo)
 */
class GeopotentialToAltitudeConverter : public BasicVerticalConverter {
public:
    static VerticalConverter_p createConverter(CDMReader_p reader, CoordinateSystem_cp cs);

    GeopotentialToAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& geopotential_height)
        : BasicVerticalConverter(reader, cs)
        , geopotential_height_(geopotential_height)
    {
    }

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    const std::string geopotential_height_;
};

} // namespace MetNoFimex

#endif // GEOPOTENTIALTOALTITUDECONVERTER_H
