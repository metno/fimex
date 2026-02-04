/*
  Fimex, include/fimex/coordSys/verticalTransform/AltitudeHeightConverter.h

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

#ifndef ALTITUDEHEIGHTCONVERTER_H
#define ALTITUDEHEIGHTCONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Use altitude (height above MSL) and topography to calculate the
 * height above ground (hg = height - topo)
 */
class AltitudeHeightConverter : public BasicVerticalConverter {
public:
    static VerticalConverter_p createToHeightConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p altitude)
    {
        return createConverter(reader, cs, altitude, false);
    }

    static VerticalConverter_p createToAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p height)
    {
        return createConverter(reader, cs, height, true);
    }

    AltitudeHeightConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p altitudeOrHeight, const std::string& topography,
                            bool addTopography);

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    static VerticalConverter_p createConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p altitudeOrHeight, bool addTopography);

private:
    VerticalConverter_p altitudeOrHeight_;
    std::string topography_;
    std::string topographyUnit_;
    float topographyFactor_;
};

} // namespace MetNoFimex

#endif // ALTITUDEHEIGHTCONVERTER_H
