/*
  Fimex, include/fimex/coordSys/verticalTransform/PressureIntegrationToAltitudeConverter.h

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

#ifndef PRESSUREINTEGRATIONTOALTITUDECONVERTER_H
#define PRESSUREINTEGRATIONTOALTITUDECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Conversion from pressure to height above MSL (i.e. altitude) integrating pressure levels using the hypsometric equation.
 * The pressure levels are initialized by a previous pressure-conversion.
 */
class PressureIntegrationToAltitudeConverter : public BasicVerticalConverter {
public:
    static VerticalConverter_p createConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p pressure);

    /**
     * another ToVLevelConverter converting to pressure
     */
    PressureIntegrationToAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p pressure, const std::string& air_temperature,
                                           const std::string& specific_humidity, const std::string& surface_air_pressure,
                                           const std::string& surface_geopotential);

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    VerticalConverter_p pressure_;
    std::string air_temperature_;
    std::string specific_humidity_;
    std::string surface_air_pressure_;
    std::string surface_geopotential_;
};

} // namespace MetNoFimex

#endif // PRESSUREINTEGRATIONTOALTITUDECONVERTER_H
