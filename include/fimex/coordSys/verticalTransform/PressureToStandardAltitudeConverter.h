/*
  Fimex, include/fimex/coordSys/verticalTransform/PressureToStandardAltitudeConverter.h

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

#ifndef PRESSURETOSTANDARDALTITUDECONVERTER_H
#define PRESSURETOSTANDARDALTITUDECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Conversion from pressure to height above MSL (i.e. altitude) using the international standard atmosphere.
 * The pressure levels are initialized by a previous pressure-conversion.
 */
class PressureToStandardAltitudeConverter : public BasicVerticalConverter {
public:
    static VerticalConverter_p createConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p pressure);

    /**
     * @param pressure another ToVLevelConverter converting to pressure
     */
    PressureToStandardAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p pressure);

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    VerticalConverter_p pressure_;
};

} // namespace MetNoFimex

#endif // PRESSURETOSTANDARDALTITUDECONVERTER_H
