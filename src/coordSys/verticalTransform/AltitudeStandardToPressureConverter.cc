/*
  Fimex, src/coordSys/verticalTransform/AltitudeStandardToPressureConverter.cc

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

#include "fimex/coordSys/verticalTransform/AltitudeStandardToPressureConverter.h"

#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/vertical_coordinate_transformations.h"

#include <memory>

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.AltitudeStandardToPressureConverter");

// static method
VerticalConverter_p AltitudeStandardToPressureConverter::createConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p altitude)
{
    LOG4FIMEX(logger, Logger::INFO, "using altitude and standard atmosphere to estimate pressure levels");
    return std::make_shared<AltitudeStandardToPressureConverter>(reader, cs, altitude);
}

AltitudeStandardToPressureConverter::AltitudeStandardToPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs, const VerticalConverter_p& altitude)
    : BasicVerticalConverter(reader, cs)
    , altitude_(altitude)
{
}

std::vector<std::string> AltitudeStandardToPressureConverter::getShape() const
{
    return altitude_->getShape();
}

DataPtr AltitudeStandardToPressureConverter::getDataSlice(const SliceBuilder &sb) const
{
    DataPtr altitudeData = altitude_->getDataSlice(sb);
    shared_array<double> aVal = altitudeData->asDouble();
    const size_t size = altitudeData->size();
    shared_array<double> pVal(new double[size]);
    mifi_barometric_standard_pressure(size, aVal.get(), pVal.get());
    return createData(size, pVal);
}

} // namespace MetNoFimex
