/*
  Fimex, src/coordSys/verticalTransform/PressureToStandardAltitudeConverter.cc

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


#include "fimex/coordSys/verticalTransform/PressureToStandardAltitudeConverter.h"

#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/vertical_coordinate_transformations.h"

#include <memory>

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.PressureToStandardAltitudeConverter");

// static method
VerticalConverter_p PressureToStandardAltitudeConverter::createConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p pressure)
{
    LOG4FIMEX(logger, Logger::INFO, "using pressure and standard atmosphere to estimate altitude levels");
    return std::make_shared<PressureToStandardAltitudeConverter>(reader, cs, pressure);
}

PressureToStandardAltitudeConverter::PressureToStandardAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p pressure)
    : BasicVerticalConverter(reader, cs)
    , pressure_(pressure)
{
}

std::vector<std::string> PressureToStandardAltitudeConverter::getShape() const
{
    return pressure_->getShape();
}

DataPtr PressureToStandardAltitudeConverter::getDataSlice(const SliceBuilder& sb) const
{
    DataPtr pressureData = pressure_->getDataSlice(sb);
    auto pVal = pressureData->asDouble();
    const size_t size = pressureData->size();
    auto altiVal = make_shared_array<double>(size);
    mifi_barometric_standard_altitude(size, pVal.get(), altiVal.get());
    return createData(size, altiVal);
}

} // namespace MetNoFimex
