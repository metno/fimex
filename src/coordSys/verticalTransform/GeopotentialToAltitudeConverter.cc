/*
  Fimex, src/coordSys/verticalTransform/GeopotentialToAltitudeConverter.cc

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

#include "fimex/coordSys/verticalTransform/GeopotentialToAltitudeConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"

#include <memory>

namespace MetNoFimex {

// static method
VerticalConverter_p GeopotentialToAltitudeConverter::createConverter(CDMReader_p reader, CoordinateSystem_cp cs)
{
    const std::string geopotential_height = findVariableWithCS(reader->getCDM(), cs, "geopotential_height");
    if (geopotential_height.empty())
        return VerticalConverter_p();
    return std::make_shared<GeopotentialToAltitudeConverter>(reader, cs, geopotential_height);
}

std::vector<std::string> GeopotentialToAltitudeConverter::getShape() const
{
    return reader_->getCDM().getVariable(geopotential_height_).getShape();
}

DataPtr GeopotentialToAltitudeConverter::getDataSlice(const SliceBuilder& sb) const
{
    return reader_->getScaledDataSliceInUnit(geopotential_height_, "m", sb);
}

} // namespace MetNoFimex
