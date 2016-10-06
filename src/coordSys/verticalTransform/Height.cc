/*
 * Fimex, Height.cc
 *
 * (C) Copyright 2013-2015, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 *  Created on: Aug 8, 2013
 *      Author: heikok
 */

#include "fimex/coordSys/verticalTransform/Height.h"

#include "fimex/coordSys/verticalTransform/AltitudeHeightConverter.h"
#include "fimex/coordSys/verticalTransform/IdentityConverter.h"

namespace MetNoFimex {

VerticalConverterPtr Height::getPressureConverter(CDMReaderPtr reader, CoordSysPtr cs) const
{
//    DataPtr h = reader->getScaledDataSliceInUnit(height, "m", unLimDimPos);
//    boost::shared_array<double> ha = h->asDouble();
//    return boost::shared_ptr<ToVLevelConverter>(new HeightStandardToPressureConverter(vector<double> (&ha[0],&ha[0] + h->size())));
    // does not exist generally without known topography, which depends the variable to read
    return VerticalConverterPtr();
}

VerticalConverterPtr Height::getHeightConverter(CDMReaderPtr reader, CoordSysPtr cs) const
{
    return IdentityConverter::createConverterForVarName(reader, cs, height, "m");
}

VerticalConverterPtr Height::getAltitudeConverter(CDMReaderPtr reader, CoordSysPtr cs) const
{
    if (VerticalConverterPtr height = getHeightConverter(reader, cs))
        return AltitudeHeightConverter::createConverter(reader, cs, height, false);

    return VerticalConverterPtr();
}

} // namespace MetNoFimex
