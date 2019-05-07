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

VerticalConverter_p Height::getPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    // does not exist generally without known topography, which depends the variable to read
    return VerticalConverter_p();
}

VerticalConverter_p Height::getHeightConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    return IdentityConverter::createConverterForVarName(reader, cs, height, "m");
}

VerticalConverter_p Height::getAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    if (VerticalConverter_p height = getHeightConverter(reader, cs))
        return AltitudeHeightConverter::createToAltitudeConverter(reader, cs, height);

    return VerticalConverter_p();
}

} // namespace MetNoFimex
