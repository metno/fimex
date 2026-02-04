/*
 * Fimex, Depth.cc
 *
 * (C) Copyright 2018-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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
 */

#include "fimex/coordSys/verticalTransform/Depth.h"

#include "fimex/coordSys/verticalTransform/IdentityConverter.h"

namespace MetNoFimex {

VerticalConverter_p Depth::getPressureConverter(CDMReader_p, CoordinateSystem_cp) const
{
    return VerticalConverter_p();
}

VerticalConverter_p Depth::getHeightConverter(CDMReader_p, CoordinateSystem_cp) const
{
    return VerticalConverter_p();
}

VerticalConverter_p Depth::getAltitudeConverter(CDMReader_p, CoordinateSystem_cp) const
{
    return VerticalConverter_p();
}

VerticalConverter_p Depth::getDepthConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    return IdentityConverter::createConverterForVarName(reader, cs, depth, "m");
}

} // namespace MetNoFimex
