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

static const std::string HEIGHT = "height";
static const std::string ALTITUDE = "altitude";

Height::Height(const std::string& height, bool is_altitude)
    : height_(height)
    , is_altitude_(is_altitude)
{
}

Height::~Height()
{
}

// static
const std::string Height::NAME()
{
    return HEIGHT;
}

std::string Height::getName() const
{
    return is_altitude_ ? ALTITUDE : HEIGHT;
}

int Height::getPreferredVerticalType() const
{
    return is_altitude_ ? MIFI_VINT_ALTITUDE : MIFI_VINT_HEIGHT;
}

std::string Height::getParameterString() const
{
    return (is_altitude_ ? "a=" : "h=") + height_;
}

VerticalConverter_p Height::getPressureConverter(CDMReader_p /*reader*/, CoordinateSystem_cp /*cs*/) const
{
    return VerticalConverter_p();
}

VerticalConverter_p Height::getHeightConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    VerticalConverter_p c = IdentityConverter::createConverterForVarName(reader, cs, height_, "m");
    if (is_altitude_)
        c = AltitudeHeightConverter::createToHeightConverter(reader, cs, c);
    return c;
}

VerticalConverter_p Height::getAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    VerticalConverter_p c = IdentityConverter::createConverterForVarName(reader, cs, height_, "m");
    if (!is_altitude_)
        c = AltitudeHeightConverter::createToAltitudeConverter(reader, cs, c);
    return c;
}

} // namespace MetNoFimex
