/*
 * Fimex, VerticalTransformation.cc
 *
 * (C) Copyright 2013, met.no
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
 *  Created on: Aug 7, 2013
 *      Author: heikok
 */

#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"
#include "fimex/CDMException.h"
#include "fimex/Logger.h"
#include "fimex/CDMReader.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Utils.h"
#include "fimex/coordSys/verticalTransform/AltitudeHeightConverter.h"
#include "fimex/coordSys/verticalTransform/IdentityConverter.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverterAdapter.h"
#include "fimex/coordSys/verticalTransform/VerticalConverter.h"
#include "coordSys/CoordSysUtils.h"

#include <iostream>
#include <map>
#include <memory>

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.VerticalTransformation");

std::ostream& operator<<(std::ostream& out, const MetNoFimex::VerticalTransformation& vt)
{
    out << vt.getName() << "(" << vt.getParameterString() << ")";
    return out;
}

ToVLevelConverter_p VerticalTransformation::getConverter(CDMReader_p reader, int verticalType, size_t unLimDimPos, CoordinateSystem_cp cs, size_t /*nx*/,
                                                         size_t /*ny*/, size_t /*nz*/, size_t /*nt*/) const
{
    return getConverter(reader, verticalType, unLimDimPos, cs);
}

ToVLevelConverter_p VerticalTransformation::getConverter(CDMReader_p reader, int verticalType, size_t unLimDimPos, CoordinateSystem_cp cs) const
{
    if (VerticalConverter_p c = getConverter(reader, cs, verticalType))
        return std::make_shared<ToVLevelConverterAdapter>(reader, cs, c, unLimDimPos);
    else
        return ToVLevelConverter_p();
}

VerticalConverter_p VerticalTransformation::getConverter(CDMReader_p reader, CoordinateSystem_cp cs, int verticalType) const
{
    LOG4FIMEX(logger, Logger::DEBUG, "getConverter vt=" << verticalType);
    switch (verticalType) {
    case MIFI_VINT_PRESSURE: return findPressureConverter(reader, cs);
    case MIFI_VINT_HEIGHT: return  getHeightConverter(reader, cs);
    case MIFI_VINT_ALTITUDE: return getAltitudeConverter(reader, cs);
    case MIFI_VINT_DEPTH: return getDepthConverter(reader, cs);
    default: throw CDMException("unknown vertical type");
    }
}

VerticalConverter_p VerticalTransformation::getAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    // try geopotential_height or fall back to pressure

    if (VerticalConverter_p c = IdentityConverter::createConverterForStandardName(reader, cs, "geopotential_height", "m"))
        return c;

    VerticalConverter_p pressure = findPressureConverter(reader, cs);
    if (!pressure)
        return VerticalConverter_p();

    if (VerticalConverter_p c = PressureIntegrationToAltitudeConverter::createConverter(reader, cs, pressure))
        return c;

    LOG4FIMEX(logger, Logger::WARN, "unable to use anything better than standard atmosphere to derive altitude from pressure");
    return PressureToStandardAltitudeConverter::createConverter(reader, cs, pressure);
}

VerticalConverter_p VerticalTransformation::getHeightConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    if (VerticalConverter_p altitude = getAltitudeConverter(reader, cs))
        return AltitudeHeightConverter::createToHeightConverter(reader, cs, altitude);

    return VerticalConverter_p(); // no converter
}

VerticalConverter_p VerticalTransformation::getDepthConverter(CDMReader_p, CoordinateSystem_cp) const
{
    return VerticalConverter_p(); // no converter
}

VerticalConverter_p VerticalTransformation::getIdentityPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    return IdentityConverter::createConverterForStandardName(reader, cs, "(air_)?pressure", "hPa");
}

VerticalConverter_p VerticalTransformation::findPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    if (VerticalConverter_p c = getIdentityPressureConverter(reader, cs))
        return c;
    return getPressureConverter(reader, cs);
}

} // namespace MetNoFimex
