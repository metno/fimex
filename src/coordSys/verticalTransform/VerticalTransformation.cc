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

#include <boost/make_shared.hpp>
#include <iostream>
#include <map>

namespace MetNoFimex {

static LoggerPtr logger = getLogger("fimex.VerticalTransformation");

std::ostream& operator<<(std::ostream& out, const MetNoFimex::VerticalTransformation& vt)
{
    out << vt.getName() << "(" << vt.getParameterString() << ")";
    return out;
}

boost::shared_ptr<ToVLevelConverter> VerticalTransformation::getConverter(CDMReaderPtr reader, int verticalType, size_t unLimDimPos,
        CoordSysPtr cs, size_t /*nx*/, size_t /*ny*/, size_t /*nz*/, size_t /*nt*/) const
{
    return getConverter(reader, verticalType, unLimDimPos, cs);
}

boost::shared_ptr<ToVLevelConverter> VerticalTransformation::getConverter(CDMReaderPtr reader, int verticalType, size_t unLimDimPos,
        CoordSysPtr cs) const
{
    if (VerticalConverterPtr c = getConverter(reader, cs, verticalType))
        return boost::make_shared<ToVLevelConverterAdapter>(reader, cs, c, unLimDimPos);
    else
        return boost::shared_ptr<ToVLevelConverter>();
}

VerticalConverterPtr VerticalTransformation::getConverter(CDMReaderPtr reader, CoordSysPtr cs, int verticalType) const
{
    LOG4FIMEX(logger, Logger::DEBUG, "getConverter vt=" << verticalType);
    switch (verticalType) {
    case MIFI_VINT_PRESSURE: return findPressureConverter(reader, cs);
    case MIFI_VINT_HEIGHT: return  getHeightConverter(reader, cs);
    case MIFI_VINT_ALTITUDE: return getAltitudeConverter(reader, cs);
    case MIFI_VINT_DEPTH: return getAltitudeConverter(reader, cs);
    default: throw CDMException("unknown vertical type");
    }
}

VerticalConverterPtr VerticalTransformation::getAltitudeConverter(CDMReaderPtr reader, CoordSysPtr cs) const
{
    // try geopotential_height or fall back to pressure

    if (VerticalConverterPtr c = IdentityConverter::createConverterForStandardName(reader, cs, "geopotential_height", "m"))
        return c;

    VerticalConverterPtr pressure = findPressureConverter(reader, cs);
    if (!pressure)
        return VerticalConverterPtr();

    if (VerticalConverterPtr c = PressureIntegrationToAltitudeConverter::createConverter(reader, cs, pressure))
        return c;

    return PressureToStandardAltitudeConverter::createConverter(reader, cs, pressure);
}

VerticalConverterPtr VerticalTransformation::getHeightConverter(CDMReaderPtr reader, CoordSysPtr cs) const
{
    if (VerticalConverterPtr altitude = getAltitudeConverter(reader, cs))
        return AltitudeHeightConverter::createConverter(reader, cs, altitude, true);

    return VerticalConverterPtr(); // no converter
}

VerticalConverterPtr VerticalTransformation::getIdentityPressureConverter(CDMReaderPtr reader, CoordSysPtr cs) const
{
    return IdentityConverter::createConverterForStandardName(reader, cs, "(air_)?pressure", "hPa");
}

VerticalConverterPtr VerticalTransformation::findPressureConverter(CDMReaderPtr reader, CoordSysPtr cs) const
{
    if (VerticalConverterPtr c = getIdentityPressureConverter(reader,cs))
        return c;
    return getPressureConverter(reader, cs);
}

} // namespace MetNoFimex
