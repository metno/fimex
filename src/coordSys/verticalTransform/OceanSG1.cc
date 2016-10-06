/*
 * Fimex, OceanSG1.cc
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
 *  Created on: Aug 8, 2013
 *      Author: heikok
 */

#include "fimex/coordSys/verticalTransform/OceanSG1.h"
#include "fimex/vertical_coordinate_transformations.h"
#include "fimex/CDMException.h"

#include <boost/make_shared.hpp>

namespace MetNoFimex {

OceanSG1::OceanSG1(const OceanSGVars& vars)
    : heightConversionFunction(mifi_ocean_s_g1_z)
    , vars(vars)
{}

VerticalConverterPtr OceanSG1::getPressureConverter(CDMReaderPtr reader, CoordSysPtr cs) const
{
    throw CDMException("no pressure converter for " + getName());
}

VerticalConverterPtr OceanSG1::getAltitudeConverter(CDMReaderPtr reader, CoordSysPtr cs) const
{
    return boost::make_shared<OceanSCoordinateGToDepthConverter>(reader, cs, vars, heightConversionFunction);
}

} // namespace MetNoFimex
