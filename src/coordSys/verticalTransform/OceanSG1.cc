/*
 * Fimex, OceanSG1.cc
 *
 * (C) Copyright 2013-2026, met.no
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
 *
 *  Created on: Aug 8, 2013
 *      Author: heikok
 */

#include "fimex/coordSys/verticalTransform/OceanSG1.h"
#include "fimex/vertical_coordinate_transformations.h"
#include "fimex/CDMException.h"

#include <memory>

namespace MetNoFimex {

OceanSG1::OceanSG1(const OceanSGVars& vars)
    : heightConversionFunction(mifi_ocean_s_g1_z)
    , vars(vars)
{}

VerticalConverter_p OceanSG1::getPressureConverter(CDMReader_p, CoordinateSystem_cp) const
{
    throw CDMException("no pressure converter for " + getName());
}

VerticalConverter_p OceanSG1::getAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    throw CDMException("no altitude converter for " + getName());
}

VerticalConverter_p OceanSG1::getDepthConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    return std::make_shared<OceanSCoordinateGToDepthConverter>(reader, cs, vars, heightConversionFunction);
}

} // namespace MetNoFimex
