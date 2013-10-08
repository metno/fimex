/*
 * Fimex, CoordSysUtils.h
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
 *  Created on: Sep 26, 2013
 *      Author: Alexander Buerger
 */

#ifndef COORDSYSUTILS_H_
#define COORDSYSUTILS_H_

#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"

namespace MetNoFimex
{
/**
 * Get the axes of a simple (1-dim x,y,z,t) coordinate-system. The t-axis might be omitted or unlimited,
 * the order of the axes must be as written above (x,y order might be reversed).
 *
 * @param cs the coordinate system
 * @param cdm the corresponding data model
 * @param xAxis,yAxis,zAxis,tAxis output axes
 * @param nx,ny,nz,nt output sizes
 * @param tIsUnlimited indicate if t-axis is unlimited axes
 */
void getSimpleAxes(const boost::shared_ptr<const CoordinateSystem>& cs, const CDM& cdm,
        CoordinateSystem::ConstAxisPtr& xAxis,
        CoordinateSystem::ConstAxisPtr& yAxis,
        CoordinateSystem::ConstAxisPtr& zAxis,
        CoordinateSystem::ConstAxisPtr& tAxis,
        size_t& nx, size_t& ny, size_t& nz, size_t& nt,
        bool& tIsUnlimited);

//! simlilar to getSimpleAxes, but sets t0 and t1 if time dimension is unlimited
void getSimpleAxes(const boost::shared_ptr<const CoordinateSystem>& cs, const CDM& cdm,
        CoordinateSystem::ConstAxisPtr& xAxis,
        CoordinateSystem::ConstAxisPtr& yAxis,
        CoordinateSystem::ConstAxisPtr& zAxis,
        CoordinateSystem::ConstAxisPtr& tAxis,
        size_t& nx, size_t& ny, size_t& nz, size_t& nt,
        size_t& t0, size_t unLimDimPos);

} // namespace MetNoFimex

#endif /* COORDSYSUTILS_H_ */
