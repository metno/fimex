/*
 * Fimex, CoordSysUtils.cc
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
 *      Author: Heiko Klein
 */

#include "CoordSysUtils.h"

#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateAxis.h"
#include "fimex/coordSys/CoordinateSystem.h"

#include "fimex/CDM.h"
#include "fimex/CDMException.h"

namespace MetNoFimex {

using namespace std;

void getSimpleAxes(const CoordinateSystem_cp& cs, const CDM& cdm, CoordinateAxis_cp& xAxis, CoordinateAxis_cp& yAxis, CoordinateAxis_cp& zAxis,
                   CoordinateAxis_cp& tAxis, size_t& nx, size_t& ny, size_t& nz, size_t& nt, bool& tIsUnlimited)
{
    zAxis = cs->getGeoZAxis();
    assert(zAxis.get() != 0); // defined by construction of cs
    nz = 1;
    {
        const vector<string>& shape = zAxis->getShape();
        if (shape.size() == 1) {
            nz = cdm.getDimension(shape.at(0)).getLength();
        } else {
            throw CDMException("vertical interpolation not possible with 2d z-Axis: "+zAxis->getName());
        }
    }

    // detect x and y axis
    xAxis = cs->getGeoXAxis();
    nx = 1;
    if (xAxis.get() != 0) {
        const vector<string>& shape = xAxis->getShape();
        if (shape.size() == 1) {
            nx = cdm.getDimension(shape.at(0)).getLength();
        } else {
            throw CDMException("vertical interpolation not possible with 2d x-Axis: "+xAxis->getName());
        }
    }

    yAxis = cs->getGeoYAxis();
    ny = 1;
    if (yAxis.get() != 0) {
        const vector<string>& shape = yAxis->getShape();
        if (shape.size() == 1) {
            ny = cdm.getDimension(shape.at(0)).getLength();
        } else {
            throw CDMException("vertical interpolation not possible with 2d y-Axis: "+yAxis->getName());
        }
    }

    // detect time axis
    tIsUnlimited = false;
    tAxis = cs->getTimeAxis();
    nt = 1;
    if (tAxis.get() != 0) {
        const vector<string>& shape = tAxis->getShape();
        if (shape.size() == 1) {
            const CDMDimension& tDim = cdm.getDimension(shape.at(0));
            nt = tDim.getLength();
            tIsUnlimited = tDim.isUnlimited();
        } else {
            throw CDMException(
                    "vertical interpolation not possible with 2d time-axis");
        }
    }
}

void getSimpleAxes(const CoordinateSystem_cp& cs, const CDM& cdm, CoordinateAxis_cp& xAxis, CoordinateAxis_cp& yAxis, CoordinateAxis_cp& zAxis,
                   CoordinateAxis_cp& tAxis, size_t& nx, size_t& ny, size_t& nz, size_t& t0, size_t& t1, size_t unLimDimPos)
{
    bool tIsUnlimited;
    getSimpleAxes(cs, cdm,
            xAxis, yAxis, zAxis, tAxis,
            nx, ny, nz, t1, tIsUnlimited);
    if (tIsUnlimited) {
        t1 = unLimDimPos + 1;
        t0 = unLimDimPos;
    } else {
        t0 = 0;
    }
}

} // namespace MetNoFimex
