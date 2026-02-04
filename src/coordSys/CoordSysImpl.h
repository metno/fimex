/*
 * Fimex, CoordSysImpl.h
 *
 * (C) Copyright 2009-2026, met.no
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
 *  Created on: Mar 18, 2010
 *      Author: Heiko Klein
 */

#ifndef COORDSYSIMPL_H_
#define COORDSYSIMPL_H_

#include <vector>
#include <set>
#include <string>
#include "fimex/coordSys/CoordinateAxis.h"
#include "fimex/coordSys/Projection.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"

namespace MetNoFimex {

struct CoordSysImpl {
    std::string conventionName_;
    bool isSimpleSpatialGridded_;
    std::set<std::string> isCSForVars_;
    std::set<std::string> isCompleteVars_;
    CoordinateAxis_cp_v axes_;
    CoordinateAxis_cp_v auxiliaryAxes_;
    std::set<std::string> dependencyVars_;
    Projection_cp proj_;
    VerticalTransformation_cp vtran_;
};

}
#endif /* COORDSYSIMPL_H_ */
