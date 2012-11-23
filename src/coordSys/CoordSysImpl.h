/*
 * Fimex, CoordSysImpl.h
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Mar 18, 2010
 *      Author: Heiko Klein
 */

#ifndef COORDSYSIMPL_H_
#define COORDSYSIMPL_H_

#include <vector>
#include <string>
#include "fimex/coordSys/CoordinateAxis.h"
#include "fimex/coordSys/Projection.h"

namespace MetNoFimex {

struct CoordSysImpl {
    std::string conventionName_;
    bool isSimpleSpatialGridded_;
    std::vector<std::string> isCSForVec_;
    std::vector<std::string> isCompleteVec_;
    std::vector<boost::shared_ptr<const CoordinateAxis> > axes_;
    std::vector<boost::shared_ptr<const CoordinateAxis> > auxiliaryAxes_;
    boost::shared_ptr<const Projection> proj_;
};

}
#endif /* COORDSYSIMPL_H_ */
