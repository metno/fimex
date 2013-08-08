/*
 * Fimex, LnPressure.cc
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

#include "fimex/coordSys/verticalTransform/LnPressure.h"
#include "fimex/CDMReader.h"
#include "fimex/Data.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"


namespace MetNoFimex {

boost::shared_ptr<ToVLevelConverter> LnPressure::getPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nt) const
{
    const vector<double> levVec = getDataSliceInUnit(reader, lev, "", unLimDimPos);
    const vector<double> p0Vec = getDataSliceInUnit(reader, p0, "hPa", unLimDimPos);
    return boost::shared_ptr<ToVLevelConverter>(new LnPressureToPressureConverter(p0Vec.at(0), levVec));
}

}



