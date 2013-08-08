/*
 * Fimex, Height.cc
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

#include "fimex/coordSys/verticalTransform/Height.h"
#include "fimex/CDMReader.h"
#include "fimex/Data.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"


namespace MetNoFimex {

boost::shared_ptr<ToVLevelConverter> Height::getPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nt) const
{
    DataPtr h = reader->getScaledDataSliceInUnit(height, "m", unLimDimPos);
    boost::shared_array<double> ha = h->asDouble();
    return boost::shared_ptr<ToVLevelConverter>(new HeightStandardToPressureConverter(vector<double> (&ha[0],&ha[0] + h->size())));
}

boost::shared_ptr<ToVLevelConverter> Height::getHeightConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const
{
    DataPtr hd = reader->getScaledDataSliceInUnit(height, "m", unLimDimPos);
    const boost::shared_array<double> ha = hd->asDouble();
    return boost::shared_ptr<ToVLevelConverter>(new IdentityToVLevelConverter(vector<double> (&ha[0], &ha[0] + hd->size())));
}


}
