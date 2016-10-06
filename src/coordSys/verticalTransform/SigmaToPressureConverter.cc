/*
 * Fimex, SigmaToPressureConverter.cc
 *
 * (C) Copyright 2016, met.no
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
 *  Created on: Jul 13, 2016
 *      Author: Alexander Bürger
 */

#include "fimex/coordSys/verticalTransform/SigmaToPressureConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/Data.h"
#include "fimex/vertical_coordinate_transformations.h"

namespace MetNoFimex {

using std::vector;

std::vector<std::string> SigmaToPressureConverter::getShape() const
{
    // combine shapes of a, b (length = height) and ps (other dimensions)
    std::vector<std::string> shape = reader_->getCDM().getVariable(ps_).getShape();
    shape.insert(shape.begin() + 2, cs_->getGeoZAxis()->getName()); // FIXME this is a crazy hack!
    return shape;
}

DataPtr SigmaToPressureConverter::getDataSlice(const SliceBuilder& sb) const
{
    DataPtr psData = getSliceData(reader_, sb, ps_, "hPa");

    const size_t unLimDimPos = 0;
    const vector<double> sigma = getDataSliceInUnit(reader_, sigma_, "", unLimDimPos);
    const double ptop = getDataSliceInUnit(reader_, ptop_, "hPa", unLimDimPos).front();

    const size_t nx = getSliceSize(sb, cs_->getGeoXAxis()->getName());
    const size_t ny = getSliceSize(sb, cs_->getGeoYAxis()->getName());
    const size_t nl = getSliceSize(sb, cs_->getGeoZAxis()->getName());
    const size_t sizeXY = nx*ny, sizeXYZ = sizeXY*nl;
    const size_t size = psData->size() * nl, sizeOther = size / sizeXYZ;

    boost::shared_array<double> pVal(new double[size]);

    // loop over all dims but z, calculate pressure
    // FIXME the following code assumes that x,y,z are the lowest dimensions for all variables
    size_t offset = 0, offsetSurface = 0;
    for (size_t i=0; i<sizeOther; ++i, offset += sizeXYZ, offsetSurface += sizeXY) {
        for (size_t xy=0; xy < sizeXY; ++xy) {
            const size_t idxSurface = offsetSurface + xy;
            for (size_t l = 0; l < nl; l += 1) {
                const size_t idx = offset + xy + l*sizeXY;
                mifi_atmosphere_sigma_pressure(1, ptop, ps_[idxSurface], &sigma[l], &pVal[idx]);
            }
        }
    }
    return createData(size, pVal);
}

} // namespace MetNoFimex
