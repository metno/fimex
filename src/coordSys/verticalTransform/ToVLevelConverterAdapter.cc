/*
  Fimex, src/coordSys/verticalTransform/ToVLevelConverterAdapter.cc

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/

#include "fimex/coordSys/verticalTransform/ToVLevelConverterAdapter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalConverter.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/SliceBuilder.h"

namespace MetNoFimex {

using std::vector;

static Logger_p logger = getLogger("fimex.ToVLevelConverterAdapter");

static const size_t NOTSET = ~0ul;

ToVLevelConverterAdapter::ToVLevelConverterAdapter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p converter, size_t unLimDimPos)
    : reader_(reader)
    , converter_(converter)
    , unlimitedTimePos_(NOTSET)
{
    const std::vector<std::string>& shape = converter_->getShape();
    const std::set<std::string> shapedims(shape.begin(), shape.end());

    if (CoordinateAxis_cp xax = cs->getGeoXAxis()) {
        const std::string& xdim0 = xax->getShape().front();
        if (shapedims.count(xdim0))
            varGeoX_ = xdim0;
    }

    if (CoordinateAxis_cp yax = cs->getGeoYAxis()) {
        const std::string& ydim0 = yax->getShape().front();
        if (shapedims.count(ydim0))
            varGeoY_ = ydim0;
    }

    if (CoordinateAxis_cp tax = cs->getTimeAxis()) {
        const std::string& tdim0 = tax->getShape().front();
        if (shapedims.count(tdim0)) {
            varTime_ = tdim0;

            const CDMDimension& tDim = reader_->getCDM().getDimension(tdim0);
            if (tDim.isUnlimited())
                unlimitedTimePos_ = unLimDimPos;
        }
    }
}

vector<double> ToVLevelConverterAdapter::operator()(size_t x, size_t y, size_t t)
{
    const SliceBuilder sb = prepareSliceBuilder(x, y, t);

    DataPtr data = converter_->getDataSlice(sb);
    shared_array<double> array = data->asDouble();
    return vector<double>(&array[0], &array[0] + data->size());
}

bool ToVLevelConverterAdapter::isValid(double val, size_t x, size_t y, size_t t)
{
    const SliceBuilder sb = prepareSliceBuilder(x, y, t);

    DataPtr dataMax = converter_->getValidityMax(sb);
    DataPtr dataMin = converter_->getValidityMin(sb);
    return ((!dataMax || val <= dataMax->getDouble(0)) && (!dataMin || val >= dataMin->getDouble(0)));
}

SliceBuilder ToVLevelConverterAdapter::prepareSliceBuilder(size_t x, size_t y, size_t t) const
{
    LOG4FIMEX(logger, Logger::DEBUG, "prepareSliceBuilder: about to create slicebuilder");
    SliceBuilder sb = createSliceBuilder(reader_->getCDM(), converter_);

    if (!varGeoX_.empty())
        sb.setStartAndSize(varGeoX_, x, 1);

    if (!varGeoY_.empty())
        sb.setStartAndSize(varGeoY_, y, 1);

    if (!varTime_.empty()) {
        if (unlimitedTimePos_ != NOTSET)
            t = unlimitedTimePos_;
        sb.setStartAndSize(varTime_, t, 1);
    }
    return sb;
}

} // namespace MetNoFimex
