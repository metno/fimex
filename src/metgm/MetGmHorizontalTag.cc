/*
 * Fimex
 *
 * (C) Copyright 2011-2019, met.no
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
 */

// internals
//
#include "MetGmHorizontalTag.h"
#include "MetGmGroup3Ptr.h"

// fimex
//
#include "fimex/coordSys/CoordinateSystem.h"

namespace MetNoFimex {

std::shared_ptr<MetGmXTag> MetGmHorizontalTag::createMetGmXTagForWriting(const CDMReader_p pCdmReader, const CDMVariable* pVariable)
{
    if (!pVariable)
        throw CDMException("pVar is null");

    if (!pCdmReader)
        throw CDMException("pCdmReader is null");

    std::shared_ptr<MetGmXTag> XTag;

    CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(listCoordinateSystems(pCdmReader), pVariable->getName());
    if (cs && cs->isSimpleSpatialGridded() && cs->getGeoXAxis()) {
        XTag = std::shared_ptr<MetGmXTag>(new MetGmXTag);

        DataPtr data = pCdmReader->getScaledDataInUnit(cs->getGeoXAxis()->getName(), "degree");
        XTag->numberOfPoints_ = data->size();
        XTag->extractHorizontalPoints(data);

        XTag->center_ = (XTag->horizontalPoints_.back() + XTag->horizontalPoints_.front()) / 2.0;
        XTag->distance_ = XTag->horizontalPoints_.at(1) - XTag->horizontalPoints_.front();
        if (XTag->distance_ < 0)
            throw CDMException("metgm is not supporting negative distances on longitude axis");
    }

    return XTag;
}

std::shared_ptr<MetGmYTag> MetGmHorizontalTag::createMetGmYTagForWriting(const CDMReader_p pCdmReader, const CDMVariable* pVariable)
{
    if (!pVariable)
        throw CDMException("pVar is null");

    if (!pCdmReader)
        throw CDMException("pCdmReader is null");

    std::shared_ptr<MetGmYTag> YTag;

    CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(listCoordinateSystems(pCdmReader), pVariable->getName());
    if (cs && cs->isSimpleSpatialGridded() && cs->getGeoYAxis()) {
        YTag = std::shared_ptr<MetGmYTag>(new MetGmYTag);

        DataPtr data = pCdmReader->getScaledDataInUnit(cs->getGeoYAxis()->getName(), "degree");
        YTag->numberOfPoints_ = data->size();
        YTag->extractHorizontalPoints(data);

        YTag->center_ = (YTag->horizontalPoints_.back() + YTag->horizontalPoints_.front()) / 2.0;
        YTag->distance_ = YTag->horizontalPoints_.at(1) - YTag->horizontalPoints_.front();
        if (YTag->distance_ < 0)
            throw CDMException("metgm is not supporting negative distances on latitude axis");
    }

    return YTag;
}

std::shared_ptr<MetGmXTag> MetGmHorizontalTag::createMetGmXTagForReading(const std::shared_ptr<MetGmGroup3Ptr> pg3)
{
    std::shared_ptr<MetGmXTag> XTag(new MetGmXTag);

    XTag->center_ = pg3->cx();
    XTag->distance_ = pg3->dx();
    XTag->numberOfPoints_ = pg3->nx();

    double x0 = XTag->cx() - (XTag->nx() - 1) * XTag->dx() / 2.0;

    for (size_t index = 0; index < XTag->nx(); ++index) {
        XTag->horizontalPoints_.push_back(x0 + index * XTag->dx());
    }

    return XTag;
}

std::shared_ptr<MetGmYTag> MetGmHorizontalTag::createMetGmYTagForReading(const std::shared_ptr<MetGmGroup3Ptr> pg3)
{
    std::shared_ptr<MetGmYTag> YTag(new MetGmYTag);

    YTag->center_ = pg3->cy();
    YTag->distance_ = pg3->dy();
    YTag->numberOfPoints_ = pg3->ny();

    double y0 = YTag->cy() - (YTag->ny() - 1) * YTag->dy() / 2.0;

    for (size_t index = 0; index < YTag->ny(); ++index) {
        YTag->horizontalPoints_.push_back(y0 + index * YTag->dy());
    }

    return YTag;
}

} // namespace MetNoFimex
