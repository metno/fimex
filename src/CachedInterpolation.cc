/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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

#include "fimex/CachedInterpolation.h"
#include "fimex/Data.h"
#include "fimex/CDMReader.h"
#include "fimex/CDM.h"
#include "fimex/SliceBuilder.h"
#include "fimex/Utils.h"

#include "fimex/Logger.h"

#include <boost/scoped_array.hpp>
#ifdef _OPENMP
#include <omp.h>
#endif


namespace MetNoFimex
{

static Logger_p logger = getLogger("fimex.CachedInterpolation");

DataPtr CachedInterpolationInterface::getInputDataSlice(CDMReader_p reader, const std::string& varName, size_t unLimDimPos) const
{
    DataPtr data;
    if (reducedDomain().get() != 0) {
        // fetch a reduced domain from the input-source
        SliceBuilder sb(reader->getCDM(), varName);
        const CDMVariable& var = reader->getCDM().getVariable(varName);
        sb.setStartAndSize(reducedDomain()->xDim, reducedDomain()->xMin, getInX());
        sb.setStartAndSize(reducedDomain()->yDim, reducedDomain()->yMin, getInY());
        if (reader->getCDM().hasUnlimitedDim(var)) {
            sb.setStartAndSize(reader->getCDM().getUnlimitedDim()->getName(), unLimDimPos, 1);
        }
        std::vector<std::string> unsetVars = sb.getUnsetDimensionNames();
        for (size_t i = 0; i < unsetVars.size(); i++) {
            sb.setAll(unsetVars.at(i));
        }
        data = reader->getDataSlice(varName, sb);
    } else {
        data = reader->getDataSlice(varName, unLimDimPos);
    }
    return data;
}

DataPtr CachedInterpolationInterface::getInputDataSlice(CDMReader_p reader, const std::string& varName, const SliceBuilder& sb) const
{
    DataPtr data;
    LOG4FIMEX(logger, Logger::DEBUG, "creating a slicebuilder for '"<< varName << "'" );
    SliceBuilder rsb(reader->getCDM(), varName);
    if (reducedDomain().get() != 0) {
        // fetch a reduced domain from the input-source
        LOG4FIMEX(logger, Logger::DEBUG, "reduced xDim='"<< reducedDomain()->xDim << "' yDim='" << reducedDomain()->yDim << "'" );
        rsb.setStartAndSize(reducedDomain()->xDim, reducedDomain()->xMin, getInX());
        rsb.setStartAndSize(reducedDomain()->yDim, reducedDomain()->yMin, getInY());
    } else {
        LOG4FIMEX(logger, Logger::DEBUG, "no reduced, _xDimName='"<< _xDimName << "' _yDimName='" << _yDimName << "'" );
        rsb.setAll(_xDimName);
        rsb.setAll(_yDimName);
    }
    std::vector<std::string> unsetVars = rsb.getUnsetDimensionNames();
    for (size_t i = 0; i < unsetVars.size(); i++) {
        size_t start, size;
        sb.getStartAndSize(unsetVars.at(i), start, size);
        rsb.setStartAndSize(unsetVars.at(i), start, size);
    }
    data = reader->getDataSlice(varName, rsb);
    return data;
}


CachedInterpolation::CachedInterpolation(const std::string& xDimName, const std::string& yDimName, int funcType,
                                         const std::vector<double>& pointsOnXAxis, const std::vector<double>& pointsOnYAxis,
                                         size_t inX, size_t inY, size_t outX, size_t outY)
  : CachedInterpolationInterface(xDimName, yDimName)
  , pointsOnXAxis(pointsOnXAxis)
  , pointsOnYAxis(pointsOnYAxis)
  , inX(inX)
  , inY(inY)
  , outX(outX)
  , outY(outY)
{
    // we do not round pointsOnXYAxis values here:
    // * mifi_get_values_bilinear_f and mifi_get_values_bicubic_f use floor/fraction
    // * mifi_get_values_f uses lround

    switch (funcType) {
    case MIFI_INTERPOL_BILINEAR: this->func = mifi_get_values_bilinear_f; break;
    case MIFI_INTERPOL_BICUBIC:  this->func = mifi_get_values_bicubic_f; break;
    case MIFI_INTERPOL_NEAREST_NEIGHBOR:
    case MIFI_INTERPOL_COORD_NN:
    case MIFI_INTERPOL_COORD_NN_KD: this->func = mifi_get_values_f; break;
    default: throw CDMException("unknown interpolation function: " + type2string(funcType));
    }
}

boost::shared_array<float> CachedInterpolation::interpolateValues(boost::shared_array<float> inData, size_t size, size_t& newSize) const
{
    const size_t outLayerSize = outX * outY;
    const size_t inZ = size / (inX*inY);
    newSize = outLayerSize*inZ;
    boost::shared_array<float> outfield(new float[newSize]);

#ifdef _OPENMP
#pragma omp parallel default(shared)
    {
#endif
    boost::scoped_array<float> zValues(new float[inZ]);
#ifdef _OPENMP
#pragma omp for
#endif
    for (size_t xy = 0; xy < outLayerSize; ++xy) {
        float* outPos = &outfield[xy];
        if (func(inData.get(), zValues.get(), pointsOnXAxis[xy], pointsOnYAxis[xy], inX, inY, inZ) != MIFI_ERROR) {
            for (size_t z = 0; z < inZ; ++z) {
                *outPos = zValues[z];
                outPos += outLayerSize;
            }
        } else (throw CDMException("error during interpolation"));
    }
#ifdef _OPENMP
    }
#endif

    return outfield;
}

inline long long clamp(long long low, double dvalue, long long high)
{
    const long long value = static_cast<long long>(dvalue);
    if (value < low)
        return low;
    if (value < high)
        return value;
    return high;
}

void CachedInterpolation::createReducedDomain(std::string xDimName, std::string yDimName)
{
    // don't set twice
    if (reducedDomain().get() != 0)
        return;

    const double pMinX = *std::min_element(pointsOnXAxis.begin(), pointsOnXAxis.end());
    const double pMinY = *std::min_element(pointsOnYAxis.begin(), pointsOnYAxis.end());
    const double pMaxX = *std::max_element(pointsOnXAxis.begin(), pointsOnXAxis.end());
    const double pMaxY = *std::max_element(pointsOnYAxis.begin(), pointsOnYAxis.end());

    // allow additional cells for interpolation (2 for bicubic)
    const long long EXTEND = 2;
    const long long minX = clamp(0, std::floor(pMinX)-EXTEND, inX-1);
    const long long minY = clamp(0, std::floor(pMinY)-EXTEND, inY-1);
    const long long maxX = clamp(0, std::ceil(pMaxX) +EXTEND, inX-1);
    const long long maxY = clamp(0, std::ceil(pMaxY) +EXTEND, inY-1);

    // make sure we still have a useful size
    if ((maxX - minX) < 1 || (maxY - minY) < 1)
        return;

    // update the axes
    for (size_t xy = 0; xy < outX * outY; ++xy) {
        pointsOnXAxis[xy] -= minX;
        pointsOnYAxis[xy] -= minY;
    }

    // create the reduced interpolation
    std::shared_ptr<ReducedInterpolationDomain> rid(new ReducedInterpolationDomain());
    rid->xDim = xDimName;
    rid->yDim = yDimName;
    rid->xMin = minX;
    rid->yMin = minY;
    rid->xOrg = inX;
    rid->yOrg = inY;
    reducedDomain_ = rid;

    // reduce the expected input size
    inX = maxX-minX+1;
    inY = maxY-minY+1;
}


}
