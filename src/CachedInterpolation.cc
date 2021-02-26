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

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/CDMReader.h"
#include "fimex/Data.h"
#include "fimex/MathUtils.h"
#include "fimex/SliceBuilder.h"
#include "fimex/Type2String.h"
#include "fimex/interpolation.h"
#include "fimex/min_max.h"

#include "fimex/Logger.h"

#ifdef _OPENMP
#include <omp.h>
#endif

namespace MetNoFimex
{

static Logger_p logger = getLogger("fimex.CachedInterpolation");

ReducedInterpolationDomain::ReducedInterpolationDomain(const std::string& xdim, const std::string& ydim, size_t xmin, size_t ymin)
    : xDim(xdim)
    , yDim(ydim)
    , xMin(xmin)
    , yMin(ymin)
{
}

CachedInterpolationInterface::CachedInterpolationInterface(const std::string& xDimName, const std::string& yDimName, size_t inX, size_t inY, size_t outX,
                                                           size_t outY)
    : _xDimName(xDimName)
    , _yDimName(yDimName)
    , inX(inX)
    , inY(inY)
    , outX(outX)
    , outY(outY)
{
}

CachedInterpolationInterface::~CachedInterpolationInterface() {}

DataPtr CachedInterpolationInterface::getInputDataSlice(CDMReader_p reader, const std::string& varName, size_t unLimDimPos) const
{
    DataPtr data;
    if (reducedDomain()) {
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
    LOG4FIMEX(logger, Logger::DEBUG, "creating a slicebuilder for '"<< varName << "'" );
    SliceBuilder rsb(reader->getCDM(), varName);
    for (const std::string& dn : rsb.getDimensionNames()) {
        size_t start, size;
        if (reducedDomain() && dn == reducedDomain()->xDim) {
            start = reducedDomain()->xMin;
            size = getInX();
        } else if (reducedDomain() && dn == reducedDomain()->yDim) {
            start = reducedDomain()->yMin;
            size = getInY();
        } else if (!reducedDomain() && (dn == _xDimName || dn == _yDimName)) {
            continue; // do nothing, all is default
        } else {
            sb.getStartAndSize(dn, start, size);
        }
        rsb.setStartAndSize(dn, start, size);
    }
    return reader->getDataSlice(varName, rsb);
}

DataPtr CachedInterpolationInterface::getOutputDataSlice(DataPtr data, const SliceBuilder& sb) const
{
    // slice the x and y direction of the data
    std::vector<size_t> startPos = sb.getDimensionStartPositions();
    std::vector<size_t> maxDims = sb.getMaxDimensionSizes();
    const std::vector<size_t>& dimSizes = sb.getDimensionSizes();
    const std::vector<std::string> dimNames = sb.getDimensionNames();
    for (size_t i = 0; i < dimSizes.size(); i++) {
        const std::string& dn = dimNames[i];
        const bool is_xy = (reducedDomain() ? (dn == reducedDomain()->xDim || dn == reducedDomain()->yDim) : (dn == _xDimName || dn == _yDimName));
        if (!is_xy) {
            // already sliced in getInputDataSlice
            startPos.at(i) = 0;
            maxDims.at(i) = dimSizes.at(i);
        }
    }
    return data->slice(maxDims, startPos, dimSizes);
}

CachedInterpolationInterface_p createCachedInterpolation(const std::string& xDimName, const std::string& yDimName, int method,
                                                         shared_array<double> pointsOnXAxis, shared_array<double> pointsOnYAxis, size_t inX, size_t inY,
                                                         size_t outX, size_t outY)
{
    switch (method) {
    case MIFI_INTERPOL_BILINEAR:
    case MIFI_INTERPOL_BICUBIC:
        return std::make_shared<CachedInterpolation>(xDimName, yDimName, method, pointsOnXAxis, pointsOnYAxis, inX, inY, outX, outY);
    case MIFI_INTERPOL_NEAREST_NEIGHBOR:
    case MIFI_INTERPOL_COORD_NN:
    case MIFI_INTERPOL_COORD_NN_KD:
        return std::make_shared<CachedNNInterpolation>(xDimName, yDimName, pointsOnXAxis, pointsOnYAxis, inX, inY, outX, outY);
    default:
        throw CDMException("unknown interpolation function: " + type2string(method));
    }
}

CachedInterpolation::CachedInterpolation(const std::string& xDimName, const std::string& yDimName, int funcType, shared_array<double> pointsOnXAxis,
                                         shared_array<double> pointsOnYAxis, size_t inx, size_t iny, size_t outx, size_t outy)
    : CachedInterpolationInterface(xDimName, yDimName, inx, iny, outx, outy)
    , pointsOnXAxis(pointsOnXAxis)
    , pointsOnYAxis(pointsOnYAxis)
{
    // we do not round pointsOnXYAxis values here:
    // * mifi_get_values_bilinear_f and mifi_get_values_bicubic_f use floor/fraction

    switch (funcType) {
    case MIFI_INTERPOL_BILINEAR: this->func = mifi_get_values_bilinear_f; break;
    case MIFI_INTERPOL_BICUBIC:  this->func = mifi_get_values_bicubic_f; break;
#if 0
    case MIFI_INTERPOL_NEAREST_NEIGHBOR:
    case MIFI_INTERPOL_COORD_NN:
    case MIFI_INTERPOL_COORD_NN_KD: this->func = mifi_get_values_f; break;
#endif
    default:
        throw CDMException("CachedInterpolation supports only bilinear and bicubic, not: " + type2string(funcType));
    }

    createReducedDomain(xDimName, yDimName);
}

shared_array<float> CachedInterpolation::interpolateValues(shared_array<float> inData, size_t size, size_t& newSize) const
{
    const size_t outLayerSize = outX * outY;
    const size_t inZ = size / (inX*inY);
    newSize = outLayerSize*inZ;
    shared_array<float> outfield(new float[newSize]);

#ifdef _OPENMP
#pragma omp parallel default(shared)
    {
#endif
        std::unique_ptr<float[]> zValues(new float[inZ]);
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
        } else {
            throw CDMException("error during interpolation");
        }
    }
#ifdef _OPENMP
    }
#endif

    return outfield;
}

namespace {
const long long EXTEND = 2;

// adapt types
inline long long clamp_ex(long long low, double dvalue, long long extend, long long high)
{
    const long long value = (extend > 0 ? std::ceil(dvalue) : std::floor(dvalue)) + extend;
    return clamp(low, value, high);
}
} // namespace

void CachedInterpolation::createReducedDomain(const std::string& xDimName, const std::string& yDimName)
{
    // don't set twice
    if (reducedDomain())
        return;

    const size_t outLayerSize = outX * outY;
    const auto minmaxX = min_max_element(pointsOnXAxis.get(), pointsOnXAxis.get() + outLayerSize);
    const auto minmaxY = min_max_element(pointsOnYAxis.get(), pointsOnYAxis.get() + outLayerSize);

    // allow additional cells for interpolation (2 for bicubic)
    const long long minX = clamp_ex(0, *minmaxX.first, -EXTEND, inX - 1);
    const long long minY = clamp_ex(0, *minmaxY.first, -EXTEND, inY - 1);
    const long long maxX = clamp_ex(0, *minmaxX.second, +EXTEND, inX - 1);
    const long long maxY = clamp_ex(0, *minmaxY.second, +EXTEND, inY - 1);

    // make sure we still have a useful size
    if ((maxX - minX) < 1 || (maxY - minY) < 1)
        return;

    // update the axes
    for (size_t xy = 0; xy < outX * outY; ++xy) {
        pointsOnXAxis[xy] -= minX;
        pointsOnYAxis[xy] -= minY;
    }

    reducedDomain_ = std::make_shared<ReducedInterpolationDomain>(xDimName, yDimName, minX, minY);

    // reduce the expected input size
    inX = maxX-minX+1;
    inY = maxY-minY+1;
}

namespace {

const size_t INVALID = ~0u;

} // namespace

// pointsOnXAxis map each point in inData[y*inX+x] to a x-position in outData
CachedNNInterpolation::CachedNNInterpolation(const std::string& xDimName, const std::string& yDimName, shared_array<double> pointsOnXAxis,
                                             shared_array<double> pointsOnYAxis, size_t inx, size_t iny, size_t outx, size_t outy)
    : CachedInterpolationInterface(xDimName, yDimName, inx, iny, outx, outy)
{
    const size_t outLayerSize = outX * outY;
    pointsInIn = std::vector<size_t>(outLayerSize, INVALID);
    size_t minInX = inX, maxInX = 0, minInY = inY, maxInY = 0;
    const RoundAndClamp roundX(0, inX - 1, INVALID);
    const RoundAndClamp roundY(0, inY - 1, INVALID);
    for (size_t oxy = 0; oxy < outLayerSize; ++oxy) {
        const size_t ix = roundX(pointsOnXAxis[oxy]), iy = roundY(pointsOnYAxis[oxy]);
        if (ix != INVALID && iy != INVALID) {
            pointsInIn[oxy] = iy * inX + ix;
            if (iy > maxInY)
                maxInY = iy;
            if (iy < minInY)
                minInY = iy;
            if (ix > maxInX)
                maxInX = ix;
            if (ix < minInX)
                minInX = ix;
        }
    }
    LOG4FIMEX(logger, Logger::DEBUG, "ranges: x=" << minInX << "..." << maxInX << " y=" << minInY << "..." << maxInY);

    // allow additional cells for pre/postprocessing
    const size_t EXTEND = 2;
    if (minInX > EXTEND)
        minInX -= EXTEND;
    else
        minInX = 0;
    if (minInY > EXTEND)
        minInY -= EXTEND;
    else
        minInY = 0;
    maxInX = std::min(inX - 1, maxInX + EXTEND);
    maxInY = std::min(inY - 1, maxInY + EXTEND);
    if ((minInX > 0 || minInY > 0 || maxInX < inX - 1 || maxInY < inY - 1) && (minInX + 2 * EXTEND <= maxInX) && (minInY + 2 * EXTEND <= maxInY)) {
        const size_t redInX = maxInX - minInX + 1;
        const size_t redInY = maxInY - minInY + 1;
        for (size_t o = 0; o < outLayerSize; ++o) {
            size_t& i = pointsInIn[o];
            if (i != INVALID) {
                const size_t iy = i / inX - minInY, ix = i % inX - minInX;
                i = iy * redInX + ix;
            }
        }

        reducedDomain_ = std::make_shared<ReducedInterpolationDomain>(xDimName, yDimName, minInX, minInY);

        inX = redInX;
        inY = redInY;
    }
}

shared_array<float> CachedNNInterpolation::interpolateValues(shared_array<float> inData, size_t size, size_t& newSize) const
{
    const size_t outLayerSize = outX * outY;
    const size_t inLayerSize = inX * inY;
    const size_t inZ = size / inLayerSize;
    newSize = outLayerSize * inZ;

    shared_array<float> outData(new float[newSize]);
    std::fill(outData.get(), outData.get() + newSize, MIFI_UNDEFINED_F);

    for (size_t z = 0; z < inZ; ++z) {
        const float* inDataZ = &inData[z * inLayerSize];
        float* outDataZ = &outData[z * outLayerSize];
        for (size_t o = 0; o < outLayerSize; o++) {
            const size_t i = pointsInIn[o];
            if (i != INVALID)
                outDataZ[o] = inDataZ[i];
        }
    }
    return outData;
}

} // namespace MetNoFimex
