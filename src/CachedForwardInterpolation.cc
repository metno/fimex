/*
 * Fimex, ForwardInterpolation.cc
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
 *  Created on: May 15, 2009
 *      Author: Heiko Klein
 */

#include "CachedForwardInterpolation.h"
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/interpolation.h"

#include <numeric>
#include <algorithm>

#include <iostream>

using namespace std;

namespace MetNoFimex
{

namespace {
Logger_p logger = getLogger("fimex.CachedForwardInterpolation");
} // namespace

namespace {

const size_t INVALID = ~0u;

} // namespace

struct Aggregator
{
    virtual ~Aggregator();
    virtual void push(float f) = 0;
    virtual float get_and_reset() = 0;
};

Aggregator::~Aggregator() {}

struct AggSimple : Aggregator
{
    size_t count;
    float val;
    AggSimple() { reset(0); }
    float get_and_reset() override { return reset(val); }
    float reset(float v)
    {
        val = 0;
        if (count == 0)
            return MIFI_UNDEFINED_F;
        count = 0;
        return v;
    }
};

struct AggSum : AggSimple
{
    void push(float f) override
    {
        count += 1;
        val += f;
    }
};

struct AggMean : AggSum
{
    float get_and_reset() override { return reset(count > 0 ? val / count : 0); }
};

struct AggMin : AggSimple
{
    void push(float f) override
    {
        if (count == 0 || f < val)
            val = f;
        count += 1;
    }
};

struct AggMax : AggSimple
{
    void push(float f) override
    {
        if (count == 0 || f > val)
            val = f;
        count += 1;
    }
};

struct AggMedian : Aggregator
{
    std::vector<float> values;
    AggMedian(size_t n) { values.reserve(n); }
    void push(float f) override { values.push_back(f); }
    float get_and_reset() override;
};

float AggMedian::get_and_reset()
{
    if (values.empty())
        return MIFI_UNDEFINED_F;

    std::nth_element(values.begin(), values.begin() + values.size() / 2, values.end());
    const float median = values[values.size() / 2];
    values.clear();
    return median;
}

// pointsOnXAxis map each point in inData[y*inX+x] to a x-position in outData
CachedForwardInterpolation::CachedForwardInterpolation(const std::string& xDimName, const std::string& yDimName, int funcType, shared_array<double> pOnX,
                                                       shared_array<double> pOnY, size_t inx, size_t iny, size_t outx, size_t outy)
    : CachedInterpolationInterface(xDimName, yDimName, inx, iny, outx, outy)
{
    const size_t outLayerSize = outX * outY;
    pointsInIn = shared_array<vector<size_t>>(new vector<size_t>[outLayerSize]);
    size_t minInX = inX, maxInX = 0, minInY = inY, maxInY = 0;
    maxPointsInIn = 0;
    const RoundAndClamp roundX(0, outX - 1, INVALID);
    const RoundAndClamp roundY(0, outY - 1, INVALID);
    for (size_t iy = 0; iy < inY; ++iy) {
        for (size_t ix = 0; ix < inX; ++ix) {
            const size_t i = iy * inX + ix;
            const size_t px = roundX(pOnX[i]), py = roundY(pOnY[i]);
            if (px != INVALID && py != INVALID) {
                std::vector<size_t>& pi = pointsInIn[py * outX + px];
                if (iy > maxInY)
                    maxInY = iy;
                if (iy < minInY)
                    minInY = iy;
                if (ix > maxInX)
                    maxInX = ix;
                if (ix < minInX)
                    minInX = ix;
                pi.push_back(i);
                if (pi.size() > maxPointsInIn)
                    maxPointsInIn = pi.size();
            }
        }
    }
    LOG4FIMEX(logger, Logger::DEBUG, "maxPointsInIn=" << maxPointsInIn);

    // allow additional cells for pre/postprocessing
    const size_t EXTEND = 2;
    if (minInX > EXTEND) minInX -= EXTEND; else minInX = 0;
    if (minInY > EXTEND) minInY -= EXTEND; else minInY = 0;
    maxInX = std::min(inX-1, maxInX + EXTEND);
    maxInY = std::min(inY-1, maxInY + EXTEND);
    if ((minInX > 0 || minInY > 0 || maxInX < inX - 1 || maxInY < inY - 1) && (minInX + 2 * EXTEND <= maxInX) && (minInY + 2 * EXTEND <= maxInY)) {
        const size_t redInX = maxInX - minInX + 1;
        const size_t redInY = maxInY - minInY + 1;
        for (size_t o = 0; o < outLayerSize; ++o) {
            for (size_t& i : pointsInIn[o]) {
                const size_t iy = i / inX - minInY, ix = i % inX - minInX;
                i = iy * redInX + ix;
            }
        }

        reducedDomain_ = std::make_shared<ReducedInterpolationDomain>(xDimName, yDimName, minInX, minInY);

        inX = redInX;
        inY = redInY;
    }

    undefAggr = false;
    // clang-format off
    switch (funcType) {
    case MIFI_INTERPOL_FORWARD_UNDEF_SUM: undefAggr = true; // fallthrough
    case MIFI_INTERPOL_FORWARD_SUM: agg.reset(new AggSum()); break;
    case MIFI_INTERPOL_FORWARD_UNDEF_MEAN: undefAggr = true; // fallthrough
    case MIFI_INTERPOL_FORWARD_MEAN: agg.reset(new AggMean()); break;
    case MIFI_INTERPOL_FORWARD_UNDEF_MEDIAN: undefAggr = true; // fallthrough
    case MIFI_INTERPOL_FORWARD_MEDIAN: agg.reset(new AggMedian(maxPointsInIn)); break;
    case MIFI_INTERPOL_FORWARD_UNDEF_MAX: undefAggr = true; // fallthrough
    case MIFI_INTERPOL_FORWARD_MAX: agg.reset(new AggMax()); break;
    case MIFI_INTERPOL_FORWARD_UNDEF_MIN: undefAggr = true; // fallthrough
    case MIFI_INTERPOL_FORWARD_MIN: agg.reset(new AggMin()); break;
    default: throw CDMException("unknown forward interpolation method: " + type2string(funcType));
    }
    // clang-format on
}

CachedForwardInterpolation::~CachedForwardInterpolation() {}

shared_array<float> CachedForwardInterpolation::interpolateValues(shared_array<float> inData, size_t size, size_t& newSize) const
{
    size_t outLayerSize = outX*outY;
    size_t inLayerSize = inX * inY;
    size_t inZ = size / inLayerSize;
    newSize = outLayerSize*inZ;
    shared_array<float> outData(new float[newSize]);
    for (size_t z = 0; z < inZ; ++z) {
        float* outDataIt = &outData[z*outLayerSize];
        for (size_t i = 0; i < outLayerSize; i++) {
            for (size_t ii : pointsInIn[i]) {
                const float val = inData[ii];
                if (undefAggr || !mifi_isnan(val))
                    agg->push(val);
            }
            *outDataIt++ = agg->get_and_reset();
        }
    }
    return outData;
}

} // namespace MetNoFimex
