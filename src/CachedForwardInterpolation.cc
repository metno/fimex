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
#include "fimex/interpolation.h"
#include "fimex/Utils.h"
#include <numeric>
#include <algorithm>

using namespace std;

namespace MetNoFimex
{

float aggrSum(vector<float>& vec) {
#if 0
    for (vector<float>::const_iterator vit = vec.begin(); vit != vec.end(); ++vit)
        cerr << *vit << " ";
    cerr << endl;
#endif
    return accumulate(vec.begin(), vec.end(), 0.f);
}
float aggrMean(vector<float>& vec) {
    return aggrSum(vec)/vec.size();
}
float aggrMedian(vector<float>& vec) {
    nth_element(vec.begin(), vec.begin()+vec.size()/2, vec.end());
    float median = vec[vec.size()/2];
    return median;
}
float aggrMax(vector<float>& vec) {
    return *(max_element(vec.begin(), vec.end()));
}
float aggrMin(vector<float>& vec) {
    return *(min_element(vec.begin(), vec.end()));
}

// pointsOnXAxis map each point in inData[y*inX+x] to a x-position in outData
CachedForwardInterpolation::CachedForwardInterpolation(const std::string& xDimName, const std::string& yDimName, int funcType,
                                                       const std::vector<double>& pOnX, const std::vector<double>& pOnY,
                                                       size_t inX, size_t inY, size_t outX, size_t outY)
  : CachedInterpolationInterface(xDimName, yDimName)
  , inX(inX)
  , inY(inY)
  , outX(outX)
  , outY(outY)
{
    pointsOnXAxis.reserve(pOnX.size());
    pointsOnYAxis.reserve(pOnY.size());
    std::transform(pOnX.begin(), pOnX.end(), std::back_inserter(pointsOnXAxis), RoundAndClamp(0, outX-1, -1));
    std::transform(pOnY.begin(), pOnY.end(), std::back_inserter(pointsOnYAxis), RoundAndClamp(0, outY-1, -1));

    undefAggr = false;
    switch (funcType) {
    case MIFI_INTERPOL_FORWARD_SUM: aggrFunc = aggrSum; break;
    case MIFI_INTERPOL_FORWARD_MEAN: aggrFunc = aggrMean; break;
    case MIFI_INTERPOL_FORWARD_MEDIAN: aggrFunc = aggrMedian; break;
    case MIFI_INTERPOL_FORWARD_MAX: aggrFunc = aggrMax; break;
    case MIFI_INTERPOL_FORWARD_MIN: aggrFunc = aggrMin; break;
    case MIFI_INTERPOL_FORWARD_UNDEF_SUM: aggrFunc = aggrSum; undefAggr = true; break;
    case MIFI_INTERPOL_FORWARD_UNDEF_MEAN: aggrFunc = aggrMean; undefAggr = true; break;
    case MIFI_INTERPOL_FORWARD_UNDEF_MEDIAN: aggrFunc = aggrMedian; undefAggr = true; break;
    case MIFI_INTERPOL_FORWARD_UNDEF_MAX: aggrFunc = aggrMax; undefAggr = true; break;
    case MIFI_INTERPOL_FORWARD_UNDEF_MIN: aggrFunc = aggrMin; undefAggr = true; break;
    default: throw CDMException("unknown forward interpolation method: " + type2string(funcType));
    }
}

boost::shared_array<float> CachedForwardInterpolation::interpolateValues(boost::shared_array<float> inData, size_t size, size_t& newSize) const
{
    size_t outLayerSize = outX*outY;
    size_t inZ = size / (inX*inY);
    newSize = outLayerSize*inZ;
    boost::shared_array<float> outData(new float[newSize]);
    boost::shared_array< vector<float> > tempOut(new vector<float>[outLayerSize]);
    float *inDataIt = &inData[0];
    // foreach value in inData, add to closest position in outData
    for (size_t z = 0; z < inZ; ++z) {
        for (size_t y = 0; y < inY; ++y) {
            for (size_t x = 0; x < inX; ++x) {
                float val = *inDataIt++;
                if (undefAggr || !mifi_isnan(val)) {
                    int xOutPos = pointsOnXAxis[y*inX+x];
                    if (xOutPos >= 0) {
                        int yOutPos = pointsOnYAxis[y*inX+x];
                        if (yOutPos >= 0) {
                            tempOut[yOutPos*outX + xOutPos].push_back(val);
                        }
                    }
                }
            }
        }
        // foreach position in outData, aggregate all values at position with
        // sum/average/median ...
        vector<float> *tempOutIt = &tempOut[0];
        float* outDataIt = &outData[z*outLayerSize];
        for (size_t i = 0; i < outLayerSize; i++) {
            vector<float>& vals = *tempOutIt++;
            if (vals.empty()) {
                *outDataIt++ = MIFI_UNDEFINED_F;
            } else {
                *outDataIt++ = aggrFunc(vals);
            }
            vals.clear();
        }
    }
    return outData;
}

}
