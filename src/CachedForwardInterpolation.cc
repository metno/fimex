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

float aggrSum(const vector<float>& vec) {
    return accumulate(vec.begin(), vec.end(), 0.f);
}
float aggrMean(const vector<float>& vec) {
    if (vec.size() > 0) {
        return aggrSum(vec)/vec.size();
    } else {
        return MIFI_UNDEFINED_F;
    }
}
float aggrMedian(const vector<float>& vec) {
    vector<float> vecCopy = vec;
    nth_element(vecCopy.begin(), vecCopy.begin()+vecCopy.size()/2, vecCopy.end());
    float median = vecCopy[vecCopy.size()/2];
    return median;
}
float aggrMax(const vector<float>& vec) {
    return *(max_element(vec.begin(), vec.end()));
}
float aggrMin(const vector<float>& vec) {
    return *(min_element(vec.begin(), vec.end()));
}

// pointsOnXAxis map each point in inData[y*inX+x] to a x-position in outData
CachedForwardInterpolation::CachedForwardInterpolation(int funcType, std::vector<double> pointsOnXAxis, std::vector<double> pointsOnYAxis, size_t inX, size_t inY, size_t outX, size_t outY)
: pointsOnXAxis(pointsOnXAxis), pointsOnYAxis(pointsOnYAxis), inX(inX), inY(inY), outX(outX), outY(outY)
{
    switch (funcType) {
    case MIFI_INTERPOL_FORWARD_SUM: aggrFunc = aggrSum; break;
    case MIFI_INTERPOL_FORWARD_MEAN: aggrFunc = aggrMean; break;
    case MIFI_INTERPOL_FORWARD_MEDIAN: aggrFunc = aggrMedian; break;
    case MIFI_INTERPOL_FORWARD_MAX: aggrFunc = aggrMax; break;
    case MIFI_INTERPOL_FORWARD_MIN: aggrFunc = aggrMin; break;
    default: throw CDMException("unknown forward interpolation method: " + type2string(funcType));
    }
}

boost::shared_array<float> CachedForwardInterpolation::interpolateValues(boost::shared_array<float> inData, size_t size, size_t& newSize) const
{

    size_t outLayerSize = outX*outY;
    size_t inZ = size / (inX*inY);
    newSize = outLayerSize*inZ;
    boost::shared_array<float> outData(new float[newSize]);
    float *inDataIt = &inData[0];
    // foreach value in inData, add to closes position in outData
    for (size_t z = 0; z < inZ; ++z) {
        boost::shared_array<vector<float> > tempOut(new vector<float>[outLayerSize]);
        for (size_t y = 0; y < inY; ++y) {
            for (size_t x = 0; x < inX; ++x) {
                float val = *inDataIt++;
                if (!mifi_isnan(val)) {
                    int xOutPos = MetNoFimex::round(pointsOnXAxis[y*inX+x]);
                    if (xOutPos >= 0 && xOutPos < static_cast<int>(outX)) {
                        int yOutPos = MetNoFimex::round(pointsOnYAxis[y*inX+x]);
                        if (yOutPos >= 0 && yOutPos < static_cast<int>(outY)) {
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
            vector<float> vals = *tempOutIt++;
            if (vals.empty()) {
                *outDataIt++ = MIFI_UNDEFINED_F;
            } else {
                *outDataIt++ = aggrFunc(vals);
            }
        }
    }
    return outData;
}

}
