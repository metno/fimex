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

#include "fimex/CachedForwardInterpolation.h"
#include "fimex/interpolation.h"
#include <numeric>

using namespace std;

namespace MetNoFimex
{

// pointsOnXAxis map each point in inData[y*inX+x] to a x-position in outData
CachedForwardInterpolation::CachedForwardInterpolation(int funcType, std::vector<double> pointsOnXAxis, std::vector<double> pointsOnYAxis, size_t inX, size_t inY, size_t outX, size_t outY)
: pointsOnXAxis(pointsOnXAxis), pointsOnYAxis(pointsOnYAxis), inX(inX), inY(inY), outX(outX), outY(outY)
{
    // TODO Auto-generated constructor stub

}

CachedForwardInterpolation::~CachedForwardInterpolation()
{
    // TODO Auto-generated destructor stub
}

boost::shared_array<float> CachedForwardInterpolation::interpolateValues(boost::shared_array<float> inData, size_t size, size_t& newSize)
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
                if (!isnanf(val)) {
                    size_t xOutPos = pointsOnXAxis[y*inX+x];
                    if (xOutPos > 0 && xOutPos < outX) {
                        size_t yOutPos = pointsOnYAxis[y*inX+x];
                        if (yOutPos > 0 && yOutPos < outY) {
                            tempOut[xOutPos*outX + yOutPos].push_back(val);
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
                // TODO: here should be a selection of functions (min,max,sum,avg,median)
                *outDataIt++ = accumulate(vals.begin(), vals.end(), 0.f) / vals.size();
            }
        }
    }
    return outData;
}

}
