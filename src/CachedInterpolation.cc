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
#include <boost/scoped_array.hpp>
#ifdef _OPENMP
#include <omp.h>
#endif


namespace MetNoFimex
{


CachedInterpolation::CachedInterpolation(int funcType, std::vector<double> pointsOnXAxis, std::vector<double> pointsOnYAxis, size_t inX, size_t inY, size_t outX, size_t outY)
: pointsOnXAxis(pointsOnXAxis), pointsOnYAxis(pointsOnYAxis), inX(inX), inY(inY), outX(outX), outY(outY) {
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

}
