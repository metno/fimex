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

#include "CachedInterpolation.h"
#include "DataImpl.h"
namespace MetNoFimex
{


CachedInterpolation::CachedInterpolation(int funcType, std::vector<double> pointsOnXAxis, std::vector<double> pointsOnYAxis, size_t inX, size_t inY, size_t outX, size_t outY)
: pointsOnXAxis(pointsOnXAxis), pointsOnYAxis(pointsOnYAxis), inX(inX), inY(inY), outX(outX), outY(outY) {
	switch (funcType) {
	case MIFI_BILINEAR: this->func = mifi_get_values_bilinear_f; break;
	case MIFI_BICUBIC:  this->func = mifi_get_values_bicubic_f; break;
	case MIFI_NEAREST_NEIGHBOR: this->func = mifi_get_values_f; break;
	default: throw CDMException("unknown interpolation function: " + funcType);
	}
}

boost::shared_ptr<Data> CachedInterpolation::interpolateValues(boost::shared_ptr<Data> inData, float badValue) {
	size_t inZ = inData->size() / (inX*inY);
	boost::shared_array<float> zValues(new float[inZ]);
	const boost::shared_array<float> inFloatData = inData->asConstFloat(); 
	mifi_bad2nanf(&inFloatData[0], &inFloatData[inData->size()], badValue);
	boost::shared_array<float> outfield(new float[outX*outY*inZ]);
	for (size_t y = 0; y < outY; ++y) {
		for (size_t x = 0; x < outX; ++x) {
			if (func(inFloatData.get(), zValues.get(), pointsOnXAxis[y*outX+x], pointsOnYAxis[y*outX+x], inX, inY, inZ) != MIFI_ERROR) {
				for (size_t z = 0; z < inZ; ++z) {
					outfield[mifi_3d_array_position(x, y, z, outX, outY, inZ)] = zValues[z];
				}			
			} else (throw CDMException("error during interpolation"));
		}
	}
	mifi_nanf2bad(&outfield[0], &outfield[outX*outY*inZ], badValue);
	return boost::shared_ptr<DataImpl<float> >(new DataImpl<float>(outfield, outX*outY*inZ));
}

}
