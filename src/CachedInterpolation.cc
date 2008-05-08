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

boost::shared_ptr<Data> CachedInterpolation::interpolateValues(boost::shared_ptr<Data> inData) {
	size_t inZ = inData->size() / (inX*inY);
	boost::shared_array<float> zValues(new float[inZ]);
	const boost::shared_array<float> inFloatData = inData->asConstFloat(); 
	boost::shared_array<float> outfield(new float[outX*outY*inZ]);
	for (size_t x = 0; x < outX; ++x) {
		for (size_t y = 0; y < outY; ++y) {
			if (func(inFloatData.get(), zValues.get(), pointsOnXAxis[y*outX+x], pointsOnYAxis[y*outX+x], inX, inY, inZ) != MIFI_ERROR) {
				for (size_t z = 0; z < inZ; ++z) {
					outfield[mifi_3d_array_position(x, y, z, outX, outY, inZ)] = zValues[z];
				}			
			}		
		}
	}
	return boost::shared_ptr<DataImpl<float> >(new DataImpl<float>(outfield, outX*outY*inZ));
}

}
