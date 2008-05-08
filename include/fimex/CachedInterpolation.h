#ifndef CACHEDINTERPOLATION_H_
#define CACHEDINTERPOLATION_H_

#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>
#include "interpolation.h"
#include "Data.h"

namespace MetNoFimex
{

/**
 * Container to cache projection details to speed up
 * interpolation of lots of fields.
 */
class CachedInterpolation
{
private:
	std::vector<double> pointsOnXAxis;
	std::vector<double> pointsOnYAxis;
	size_t inX;
	size_t inY;
	size_t outX;
	size_t outY;
	int (*func)(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz);
public:
	CachedInterpolation() : inX(0), inY(0), outX(0), outY(0), func(mifi_get_values_f) {}
	/**
	 * @param funcType {@link interpolation.h} interpolation method
	 * @param pointsOnXAxis projected values of the new projections coordinates expressed in the current x-coordinate (size = outX*outY)
	 * @param pointsOnYAxis projected values of the new projections coordinates expressed in the current y-coordinate (size = outX*outY)
	 * @param inX size of current X axis
	 * @param inY size of current Y axis
	 * @param outX size of new X axis
	 * @param outY size of new Y axis
	 */
	CachedInterpolation(int funcType, std::vector<double> pointsOnXAxis, std::vector<double> pointsOnYAxis, size_t inX, size_t inY, size_t outX, size_t outY);
	virtual ~CachedInterpolation() {}
	boost::shared_ptr<Data> interpolateValues(boost::shared_ptr<Data> inData);
};



}

#endif /*CACHEDINTERPOLATION_H_*/
