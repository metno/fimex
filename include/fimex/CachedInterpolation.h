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

#ifndef CACHEDINTERPOLATION_H_
#define CACHEDINTERPOLATION_H_

#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>
#include "fimex/interpolation.h"
#include "fimex/Data.h"

namespace MetNoFimex
{

/**
 * Interface for new cached spatial interpolation as used in #MetNoFimex::CDMInterpolator
 */
class CachedInterpolationInterface {
public:
    virtual boost::shared_array<float> interpolateValues(boost::shared_array<float> inData, size_t size, size_t& newSize) const = 0;
};

/**
 * Container to cache projection details to speed up
 * interpolation of lots of fields.
 */
class CachedInterpolation : public CachedInterpolationInterface
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
	/**
	 * Actually interpolate the data. The data will be interpolated as floats internally.
	 *
	 * @param inData the input data
	 * @param the size of the input data array
	 * @param newSize return the size of the output-array
	 */
	virtual boost::shared_array<float> interpolateValues(boost::shared_array<float> inData, size_t size, size_t& newSize) const;
};



}

#endif /*CACHEDINTERPOLATION_H_*/
