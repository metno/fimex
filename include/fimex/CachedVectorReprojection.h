#ifndef CACHEDVECTORREPROJECTION_H_
#define CACHEDVECTORREPROJECTION_H_

#include <boost/shared_ptr.hpp>
#include "Data.h"
#include "interpolation.h"

namespace MetNoFimex
{

class CachedVectorReprojection
{
public:
	CachedVectorReprojection() : method(MIFI_OK), matrix(new double[0]), ox(0), oy(0) {}
	CachedVectorReprojection(int method, boost::shared_array<double> matrix, int ox, int oy) : method(method), matrix(matrix), ox(ox), oy(oy) {}
	virtual ~CachedVectorReprojection() {}
	/**
	 *  reproject the vector values
	 * 
	 * @param uValues the values in x-direction. These will be changed in-place.
	 * @param vValues the values in y-direction. These will be changed in-place.
	 */
	void reprojectValues(boost::shared_ptr<Data>& uValues, boost::shared_ptr<Data>& vValues, float uBadValue = MIFI_UNDEFINED_F, float vBadValue = MIFI_UNDEFINED_F) const throw(CDMException);
	// @return size of the spatial plane in x-direction
	size_t getXSize() const {return ox;}
	// @return size of the spatial plane in y-direction
	size_t getYSize() const {return oy;}
	
private:
	int method;
	boost::shared_array<double> matrix;
	size_t ox;
	size_t oy;
};

}

#endif /*CACHEDVECTORREPROJECTION_H_*/
