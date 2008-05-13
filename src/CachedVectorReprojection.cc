#include "CachedVectorReprojection.h"
#include "CDMException.h"
#include "DataImpl.h"
#include <iostream>

namespace MetNoFimex
{

void CachedVectorReprojection::reprojectValues(boost::shared_ptr<Data>& uValues, boost::shared_ptr<Data>& vValues, float uBadValue, float vBadValue) const throw(CDMException)
{
	if (ox == 0 || oy == 0 || matrix.get() == 0) {
		throw CDMException("CachedVectorReprojection not initialized"); 
	}
	size_t oz = uValues->size() / (ox*oy);
	boost::shared_array<float> u = uValues->asFloat();
	boost::shared_array<float> v = uValues->asFloat();
	mifi_bad2nanf(&u[0], &u[uValues->size()], uBadValue);
	mifi_bad2nanf(&v[0], &v[vValues->size()], vBadValue);
	int errcode = mifi_vector_reproject_values_by_matrix_f(method, matrix.get(), &u[0], &v[0], ox, oy, oz);
	if (errcode != MIFI_OK)	throw CDMException("Error during reprojection of vector-values");
	mifi_nanf2bad(&u[0], &u[uValues->size()], uBadValue);
	mifi_nanf2bad(&v[0], &v[vValues->size()], vBadValue);
	size_t size = ox*oy*oz;
	uValues->setValues(0, DataImpl<float>(u,size), 0, size);
	vValues->setValues(0, DataImpl<float>(v,size), 0, size);
}

}
