#include "CachedVectorReprojection.h"
#include "interpolation.h"
#include "CDMException.h"
#include "DataImpl.h"

namespace MetNoFimex
{

void CachedVectorReprojection::reprojectValues(boost::shared_ptr<Data>& uValues, boost::shared_ptr<Data>& vValues) const throw(CDMException)
{
	if (ox == 0 || oy == 0 || matrix.get() == 0) {
		throw CDMException("CachedVectorReprojection not initialized"); 
	}
	size_t oz = uValues->size() / (ox*oy);
	boost::shared_array<float> u = uValues->asFloat();
	boost::shared_array<float> v = uValues->asFloat();
	int errcode = mifi_vector_reproject_values_by_matrix_f(method, matrix.get(), u.get(), v.get(), ox, oy, oz);
	if (errcode != MIFI_OK)	throw CDMException("Error during reprojection of vector-values");
	size_t size = ox*oy*oz;
	uValues->setValues(0, DataImpl<float>(u,size), size);
	vValues->setValues(0, DataImpl<float>(v,size), size);
}

}
