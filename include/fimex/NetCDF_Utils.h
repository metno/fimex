#ifndef NETCDF_UTILS_H_
#define NETCDF_UTILS_H_

#include <boost/shared_ptr.hpp>
#include <netcdfcpp.h>
#include "CDMDataType.h"
#include "Data.h"

namespace MetNoUtplukk
{

/**
 * conversion from CDMDataType to NcType
 */
NcType cdmDataType2ncType(CDMDataType dt);

/**
 * conversion from NcType to CDMDataType
 */
CDMDataType ncType2cdmDataType(NcType dt);

/**
 * convert ncValues to a Data pointer
 * @warning: the data belonging to values will be freed within this function
 * or with the shared_array. Do not free the values otherwise! 
 */
boost::shared_ptr<Data> ncValues2Data(NcValues* values, NcType dt, size_t length);

}

#endif /*NETCDF_UTILS_H_*/
