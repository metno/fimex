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

#ifndef NETCDF_UTILS_H_
#define NETCDF_UTILS_H_

#include <boost/shared_ptr.hpp>
#include "netcdfcpp.h"
extern "C" {
#include "netcdf.h"
}
#include "fimex/CDMDataType.h"

namespace MetNoFimex
{
// forward decl
class Data;

/**
 * @headerfile "fimex/NetCDF_Utils.h"
 */

/**
 * conversion from CDMDataType to NcType
 */
NcType cdmDataType2ncType(CDMDataType dt);

/**
 * conversion from NcType to CDMDataType
 */
CDMDataType ncType2cdmDataType(NcType dt);

/**
 * conversion from nc_type to CDMDataType
 */
CDMDataType ncType2cdmDataType(nc_type dt);

/**
 * read a nc-status and throw an error if status != NC_NOERR
 */
void ncCheck(int status);

boost::shared_ptr<Data> ncGetAttValues(int ncId, int varId, const std::string& attName, nc_type dt);
boost::shared_ptr<Data> ncGetValues(int ncId, int varId, nc_type dt, size_t dimLen, const size_t* start, const size_t* count);


/**
 * convert void* pointer to a Data pointer
 * @warning: the data belonging to values will be delete[]ed within this function
 * or with the shared_array. Do not free the values otherwise!
 */
boost::shared_ptr<Data> ncValues2Data(void* values, nc_type dt, size_t length);

/**
 * convert ncValues to a Data pointer
 * @warning: the data belonging to values will be freed within this function
 * or with the shared_array. Do not free the values otherwise!
 */
boost::shared_ptr<Data> ncValues2Data(NcValues* values, NcType dt, size_t length);

}

#endif /*NETCDF_UTILS_H_*/
