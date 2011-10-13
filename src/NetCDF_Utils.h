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
#include "fimex/CDMDataType.h"

namespace MetNoFimex
{
// forward decl
class Data;

/// storage class for netcdf-file pointer
class Nc {
public:
    Nc() : isOpen(false) {}
    ~Nc();
    std::string filename;
    int ncId;
    int format;
    bool isOpen;
};




/**
 * @headerfile "fimex/NetCDF_Utils.h"
 */

/**
 * conversion from CDMDataType to NcType
 */
int cdmDataType2ncType(CDMDataType dt);

/**
 * conversion from nc_type to CDMDataType
 */
CDMDataType ncType2cdmDataType(int nc_type);

/**
 * read a nc-status and throw an error if status != NC_NOERR
 */
void ncCheck(int status);

/**
 * read values from an attribute to a data
 * @param ncId netcdf file id
 * @param varId variable id or NC_GLOBAL
 * @param attName attribute name
 * @param nc_type attribute datatype (in netcdf-notation)
 */
boost::shared_ptr<Data> ncGetAttValues(int ncId, int varId, const std::string& attName, int nc_type);
/**
 * read value-slices from a variable to a data
 * @param ncId netcdf file id
 * @param varId variable id or NC_GLOBAL
 * @param nc_type attribute datatype (in netcdf-notation)
 * @param dimLen number of dimensions
 * @param start start-point for each dimension
 * @param count size in each dimension
 */
boost::shared_ptr<Data> ncGetValues(int ncId, int varId, int nc_type, size_t dimLen, const size_t* start, const size_t* count);

/**
 * write value-slices from a variable to disk
 * @param data the data to put
 * @param ncId netcdf file id
 * @param varId variable id or NC_GLOBAL
 * @param nc_type attribute datatype (in netcdf-notation)
 * @param dimLen number of dimensions
 * @param start start-point for each dimension
 * @param count size in each dimension
 */
void ncPutValues(boost::shared_ptr<Data> data, int ncId, int varId, int nc_type, size_t dimLen, const size_t* start, const size_t* count);


}

#endif /*NETCDF_UTILS_H_*/
