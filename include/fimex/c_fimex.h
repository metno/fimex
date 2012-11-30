/*
 * Fimex, c_fimex.h
 *
 * (C) Copyright 2009, met.no
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
 *
 *  Created on: Oct 19, 2009
 *      Author: Heiko Klein
 */

#ifndef C_FIMEX_H_
#define C_FIMEX_H_

#include <stddef.h>
#include "fimex/CDMconstants.h"
#include "fimex/deprecated.h"

/**
 * This is the public C-Api for fimex. It is a wrapper api for the underlying C++ api.
 */

#ifdef __cplusplus
extern "C" {
#endif

typedef struct mifi_cdm_reader mifi_cdm_reader;
typedef struct mifi_slicebuilder mifi_slicebuilder;

/**
 * Function pointer as used for the get_double_dataslice callback function
 * @return 0 on success, error otherwise
 */
typedef int (*doubleDatasliceCallbackPtr)(mifi_cdm_reader* reader, const char* varName, size_t unLimDimPos, double* scaledData, size_t dataSize);

/**
 * Free the reader. This won't free the resources immediately, but reduce the reference counter.
 * It is therefore possible to free a reader, while it still is used within another part of the fimex-chain.
 */
void mifi_free_cdm_reader(mifi_cdm_reader* reader);

/**
 * Get a new reader from a file.
 * @param file_type MIFI_FILETYPE_* constant
 * @param filename name of the felt-file
 * @param configFile configuration file for the felt-file
 * @return the reader object-pointer, use #mifi_freeCDMReader to free, or NULL on error.
 */
mifi_cdm_reader* mifi_new_io_reader(int file_type, const char* filename, const char* configFile);

/**
 * Get a new reader from a felt file.
 * @param filename name of the felt-file
 * @param configFile configuration file for the felt-file
 * @return the reader object-pointer, use #mifi_freeCDMReader to free, or NULL on error.
 * @deprecated use mifi_new_io_reader()
 */
DEPRECATED(mifi_cdm_reader* mifi_new_felt_reader(const char* filename, const char* configFile));

/**
 * Get a new reader from a netcdf file.
 * @param filename name of the felt-file
 * @param configFile configuration file for the felt-file
 * @return the reader object-pointer, use #mifi_freeCDMReader to free, or NULL on error.
 * @deprecated use mifi_new_io_reader()
 */
DEPRECATED(mifi_cdm_reader* mifi_new_netcdf_reader(const char* filename));

/**
 * Get a new reader from a grib1/2 file.
 * @param filename name of the grib-file
 * @param configFile configuration file for the grib-file
 * @return the reader object-pointer, use #mifi_freeCDMReader to free, or NULL on error.
 * @deprecated use mifi_new_io_reader()
 */
DEPRECATED(mifi_cdm_reader* mifi_new_grib_reader(const char* filename, const char* configFile));


/**
 * Get a new reader from a ncml file.
 * @param ncmlFile name of the ncml config file
 * @return the reader object-pointer, use #mifi_freeCDMReader to free, or NULL on error.
 * @deprecated use mifi_new_io_reader()
 */
DEPRECATED(mifi_cdm_reader* mifi_new_ncml_reader(const char* ncmlFile));

/**
 * Modify a reader using a ncml file.
 * @param reader the data/cdm source
 * @param ncmlFile name of the ncml config file
 * @return the reader object-pointer, use #mifi_freeCDMReader to free, or NULL on error.
 */
mifi_cdm_reader* mifi_new_ncml_modifier(mifi_cdm_reader* reader, const char* ncmlFile);



/**
 * Write the content of the reader to the filename.
 * @param reader the data source
 * @param filename the name of the netcdf-file to write
 * @param configFile an optional configFile, use "" or 0 if not needed
 * @param version, the version of the netcdf-file. Implemented are 3 or 4.
 * @return 0 on success.
 */
int mifi_netcdf_writer(mifi_cdm_reader* reader, const char* filename, const char* configFile, int version);

/**
 * Write the content of the reader to the filename as gribfile.
 * @param reader the data source
 * @param filename the name of the grib-file to write
 * @param configFile an optional configFile, use "" or 0 if not needed
 * @param version, the version of the grib-edition. Implemented are 1 or 2.
 * @return 0 on success.
 */
int mifi_grib_writer(mifi_cdm_reader* reader, const char* filename, const char* configFile, int version);


/**
 * Fetch the whole data belonging to the cdm, but don't write it anywhere.
 * @param reader the data source
 * @return 0 on success.
 */
int mifi_nullcdm_writer(mifi_cdm_reader* reader);

/**
 * @brief change the projection of the reader to this new projection
 *
 * @param method Interpolation method
 * @param proj_input input-string for proj4, used as output projection
 * @param out_x_axis config-string for x_axis, either '1,2,...,5' or 'auto' or 'auto,distance=3.5'
 * @param out_y_axis config-string for y_axis, either '1,2,...,5' or 'auto' or 'auto,distance=3.5'
 * @param out_x_axis_unit unit of the output x-axis
 * @param out_y_axis_unit unit of the output y-axis
 * @return the reader object-pointer, use #mifi_freeCDMReader to free, or NULL on error.
 */
mifi_cdm_reader* mifi_new_cdminterpolator(mifi_cdm_reader* reader, int method, const char* proj_input, const char* out_x_axis, const char* out_y_axis, const char* out_x_axis_unit, const char* out_y_axis_unit);

/**
 * Get a new reader which allows setting c-callback functions.
 * @param the original data-source
 * @return the reader object-pointer, use #mifi_freeCDMReader to free, or NULL on error.
 */
mifi_cdm_reader* mifi_new_c_reader(mifi_cdm_reader* reader);

/**
 * Add a callback for a variable. The variable will be converted to datatype double.
 * @param c_reader the reader as created by #mifi_new_c_reader
 * @param varName the name of the variable
 * @param callback a function-ptr to the callback function
 * @return 0 on success, else error
 *
 * @warning the callback function will only be able to modify data which is available
 * in the reader. It cannot change any information the writer request, but the reader doesn't
 * now about. This data will continue to be undefined!
 */
int mifi_set_callback_double(mifi_cdm_reader* c_reader, const char* varName, doubleDatasliceCallbackPtr callback);


/**
 * Get the number of the variables from the reader.
 * @param reader the data source
 * @return the number of variables
 */
size_t mifi_get_variable_number(mifi_cdm_reader* reader);
/**
 * Get the name of a variable from the reader.
 * @param reader the data source
 * @param pos the position number of the variable, should be between 0 and size-1
 * @return the variable name, or NULL on failure
 */
const char* mifi_get_variable_name(mifi_cdm_reader* reader, size_t pos);


mifi_slicebuilder* mifi_new_slicebuilder(mifi_cdm_reader* reader, const char* varName);
void mifi_free_slicebuilder(mifi_slicebuilder* sb);



/**
 * get a slice of data from the dataReader
 * @param reader dataReader to read the data from
 * @param varName variable name associated with the data
 * @param unLimDimPos unlimited dimension of the slice
 * @param data: the returned data. It will be allocated automatically, it is the task of the user to <b>free</b> it. Undefined values will be NaN.
 * @param size: the size of the returned data.
 * @return 0 on success
 */
int mifi_get_double_dataslize(mifi_cdm_reader* reader, const char* varName, size_t unLimDimPos, double** data, size_t* size);

/**
 * get all the data from the dataReader
 * @param reader dataReader to read the data from
 * @param varName variable name associated with the data
 * @param data: the returned data. It will be allocated automatically, it is the task of the user to <b>free</b> it. Undefined values will be NaN.
 * @param size: the size of the returned data.
 * @return 0 on success
 */
int mifi_get_double_data(mifi_cdm_reader* reader, const char* varName, double** data, size_t* size);




#ifdef __cplusplus
}
#endif

#endif /* C_FIMEX_H_ */
