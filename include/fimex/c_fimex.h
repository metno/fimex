/*
 * Fimex, c_fimex.h
 *
 * (C) Copyright 2009-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

/**
 * @headerfile fimex/c_fimex.h
 */
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
extern void mifi_free_cdm_reader(mifi_cdm_reader* reader);

/**
 * Get a new reader from a file.
 * @param file_type #mifi_filetype constant. To get a CDMReaderWriter, use MIFI_FILETYPE_NETCDF|MIFI_FILETYPE_RW.
 * @param filename name of the felt-file
 * @param configFile configuration file for the felt-file
 * @return the reader object-pointer, use #mifi_free_cdm_reader to free, or NULL on error.
 */
extern mifi_cdm_reader* mifi_new_io_reader(const char* file_type, const char* filename, const char* configFile);

/**
 * Modify a reader using a ncml file.
 * @param reader the data/cdm source
 * @param ncmlFile name of the ncml config file
 * @return the reader object-pointer, use #mifi_free_cdm_reader to free, or NULL on error.
 */
extern mifi_cdm_reader* mifi_new_ncml_modifier(mifi_cdm_reader* reader, const char* ncmlFile);

/**
 * Write the content of the reader to the filename.
 * @param reader the data source
 * @param filetype the type of the file to write
 * @param filename the name of the netcdf-file to write
 * @param configFile an optional configFile, use "" or 0 if not needed
 * @return 0 on success.
 */
extern int mifi_writer(mifi_cdm_reader* reader, const char* filetype, const char* filename, const char* configFile);

/**
 * @brief change the projection of the reader to this new projection
 *
 * @param reader the original data-source
 * @param method Interpolation method
 * @param proj_input input-string for proj4, used as output projection
 * @param out_x_axis config-string for x_axis, either '1,2,...,5' or 'auto' or 'auto,distance=3.5'
 * @param out_y_axis config-string for y_axis, either '1,2,...,5' or 'auto' or 'auto,distance=3.5'
 * @param out_x_axis_unit unit of the output x-axis
 * @param out_y_axis_unit unit of the output y-axis
 * @return the reader object-pointer, use #mifi_free_cdm_reader to free, or NULL on error.
 */
extern mifi_cdm_reader* mifi_new_cdminterpolator(mifi_cdm_reader* reader, int method, const char* proj_input, const char* out_x_axis, const char* out_y_axis, const char* out_x_axis_unit, const char* out_y_axis_unit);

/**
 * @brief change the projection of the reader to this new projection
 *
 * @param reader the original data-source
 * @param method Interpolation method
 * @param n number of latitude-longitude points
 * @param lonVals array of size n with longitude positions
 * @param latVals array of size n with latitude positions
 * @return the reader object-pointer, use #mifi_free_cdm_reader to free, or NULL on error.
 */
extern mifi_cdm_reader* mifi_new_lonlat_interpolator(mifi_cdm_reader* reader, int method, int n, const double* lonVals, const double* latVals);


/**
 * Get a new reader which allows setting c-callback functions.
 * @param reader the original data-source
 * @return the reader object-pointer, use #mifi_free_cdm_reader to free, or NULL on error.
 */
extern mifi_cdm_reader* mifi_new_c_reader(mifi_cdm_reader* reader);

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
extern int mifi_set_callback_double(mifi_cdm_reader* c_reader, const char* varName, doubleDatasliceCallbackPtr callback);


/**
 * Get the number of the variables from the reader.
 * @param reader the data source
 * @return the number of variables
 */
extern size_t mifi_get_variable_number(mifi_cdm_reader* reader);
/**
 * Get the name of a variable from the reader.
 * @param reader the data source
 * @param pos the position number of the variable, should be between 0 and size-1
 * @return the variable name, or "" on failure
 */
extern const char* mifi_get_variable_name(mifi_cdm_reader* reader, size_t pos);

/**
 * Get the MetNoFimex::CDMDataType datatype for a variable.
 * @param reader the data source
 * @param varName name of the variable
 * @return datatype, or CDM_NAT=0 on error
 */
extern unsigned int mifi_get_variable_type(mifi_cdm_reader* reader, const char* varName);

/**
 * Get the number of the dimensions from the reader.
 * @param reader the data source
 * @return the number of dimension
 */
extern size_t mifi_get_dimension_number(mifi_cdm_reader* reader);
/**
 * Get the name of a dimension from the reader.
 * @param reader the data source
 * @param pos the position number of the dimensin, should be between 0 and size-1
 * @return the dimension name, or "" on failure
 */
extern const char* mifi_get_dimension_name(mifi_cdm_reader* reader, size_t pos);
/**
 * Get the size of a dimension
 * @param reader the data source
 * @param dimName the name of the dimension
 * @return the size of the dimension, or 0 if dimension does not exist
 */
extern size_t mifi_get_dimension_size(mifi_cdm_reader* reader, const char* dimName);

/**
 * Get the name of the unlimited dimension
 * @param reader
 * @return name of unlimited dimension, or ""
 */
extern const char* mifi_get_unlimited_dimension_name(mifi_cdm_reader* reader);

/**
 * Get the name of the longitude variable belonging to a parameter. The longitude
 * variable might be a dimension (1D), or an 2D field.
 * @param reader
 * @param varName The name of a parameter.
 * @return longitude-variable name, which must be free'd, or NULL
 */
extern char* mifi_get_var_longitude(mifi_cdm_reader* reader, const char* varName);

/**
 * Get the name of the latitude variable belonging to a parameter. This function is similar
 * to mifi_get_var_longitude, except that the name is copied to lonName which is expected to
 * have a capacity of n.
 * @param reader
 * @param varName The name of a parameter.
 * @param lonName The place to copy the longitude name to.
 * @param n The capacity of lonName -- 10 means at most 9 characters plus a final 0 byte.
 * @return -1 if no longitude-latitude variables found; 0 if n is too small, else the
 *         number of characters copied to lonName, including final NUL.
 */
extern int mifi_get_var_longitude_cpy(mifi_cdm_reader* reader, const char* varName, char* lonName, int n);

/**
 * Get the name of the latitude variable belonging to a parameter. The latitude
 * variable might be a dimension (1D), or an 2D field.
 * @param reader
 * @param varName The name of a parameter.
 * @return latitude-variable name, which must be free'd, or NULL
 */
extern char* mifi_get_var_latitude(mifi_cdm_reader* reader, const char* varName);

/**
 * Get the name of the latitude variable belonging to a parameter. This function is similar
 * to mifi_get_var_longitude, except that the name is copied to lonName which is expected to
 * have a capacity of n.
 * @param reader
 * @param varName The name of a parameter.
 * @param latName The place to copy the longitude name to.
 * @param n The capacity of latName -- 10 means at most 9 characters plus a final 0 byte.
 * @return -1 if no longitude-latitude variables found; 0 if n is too small, else the
 *         number of characters copied to latName, including final NUL.
 */
extern int mifi_get_var_latitude_cpy(mifi_cdm_reader* reader, const char* varName, char* latName, int n);


/**
 * Create a new slice_builder for the reader and the variable. It
 * will try to attach a coordinate-system if possible
 * @param reader
 * @param varName
 * @return a slicebuilder handle
 */
extern mifi_slicebuilder* mifi_new_slicebuilder(mifi_cdm_reader* reader, const char* varName);
/**
 * Check if the slicebuilder is connected to a coordinate-system.
 * @param sb
 * @return 1 on success, 0 on failure
 */
extern int mifi_slicebuilder_has_CS(mifi_slicebuilder* sb);

/**
 * Get the projection of the slicebuilder, if it belongs to a
 * coordinate-system with projection.
 * @param sb
 * @return proj4 string or empty string, which both must be free'd
 */
extern const char* mifi_slicebuilder_get_proj4(mifi_slicebuilder* sb);

/**
 * Get the projection of the slicebuilder. This function is similar to
 * mifi_slicebuilder_get_proj4, except that the name is copied to proj4 which
 * is expected to have a capacity of n.
 * @param sb
 * @param proj4 The place to copy the projection information to.
 * @param n The capacity of proj4 -- 10 means at most 9 characters plus a final 0 byte.
 * @return 0 if n is too small, else the number of characters copied to proj4, including final NUL.
 */
extern int mifi_slicebuilder_get_proj4_cpy(mifi_slicebuilder* sb, char* proj4, int n);

/**
 * Get the number of dimensions (i.e. the rank) of the slicebuilder.
 * @param sb
 * @return rank
 */
extern int mifi_slicebuilder_ndims(mifi_slicebuilder* sb);
/**
 * Get the name of the dimension of the slicebuilder at a certain
 * position.
 * @param sb
 * @param pos
 * @return dimension's name or empty string, which both must be free'd
 */
extern const char* mifi_slicebuilder_dimname(mifi_slicebuilder* sb, int pos);

/**
 * Get the projection of the slicebuilder. This function is similar to
 * mifi_slicebuilder_get_proj4, except that the name is copied to proj4 which
 * is expected to have a capacity of n.
 * @param sb
 * @param pos
 * @param dimName The place to copy the dimension name to.
 * @param n The capacity of dimName -- 10 means at most 9 characters plus a final 0 byte.
 * @return 0 if n is too small, else the number of characters copied to dimName, including final NUL.
 */
extern int mifi_slicebuilder_dimname_cpy(mifi_slicebuilder* sb, int pos, char* dimName, int n);

/**
 * Get the current start positions and dimension-sizes for all dimensions.
 * @param sb
 * @param start pre-allocated array of size ndims
 * @param size pre-allocated array of size ndims
 * @return 0 on success, < 0 on failure
 */
extern int mifi_slicebuilder_get_start_size(mifi_slicebuilder* sb, unsigned int* start, unsigned int* size);

extern int mifi_slicebuilder_get_axistype(mifi_slicebuilder* sb, int* axistype);


/**
 * Set the start-position (starting with 0) and size of a named dimension.
 * @param sb
 * @param dimName
 * @param start
 * @param size
 * @return 0 on success, < 0 on failure
 */
extern int mifi_slicebuilder_set_dim_start_size(mifi_slicebuilder* sb, const char* dimName, unsigned int start, unsigned int size);
/**
 * Free the memory allocated for the slicebuilder.
 * @param sb
 */
extern void mifi_free_slicebuilder(mifi_slicebuilder* sb);



/**
 * get a slice of data from the dataReader
 * @param reader dataReader to read the data from
 * @param varName variable name associated with the data
 * @param unLimDimPos unlimited dimension of the slice
 * @param data: the returned data. It will be allocated automatically, it is the task of the user to <b>free</b> it. Undefined values will be NaN.
 * @param size: the size of the returned data.
 * @return 0 on success
 */
extern int mifi_get_double_dataslice(mifi_cdm_reader* reader, const char* varName, size_t unLimDimPos, double** data, size_t* size);

/**
 * get all the data from the dataReader
 * @param reader dataReader to read the data from
 * @param varName variable name associated with the data
 * @param data: the returned data. It will be allocated automatically, it is the task of the user to <b>free</b> it. Undefined values will be NaN.
 * @param size: the size of the returned data.
 * @return 0 on success
 */
extern int mifi_get_double_data(mifi_cdm_reader* reader, const char* varName, double** data, size_t* size);

/**
 * Read information from the readers variable to the pre-allocated
 * data-variable.
 *
 * @param reader the data-source
 * @param varName variable-name to read
 * @param sb The slicebuilder to restrict dimensions. It is possible to reuse a slicebuilder for several variables with the same dimensions.
 * @param units Units of the data. Scaling and unit-conversion will be done automatically. Use units = "" if you don't want any units-conversion. Units need to be udunits-compatible.
 * @param data Preallocated data. The total size must be identical to the slicebuilders total size.
 * @param size The actually read data. This might be <= the requested data if data not available.
 * @return 0 on success
 */
extern int mifi_fill_scaled_double_dataslice(mifi_cdm_reader* reader, const char* varName, mifi_slicebuilder* sb, const char* units, double* data, size_t* size);

/**
 * Write data to the variable on disk
 *
 * @param rwreader the data-sink, should be opened with mifi_new_io_reader() with MIFI_FILETYPE_RW|MIFI_FILETYPE_NETCDF
 * @param varName variable-name to read
 * @param sb The slicebuilder to restrict dimensions. It is possible to reuse a slicebuilder for several variables with the same dimensions.
 * @param units Units of the data. Scaling and unit-conversion will be done automatically. Use units = "" if you don't want any units-conversion. Units need to be udunits-compatible.
 * @param data Preallocated data. The total size must be identical to the slicebuilders total size.
 * @param size The size of data.
 * @return 0 on success
 */
extern int mifi_write_scaled_double_dataslice(mifi_cdm_reader* rwreader, const char* varName, mifi_slicebuilder* sb, const char* units, double* data, size_t size);


/**
 * get the unique forecast reference time in a unit
 * @param reader
 * @param units a unit for point in time, e.g. "seconds since 1970-01-01"
 * @return time in the given unit, or NaN
 */
double mifi_get_unique_forecast_reference_time(mifi_cdm_reader* reader, const char* units);

/**
 * Set default logging level.
 */
extern void mifi_set_default_log_level(int loglevel);

#ifdef __cplusplus
}
#endif

#endif /* C_FIMEX_H_ */
