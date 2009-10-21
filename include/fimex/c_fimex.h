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

/**
 * This is the public C-Api for fimex. It is a wrapper api for the underlying C++ api.
 */

#ifdef __cplusplus
extern "C" {
#endif

typedef struct mifi_cdm_reader mifi_cdm_reader;

/**
 * Free the reader. This won't free the resources immediately, but reduce the reference counter.
 * It is therefore possible to free a reader, while it still is used within another part of the fimex-chain.
 */
void mifi_free_cdm_reader(mifi_cdm_reader* reader);

/**
 * Get a new reader from a felt file.
 * @param filename name of the felt-file
 * @param configFile configuration file for the felt-file
 * @return the reader object-pointer, use #mifi_freeCDMReader to free, or NULL on error.
 */
mifi_cdm_reader* mifi_new_felt_reader(const char* filename, const char* configFile);

/**
 * Get a new reader from a netcdf file.
 * @param filename name of the felt-file
 * @param configFile configuration file for the felt-file
 * @return the reader object-pointer, use #mifi_freeCDMReader to free, or NULL on error.
 */
mifi_cdm_reader* mifi_new_netcdf_reader(const char* filename);

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
