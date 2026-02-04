/*
 * Fimex, test_c_consumer.c
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

#include "fimex/c_fimex.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int testFeltRead(const char* feltFile, const char* configFile) {
    int retVal = 0;

    mifi_cdm_reader* feltReader = mifi_new_io_reader("felt", feltFile, configFile);
    if (feltReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading felt-file\n");
    }
    mifi_free_cdm_reader(feltReader);
    return retVal;
}

int testFeltVariables(const char* feltFile, const char* configFile) {
    int retVal = 0;

    mifi_cdm_reader* feltReader = mifi_new_io_reader("felt", feltFile, configFile);
    if (feltReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading felt-file\n");
    }
    size_t varCount = mifi_get_variable_number(feltReader);
    int found = 0;
    for (size_t i=0; i < varCount; i++) {
        const char* varName = mifi_get_variable_name(feltReader, i);
        if (varName == 0) {
            fprintf(stderr, "unable to read variable number %lu\n", i);
            retVal++;
        } else {
            //printf("found variable: %s\n", varName);
            if ('x' ==  varName[0]) {
                found++;
            }
        }
    }
    if (found == 0) {
        fprintf(stderr, "cannot find variable starting with 'x'\n");
        retVal++;
    }
    mifi_free_cdm_reader(feltReader);
    return retVal;
}

int testFeltData(const char* feltFile, const char* configFile) {
    int retVal = 0;

    mifi_cdm_reader* feltReader = mifi_new_io_reader("felt", feltFile, configFile);
    if (feltReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading felt-file\n");
    }

    {
        // read dataslice
        double *data = NULL;
        size_t size = 0;
        if (mifi_get_double_dataslice(feltReader, "sea_surface_temperature", 0,
                &data, &size) != 0) {
            retVal++;
            fprintf(stderr, "error reading data\n");
        } else {
            if (size == 0) {
                retVal++;
                fprintf(stderr, "error: datasize = 0\n");
            }
            if (data == NULL) {
                retVal++;
                fprintf(stderr, "error: no data allocated\n");
            } else {
                for (size_t i = 0; i < size; ++i) {
                    // check accessibility of data, might segfault on error
                    double x = data[i];
                    x++;
                }
                free(data);
            }
        }
    }

    {
        // read all data for axis x
        double *data = NULL;
        size_t size = 0;
        if (mifi_get_double_data(feltReader, "x", &data, &size) != 0) {
            retVal++;
            fprintf(stderr, "error reading data\n");
        } else {
            if (size != 229) {
                retVal++;
                fprintf(stderr, "error: datasize != 229\n");
            }
            if (data == NULL) {
                retVal++;
                fprintf(stderr, "error: no data allocated\n");
            } else {
                for (size_t i = 0; i < size; ++i) {
                    // check accessibility of data, might segfault on error
                    double x = data[i];
                    x++;
                    //printf("i: %f\n", data[i]);
                }
                if (fabs(data[0]+5718494.) > 1) {
                    retVal++;
                    fprintf(stderr, "data[0] != -5718494: %f", data[0]);
                }
                if (fabs(data[size - 1]-5718494.) > 1) {
                    retVal++;
                    fprintf(stderr, "data[size -1] != 5718494: %f", data[size - 1]);
                }
                free(data);
            }
        }
    }

    mifi_free_cdm_reader(feltReader);
    return retVal;
}

static const char from_felt_nc[] = "test_c_fromfelt.nc";

int testFeltReadNetcdfWrite(const char* feltFile, const char* configFile) {
    int retVal = 0;

    mifi_cdm_reader* feltReader = mifi_new_io_reader("felt", feltFile, configFile);
    if (feltReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading felt-file\n");
    }

    if (mifi_writer(feltReader, "netcdf", from_felt_nc, 0) != 0) {
        retVal++;
        fprintf(stderr, "error in writing netcdf-file %s\n", from_felt_nc);
    }

    mifi_free_cdm_reader(feltReader);
    return retVal;
}

int testNetcdfReadNetcdfWrite()
{
    int retVal = 0;
    const char w_filename[] = "test_c_NetcdfReadNetcdfWrite.nc";

    mifi_cdm_reader* ncReader = mifi_new_io_reader("netcdf", from_felt_nc, "");
    if (ncReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading nc-file %s\n", from_felt_nc);
    }

    if (mifi_writer(ncReader, "netcdf", w_filename, 0) != 0) {
        retVal++;
        fprintf(stderr, "error in writing netcdf-file %s\n", w_filename);
    }

    mifi_free_cdm_reader(ncReader);
    remove(w_filename);
    return retVal;
}

int doubleCallback(mifi_cdm_reader* reader, const char* varName, size_t unLimDimPos, double* scaledData, size_t dataSize)
{
    (void)reader;
    (void)varName;
    (void)unLimDimPos;
    (void)dataSize;

    for (size_t i = 0; i < dataSize; ++i) {
        *scaledData++ = (double) i;
    }
    return 0;
}

int testCReader(const char* feltFile, const char* configFile) {
    int retVal = 0;
    const char w_filename[] = "test_c_reader.nc";

    mifi_cdm_reader* feltReader = mifi_new_io_reader("felt", feltFile, configFile);
    if (feltReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading felt-file\n");
    }

    mifi_cdm_reader *cReader = mifi_new_c_reader(feltReader);
    if (mifi_set_callback_double(cReader, "land_ice_area_fraction", &doubleCallback) != 0) {
        retVal++;
        fprintf(stderr, "error in setting a callback");
    }

    if (mifi_writer(cReader, "netcdf", w_filename, 0) != 0) {
        retVal++;
        fprintf(stderr, "error in writing netcdf-file '%s'\n", w_filename);
    }

    mifi_free_cdm_reader(cReader);
    mifi_free_cdm_reader(feltReader);
    remove(w_filename);
    return retVal;
}

int testCSliceBuilder(const char* feltFile, const char* configFile) {
    int retVal = 0;

    mifi_cdm_reader* feltReader = mifi_new_io_reader("felt", feltFile, configFile);
    if (feltReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading felt-file\n");
    }

    mifi_slicebuilder *sb = mifi_new_slicebuilder(feltReader, "sea_surface_temperature");
    if (sb == NULL) {
        retVal++;
        fprintf(stderr, "error getting sb\n");
    }

    int ndims = mifi_slicebuilder_ndims(sb);
    if (ndims != 2) {
        retVal++;
        fprintf(stderr, "wrong number of ndims, 2 != %d", ndims);
    }

    size_t totDims = mifi_get_dimension_number(feltReader);
    if (totDims != 4) {
        retVal++;
        fprintf(stderr, "wrong number of totDims, 4 != %lu\n", totDims);
    }

    const char* dimName = mifi_get_dimension_name(feltReader, 2);
    if (strcmp("x", dimName)) {
        retVal++;
        fprintf(stderr, "dimension 2 not named right, x != %s\n", dimName);
    }

    size_t dSize = mifi_get_dimension_size(feltReader, dimName);
    if (dSize != 229) {
        retVal++;
        fprintf(stderr, "dimension %s has size %lu != 229\n", dimName, dSize);
    }

    const char* undimName = mifi_get_unlimited_dimension_name(feltReader);
    if (strcmp("time", undimName)) {
        retVal++;
        fprintf(stderr, "unlimited not named right, time != %s\n", undimName);
    }

    dSize = mifi_get_dimension_size(feltReader, undimName);
    if (dSize != 61) {
        retVal++;
        fprintf(stderr, "unlimited dimension %s has size %lu != 61\n", undimName, dSize);
    }


    unsigned int type = mifi_get_variable_type(feltReader, "precipitation_amount");
    if (type != 4) { // 4=CDM_FLOAT
        retVal++;
        fprintf(stderr, "variable type of precipitation_amount is %d != 4\n", type);
    }

    type = mifi_get_variable_type(feltReader, "altitude");
    if (type != 2) { // 2=CDM_SHORT
        retVal++;
        fprintf(stderr, "variable type of altitude is %d != 2\n", type);
    }

    type = mifi_get_variable_type(feltReader, "lasdfj");
    if (type != 0) { // 0=CDM_NAN
        retVal++;
        fprintf(stderr, "variable type of unknown variable is %d != 0\n", type);
    }


    char* lonName = mifi_get_var_longitude(feltReader, "altitude");
    if (!lonName) {
        retVal++;
        fprintf(stderr, "longitude not found\n");
    } else if (strcmp("longitude", lonName)) {
        retVal++;
        fprintf(stderr, "longitude not named right, longitude != %s\n", lonName);
        free(lonName);
    }

    const char* latName = mifi_get_var_latitude(feltReader, "altitude");
        if (!latName) {
        retVal++;
        fprintf(stderr, "latitude not found\n");
    } else if (strcmp("latitude", latName)) {
        retVal++;
        fprintf(stderr, "latitude not named right, latitude != %s\n", latName);
        free(lonName);
    }


    return retVal;
}


int main(int argc, char* argv[])
{
    int tests = 7;

    // setup
    char* feltFile = (char*) calloc(1024, sizeof(char));
    strcat(feltFile, TEST_EXTRADATA_DIR);
    strcat(feltFile, "/flth00.dat");
    char* configFile = (char*) calloc(1024, sizeof(char));
    strcat(configFile, TOP_SRCDIR);
    strcat(configFile, "/share/etc/felt2nc_variables.xml");

    // check if file exists
    FILE *fh = fopen(feltFile, "rb");
    if (fh == NULL) {
        fprintf(stderr, "\n*** Skipping tests as optional test data file '%s' is not found\n", feltFile);
        tests = 0;
    } else {
        fclose(fh);
    }

    int retVal = tests;
    printf("Running %d test cases...\n", tests);
    if (tests == 0) {
        exit(0);
    }

    // the tests
    if (testFeltRead(feltFile, configFile) == 0) retVal--;
    if (testFeltVariables(feltFile, configFile) == 0) retVal--;
    if (testFeltData(feltFile, configFile) == 0) retVal--;
    if (testFeltReadNetcdfWrite(feltFile, configFile) == 0) retVal--;
    if (testNetcdfReadNetcdfWrite() == 0) retVal--;
    if (testCReader(feltFile, configFile) == 0) retVal--;
    if (testCSliceBuilder(feltFile, configFile) == 0) retVal--;

    remove(from_felt_nc);

    if (retVal == 0) {
        fprintf(stderr, "\n*** No errors detected\n");
    } else {
        fprintf(stderr, "\n*** %d errors detected\n", retVal);
    }
    exit(retVal);
}
