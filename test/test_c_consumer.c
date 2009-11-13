/*
 * Fimex, test_c_consumer.c
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

#include "fimex/c_fimex.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int testFeltRead(const char* feltFile, const char* configFile) {
    int retVal = 0;

    mifi_cdm_reader *feltReader = mifi_new_felt_reader(feltFile, configFile);
    if (feltReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading felt-file\n");
    }
    mifi_free_cdm_reader(feltReader);
    return retVal;
}

int testFeltVariables(const char* feltFile, const char* configFile) {
    int retVal = 0;

    mifi_cdm_reader *feltReader = mifi_new_felt_reader(feltFile, configFile);
    if (feltReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading felt-file\n");
    }
    int varCount = mifi_get_variable_number(feltReader);
    int found = 0;
    for (int i=0; i < varCount; i++) {
        const char* varName = mifi_get_variable_name(feltReader, i);
        if (varName == 0) {
            fprintf(stderr, "unable to read variable number %d\n", i);
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

    mifi_cdm_reader *feltReader = mifi_new_felt_reader(feltFile, configFile);
    if (feltReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading felt-file\n");
    }

    {
        // read dataslice
        double *data = NULL;
        size_t size = 0;
        if (mifi_get_double_dataslize(feltReader, "sea_surface_temperature", 0,
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
                for (int i = 0; i < size; ++i) {
                    // check accessibility of data, might segfault on error
                    double x = data[i];
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
                for (int i = 0; i < size; ++i) {
                    // check accessibility of data, might segfault on error
                    double x = data[i];
                    //printf("i: %f\n", data[i]);
                }
                if (fabs(data[0]+5718494.) > 0.e-5) {
                    retVal++;
                    fprintf(stderr, "data[0] != -5718494: %f", data[0]);
                }
                if (fabs(data[size - 1]-5718494.) > 0.e-5) {
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

int testFeltReadNetcdfWrite(const char* feltFile, const char* configFile) {
    int retVal = 0;

    mifi_cdm_reader *feltReader = mifi_new_felt_reader(feltFile, configFile);
    if (feltReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading felt-file\n");
    }

    if (mifi_netcdf_writer(feltReader, "test.nc",0,3) != 0) {
        retVal++;
        fprintf(stderr, "error in writing netcdf-file test.nc\n");
    }

    mifi_free_cdm_reader(feltReader);
    return retVal;
}

int testNetcdfReadNetcdfWrite() {
    int retVal = 0;

    mifi_cdm_reader *ncReader = mifi_new_netcdf_reader("test.nc");
    if (ncReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading nc-file test.nc\n");
    }

    if (mifi_netcdf_writer(ncReader, "test2.nc",0,3) != 0) {
        retVal++;
        fprintf(stderr, "error in writing netcdf-file test2.nc\n");
    }

    mifi_free_cdm_reader(ncReader);
    return retVal;
}

int doubleCallback(mifi_cdm_reader* reader, const char* varName, size_t unLimDimPos, double* scaledData, size_t dataSize)
{
    for (int i = 0; i < dataSize; ++i) {
        *scaledData++ = (double) i;
    }
    return 0;
}

int testCReader(const char* feltFile, const char* configFile) {
    int retVal = 0;

    mifi_cdm_reader *feltReader = mifi_new_felt_reader(feltFile, configFile);
    if (feltReader == NULL) {
        retVal++;
        fprintf(stderr, "error reading felt-file\n");
    }

    mifi_cdm_reader *cReader = mifi_new_c_reader(feltReader);
    if (mifi_set_callback_double(cReader, "land_ice_area_fraction", &doubleCallback) != 0) {
        retVal++;
        fprintf(stderr, "error in setting a callback");
    }

    if (mifi_netcdf_writer(cReader, "test.nc",0,3) != 0) {
        retVal++;
        fprintf(stderr, "error in writing netcdf-file test.nc\n");
    }

    mifi_free_cdm_reader(cReader);
    mifi_free_cdm_reader(feltReader);
    return retVal;
}


int main(int argc, char* argv[])
{
    int tests = 6;
    int retVal = tests;

    // setup
    char* feltFile = (char*) calloc(1024, sizeof(char));
    strcat(feltFile, TOP_SRCDIR);
    strcat(feltFile, "/test/flth00.dat");
    char* configFile = (char*) calloc(1024, sizeof(char));
    strcat(configFile, TOP_SRCDIR);
    strcat(configFile, "/share/etc/felt2nc_variables.xml");

    // check if file exists
    FILE *fh = fopen(feltFile, "r");
    if (fh == NULL) {
        tests = 0;
    } else {
        fclose(fh);
    }

    printf("Running %d test cases...\n", tests);
    if (tests == 0) {
        exit(retVal);
    }

    // the tests
    if (testFeltRead(feltFile, configFile) == 0) retVal--;
    if (testFeltVariables(feltFile, configFile) == 0) retVal--;
    if (testFeltData(feltFile, configFile) == 0) retVal--;
    if (testFeltReadNetcdfWrite(feltFile, configFile) == 0) retVal--;
    if (testNetcdfReadNetcdfWrite(feltFile, configFile) == 0) retVal--;
    if (testCReader(feltFile, configFile) == 0) retVal--;


    if (retVal == 0) {
        fprintf(stderr, "\n*** No errors detected\n");
    } else {
        fprintf(stderr, "\n*** %d errors detected\n", retVal);
    }
    exit(retVal);
}
