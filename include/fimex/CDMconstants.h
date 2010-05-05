#ifndef CONSTANTS_H_
#define CONSTANTS_H_

/**
 * @headerfile "fimex/CDMconstants.h"
 */

/**
 * @brief constants used through-out fimex
 *
 * CDMConstants stores several constants used in fimex, accessible from
 * C and C++. Constants are either available as macro, or as function.
 */

/**
 * the default radius of a sperical earth in meter
 */
#define MIFI_EARTH_RADIUS_M 6371000

/**
 * The MIFI_FILETYPE_* define the available input and output file-formats
 */
#define MIFI_FILETYPE_UNKNOWN -1
#define MIFI_FILETYPE_FELT   0
#define MIFI_FILETYPE_NETCDF 1
#define MIFI_FILETYPE_NCML   2
#define MIFI_FILETYPE_GRIB   3

#ifdef __cplusplus
extern "C" {
#endif

/**
 * version of fimex
 */
const char* fimexVersion();

/**
 * check if fimex is configured with netcdf-support
 */
int fimexHasNetcdf();
/**
 * check if fimex is configured with grib_api-support
 */
int fimexHasGribApi();
/**
 * check if fimex is configured with felt-support
 */
int fimexHasFelt();

#ifdef __cplusplus
}
#endif


#endif /*CONSTANTS_H_*/
