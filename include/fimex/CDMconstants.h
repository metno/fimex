#ifndef CONSTANTS_H_
#define CONSTANTS_H_

/**
 * @headerfile "fimex/CDMconstants.h"
 */
#include "fimex/deprecated.h"

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
/* when changing, remember to update CDMconstants.cc#getFileTypeNames */
#define MIFI_FILETYPE_UNKNOWN -1
#define MIFI_FILETYPE_FELT   0
#define MIFI_FILETYPE_NETCDF 1
#define MIFI_FILETYPE_NCML   2
#define MIFI_FILETYPE_GRIB   3
#define MIFI_FILETYPE_WDB    4
#define MIFI_FILETYPE_METGM  5

#ifdef __cplusplus
extern "C" {
#endif

/**
 * version of fimex
 */
const char* fimexVersion();

/**
 * @brief get the filetype of a filetype name
 * @return one of MIFI_FILETYPE_*
 */
int mifi_get_filetype(const char* filetypeName);

/**
 * @brief get the filetype-name of a filetype
 * @param one of MIFI_FILETYPE_*
 */
const char* mifi_get_filetype_name(int filetype);

/**
 * check if fimex is configured with the filetype
 * @param fileType one of the MIFI_FILETYPE_* define constants
 */
int fimexHas(int fileType);
/**
 * check if fimex is configured with netcdf-support
 * @deprecated use fimexHas(fileType)
 */
DEPRECATED(int fimexHasNetcdf());
/**
 * check if fimex is configured with grib_api-support
 * @deprecated use fimexHas(fileType)
 */
DEPRECATED(int fimexHasGribApi());
/**
 * check if fimex is configured with felt-support
 * @deprecated use fimexHas(fileType)
 */
DEPRECATED(int fimexHasFelt());

#ifdef __cplusplus
}
#endif


#endif /*CONSTANTS_H_*/
