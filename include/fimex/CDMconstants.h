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
/* when changing, remember to update CDMconstants.cc#getFileTypeNames,
 * make sure that the maximum number is <= size of filetypes */
#define MIFI_FILETYPE_UNKNOWN -1
#define MIFI_FILETYPE_FELT   0
#define MIFI_FILETYPE_NETCDF 1
#define MIFI_FILETYPE_NCML   2
#define MIFI_FILETYPE_GRIB   3
#define MIFI_FILETYPE_WDB    4
#define MIFI_FILETYPE_METGM  5

/**
 * default fill values taken from netcdf.h
 */
#define MIFI_FILL_CHAR    ((signed char)-127)
#define MIFI_FILL_SHORT   ((short)-32767)
#define MIFI_FILL_INT     (-2147483647L)
#define MIFI_FILL_FLOAT   (9.9692099683868690e+36f) /* near 15 * 2^119 */
#define MIFI_FILL_DOUBLE  (9.9692099683868690e+36)
#define MIFI_FILL_UCHAR   (255)
#define MIFI_FILL_USHORT  (65535)
#define MIFI_FILL_UINT    (4294967295U)
#define MIFI_FILL_INT64   ((long long)-9223372036854775806LL)
#define MIFI_FILL_UINT64  ((unsigned long long)18446744073709551614ULL)


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
 * get the maximum number of filetypes, that is , the largest number
 * of valid filetype you can get.
 */
int mifi_get_max_filetype_number();

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
