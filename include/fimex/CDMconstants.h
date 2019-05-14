/*
  Fimex, include/fimex/CDMconstants.h

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/

#ifndef CDMCONSTANTS_H_
#define CDMCONSTANTS_H_

/**
 * @headerfile fimex/CDMconstants.h
 */
/**
 * @brief constants used through-out fimex
 *
 * CDMConstants stores several constants used in fimex, accessible from
 * C and C++. Constants are either available as macro, or as function.
 */

#define MIFI_STRINGIFY1(x) #x
#define MIFI_STRINGIFY(x) MIFI_STRINGIFY1(x)

/*
 * The following numbers are used by configure.ac
 * - make sure the line-number is correct
 */
/*
 * fimex version status, e.g >=0xF0 = final, 0xAX = alphaX,
 * 0xBX = betaX, 0xCX= releaseCandidateX
 * touch configure.ac after changing these numbers
 */
#define MIFI_VERSION_MAJOR  1
#define MIFI_VERSION_MINOR  1
#define MIFI_VERSION_PATCH  0
#define MIFI_VERSION_STATUS 0xF0

#define MIFI_VERSION_PATCH_STRING "." MIFI_STRINGIFY(MIFI_VERSION_PATCH)

#if (MIFI_VERSION_STATUS == 0xA1)
#define MIFI_VERSION_STATUS_STRING "~alpha1"
#elif (MIFI_VERSION_STATUS == 0xA2)
#define MIFI_VERSION_STATUS_STRING "~alpha2"
#elif (MIFI_VERSION_STATUS == 0xA3)
#define MIFI_VERSION_STATUS_STRING "~alpha3"
#elif (MIFI_VERSION_STATUS == 0xA4)
#define MIFI_VERSION_STATUS_STRING "~alpha4"
#elif (MIFI_VERSION_STATUS == 0xA5)
#define MIFI_VERSION_STATUS_STRING "~alpha5"
#elif (MIFI_VERSION_STATUS == 0xA6)
#define MIFI_VERSION_STATUS_STRING "~alpha6"
#elif (MIFI_VERSION_STATUS == 0xA7)
#define MIFI_VERSION_STATUS_STRING "~alpha7"
#elif (MIFI_VERSION_STATUS == 0xA8)
#define MIFI_VERSION_STATUS_STRING "~alpha8"
#elif (MIFI_VERSION_STATUS == 0xA9)
#define MIFI_VERSION_STATUS_STRING "~alpha9"

#elif (MIFI_VERSION_STATUS == 0xB1)
#define MIFI_VERSION_STATUS_STRING "~beta1"
#elif (MIFI_VERSION_STATUS == 0xB2)
#define MIFI_VERSION_STATUS_STRING "~beta2"
#elif (MIFI_VERSION_STATUS == 0xB3)
#define MIFI_VERSION_STATUS_STRING "~beta3"
#elif (MIFI_VERSION_STATUS == 0xB4)
#define MIFI_VERSION_STATUS_STRING "~beta4"
#elif (MIFI_VERSION_STATUS == 0xB5)
#define MIFI_VERSION_STATUS_STRING "~beta5"
#elif (MIFI_VERSION_STATUS == 0xB6)
#define MIFI_VERSION_STATUS_STRING "~beta6"
#elif (MIFI_VERSION_STATUS == 0xB7)
#define MIFI_VERSION_STATUS_STRING "~beta7"
#elif (MIFI_VERSION_STATUS == 0xB8)
#define MIFI_VERSION_STATUS_STRING "~beta8"
#elif (MIFI_VERSION_STATUS == 0xB9)
#define MIFI_VERSION_STATUS_STRING "~beta9"

#elif (MIFI_VERSION_STATUS == 0xC1)
#define MIFI_VERSION_STATUS_STRING "~rc1"
#elif (MIFI_VERSION_STATUS == 0xC2)
#define MIFI_VERSION_STATUS_STRING "~rc2"
#elif (MIFI_VERSION_STATUS == 0xC3)
#define MIFI_VERSION_STATUS_STRING "~rc3"
#elif (MIFI_VERSION_STATUS == 0xC4)
#define MIFI_VERSION_STATUS_STRING "~rc4"
#elif (MIFI_VERSION_STATUS == 0xC5)
#define MIFI_VERSION_STATUS_STRING "~rc5"
#elif (MIFI_VERSION_STATUS == 0xC6)
#define MIFI_VERSION_STATUS_STRING "~rc6"
#elif (MIFI_VERSION_STATUS == 0xC7)
#define MIFI_VERSION_STATUS_STRING "~rc7"
#elif (MIFI_VERSION_STATUS == 0xC8)
#define MIFI_VERSION_STATUS_STRING "~rc8"
#elif (MIFI_VERSION_STATUS == 0xC9)
#define MIFI_VERSION_STATUS_STRING "~rc9"

#elif (MIFI_VERSION_STATUS == 0xF0)
#define MIFI_VERSION_STATUS_STRING ""

#else
#error "unknown MIFI_VERSION_STATUS"
#endif

#define MIFI_VERSION_STRING \
    MIFI_STRINGIFY(MIFI_VERSION_MAJOR )\
    "." MIFI_STRINGIFY(MIFI_VERSION_MINOR) \
    MIFI_VERSION_PATCH_STRING \
    MIFI_VERSION_STATUS_STRING

#define MIFI_VERSION_INT(major,minor,patch) \
    (1000000*major + 1000*minor + patch)
#define MIFI_VERSION_CURRENT_INT \
    MIFI_VERSION_INT(MIFI_VERSION_MAJOR, MIFI_VERSION_MINOR, MIFI_VERSION_PATCH)

/**
 * the default radius of a spherical earth in meter
 */
#define MIFI_EARTH_RADIUS_M 6371000

/**
 * the default earth projection (WGS84 latlong)
 */
#define MIFI_WGS84_LATLON_PROJ4 "+proj=latlong +datum=WGS84 +towgs84=0,0,0 +no_defs"

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
extern const char* fimexVersion();

/**
 * major version of fimex
 */
extern unsigned int mifi_version_major();

/**
 * minor version of fimex
 */
extern unsigned int mifi_version_minor();

/**
 * patch version of fimex
 */
extern unsigned int mifi_version_patch();

/**
 * fimex version status, e.g >=0xF0 = final, 0xAX = alphaX,
 * 0xBX = betaX, 0xCX= releaseCandidateX
 */
extern unsigned int mifi_version_status();

#ifdef __cplusplus
}
#endif


#endif /*CDMCONSTANTS_H_*/
