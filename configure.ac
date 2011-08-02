AC_INIT([fimex],[0.30beta],[heiko.klein@met.no])
# use POSIX tar to support longer filenames (created by doxygen)
AM_INIT_AUTOMAKE([1.9 tar-pax -Wall foreign])

AC_CONFIG_MACRO_DIR([m4])
AC_PROG_CC_STDC
AC_PROG_CXX
AC_PROG_LIBTOOL
AC_PROG_CC_C99

# on gcc, enable -Wall
AC_MSG_CHECKING(whether compiler understands -Wall -pedantic)
old_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -Wall"
AC_TRY_COMPILE([],[],AC_MSG_RESULT(yes),
                     AC_MSG_RESULT(no)
	                 CFLAGS="$old_CFLAGS")


# Checks for libraries.
AC_CXX_HAVE_STD
AC_CXX_HAVE_STL

AC_LANG_PUSH(C++)
# on g++, enable -Wall
AC_MSG_CHECKING(whether c++ compiler understands -Wall -pedantic)
old_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="$CXXFLAGS -Wall"
AC_TRY_COMPILE([],[],AC_MSG_RESULT(yes),
                     AC_MSG_RESULT(no)
	                 CXXFLAGS="$old_CXXFLAGS")

# Checks for header files.
AC_HEADER_STDC

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_STAT
AC_CHECK_FUNCS([memset pow sqrt strstr])
AC_CHECK_LIB([m],[cos])

# proj is a strong requirements
MIFI_REQLIB([proj], [pj_init])

MIFI_USELIB([udunits], [utInit])
if test [ "x$with_udunits" != "xyes" ]; then
    # check older udunits, at least one is required
    MIFI_USELIB([udunits2], [ut_new_system])
    if test [ "x$with_udunits2" != "xyes" ]; then
        # check for udunits2 with external expat
        MIFI_REQLIB([udunits2], [ut_new_system], [-lexpat])
    fi
    if test [ "x$with_udunits2" == "xyes" ]; then
        AC_DEFINE(HAVE_UDUNITS2,1,[Define if UDUNITS2 is enabled])
    fi
else
    AC_DEFINE(HAVE_UDUNITS,1,[Define if UDUNITS is enabled])
fi

AM_PATH_XML2(2.5.0,,AC_MSG_ERROR([*** Libxml is required to build fimex; Available at http://www.libxml.org/.]))

AX_BOOST_BASE([1.32.0])
AX_BOOST_REGEX
AX_BOOST_PROGRAM_OPTIONS
AX_BOOST_SYSTEM
AX_BOOST_FILESYSTEM
AX_BOOST_IOSTREAMS
AX_BOOST_DATE_TIME
AX_BOOST_UNIT_TEST_FRAMEWORK

# Make sure pkg-config also considers $prefix/lib/pkgconfig.
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:$prefix/lib/pkgconfig
PKG_PROG_PKG_CONFIG()

AC_LANG_PUSH(C++)
# Check compiler supporting openmp in C++ and C
AC_ARG_ENABLE(openmp, [  --enable-openmp         enable use of openmp [[default=no]]],[
  if test "$enableval" = yes; then
AX_OPENMP([AC_DEFINE(HAVE_OPENMP,1,[Define if OpenMP is enabled])
           CXXFLAGS="$CXXFLAGS $OPENMP_CXXFLAGS";CFLAGS="$CFLAGS $OPENMP_CXXFLAGS"],[])
  fi
], [])
AC_LANG_POP(C++)

# optional io libraries
MIFI_POSTGRES_CHECK
AM_CONDITIONAL([HAVE_LIBPQ], [test "x$with_pq" != xno])

MIFI_USELIB([netcdf], [nc_create])
if test [ "x$with_netcdf" == "xyes" ]; then
    # check also netcdf-cpp, can't use mifi_reqlib because of ++ (not allowed in m4)
    AC_LANG_PUSH(C++)
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    save_CPPFLAGS="$CPPFLAGS"
    MIFI_NETCDF_CPP_LIBS="-lnetcdf_c++"
    LIBS="$LIBS $MIFI_NETCDF_CPP_LIBS $MIFI_NETCDF_LIBS"
    LDFLAGS="$LDFLAGS $MIFI_NETCDF_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $MIFI_NETCDF_CPPFLAGS"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <netcdfcpp.h>], [NcError::set_err(5)])],
                   [],
                   [AC_MSG_ERROR([Did not find libnetcdf_c++, this is required to continue])])
    AC_CHECK_TYPE([NcFile::FileFormat],[  AC_DEFINE([HAVE_NCFILE_FILEFORMAT],[1],
                                                     [netcdf knows NcFile::FileFormat enum])
                                                    ],[],[#include "netcdfcpp.h"])
    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"
    AC_SUBST(MIFI_NETCDF_CPP_LIBS)
    AC_LANG_POP(C++)
    AC_DEFINE([MIFI_HAVE_NETCDF], 1, 'Define to 1 if you have the <netcdf.h> header file.')
fi
AM_CONDITIONAL([HAVE_NETCDF], [test "x$with_netcdf" == xyes])

# Check for the libmi / milib library.
AC_ARG_ENABLE(milib, [  --enable-milib          enable milib felt-reader [[default=no]]],[
  if test "$enableval" = yes; then
    use_milib=yes
  else
    use_milib=no
  fi
  ],[ use_milib=no ])
AM_CONDITIONAL([HAVE_MILIB], [test x$use_milib = xyes])

# Check for felt c++ library, currently always src included
AC_ARG_ENABLE(felt, [  --enable-felt           enable the c++ felt-reader [[default=yes]]],[
  if test "$enableval" = no; then
    use_felt=no
  else
    use_felt=yes
  fi
  ],[ use_felt=yes ])
AM_CONDITIONAL([HAVE_FELT], [test x$use_felt = xyes])
if test x$use_felt = xyes; then
    AC_DEFINE([HAVE_FELT], [1], [define if felt/FeltFile.h is available])
fi
AM_CONDITIONAL([HAVE_FELT_OR_MILIB], [test x$use_felt = xyes || test x$use_milib = xyes])

if test "$use_milib" = yes; then
# Check for the milib library.
if test "x$PKG_CONFIG" != "x"; then
   PKG_CHECK_MODULES([milib], [milib])
fi
METNO_CHECK_HEADERS([milib/milib.h], , , ,
                    [$milib_CFLAGS], [the milib package])

METNO_CHECK_LIB([mic], [mrfelt], , , , [$milib_LIBS],
                 [the milib package])
fi

# forcing jasper and png libraries on grib_api
# this might not be required for any build, but as long
# as grib_api does not provide a list over required libraries (e.g. pkgconfig, libtool)
# I will just try that
MIFI_USELIB([grib_api], [grib_set_string], [-ljasper -lpng])
AM_CONDITIONAL([HAVE_GRIBAPI], [test "x$with_grib_api" = "xyes"])
if test "x$with_grib_api" = "xyes"; then
    AC_DEFINE([HAVE_GRIBAPI_H], 1, 'Define to 1 if you have the <grib_api.h> header file.')
fi

MIFI_USELIB([metgm], [mgm_new_handle], [])
AM_CONDITIONAL([HAVE_METGM], [test "x$with_metgm" = "xyes"])
if test "x$with_metgm" = "xyes"; then
    AC_DEFINE([HAVE_METGM_H], 1, 'Define to 1 if you have the <metgm.h> header file.')
fi

AC_LANG_POP(C++)

DEBUG_MODE

#DX_INIT_DOXYGEN('Fimex', 'Doxyfile', 'share/doc')

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([fimex.pc
                          Doxyfile
                          Makefile
                          src/Makefile
                          src/felt/Makefile
                          src/binSrc/Makefile
                          test/Makefile
                          share/etc/Makefile
                          include/Makefile
                          include/fimex/Makefile
                          include/fimex/coordSys/Makefile
                          include/felt/Makefile
                          include/kdtree++/Makefile])
AC_OUTPUT


# ###########################################################
# Print Out Summary of Configuration
#
AC_MSG_RESULT([ ])
AC_MSG_RESULT([$PACKAGE_NAME $PACKAGE_VERSION])
AC_MSG_RESULT([Configuration:])
AC_MSG_RESULT([------------------------------------------------------------])
AC_MSG_RESULT([ ])
AC_MSG_RESULT([Source Code:      $srcdir])
AC_MSG_RESULT([Host System:      $host])
AC_MSG_RESULT([  ])
AC_MSG_RESULT([Prefix:           $prefix])
AC_MSG_RESULT([Binaries:         $exec_prefix])
AC_MSG_RESULT([Manuals:          $mandir])
AC_MSG_RESULT([Data:             $datadir])
AC_MSG_RESULT([System Config:    $sysconfdir])
AC_MSG_RESULT([ ])
AC_MSG_RESULT([CPPFLAGS:         $CPPFLAGS $XML_CPPFLAGS $milib_CFLAGS $MIFI_UDUNITS_CPPFLAGS $MIFI_UDUNITS2_CPPFLAGS $MIFI_METGM_CPPFLAGS $MIFI_PQ_CPPFLAGS $MIFI_PROJ_CPPFLAGS $MIFI_NETCDF_CPPFLAGS $BOOST_CPPFLAGS])
AC_MSG_RESULT([LDFLAGS:          $LDFLAGS $XML_LDFLAGS $MIFI_UDUNITS_LDFLAGS $MIFI_UDUNITS2_LDFLAGS $MIFI_METGM_LDFLAGS $MIFI_PQ_LDFLAGS $MIFI_PROJ_LDFLAGS $MIFI_NETCDF_LDFLAGS $MIFI_GRIB_API_LDFLAGS $BOOST_LDFLAGS])
AC_MSG_RESULT([LIBS:             $LIBS $XML_LIBS ${milib_LIBS} $MIFI_UDUNITS_LIBS $MIFI_UDUNITS2_LIBS $MIFI_METGM_LIBS $MIFI_PQ_LIBS $MIFI_PROJ_LIBS $MIFI_NETCDF_LIBS $MIFI_NETCDF_CPP_LIBS $MIFI_GRIB_API_LIBS $MIFI_GRIB_API_CPPFLAGS $BOOST_REGEX_LIB $BOOST_FILESYSTEM_LIB $BOOST_SYSTEM_LIB $BOOST_DATE_TIME_LIB $BOOST_IOSTREAMS_LIB $BOOST_PROGRAM_OPTIONS_LIB $pgsql_LIB])
AC_MSG_RESULT([DEFS:             $DEFS])
AC_MSG_RESULT([  ])
AC_MSG_RESULT([------------------------------------------------------------])
AC_MSG_RESULT([ ])

