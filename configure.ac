AC_INIT([fimex],[0.44],[heiko.klein@met.no])
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
MIFI_REQLIB([proj], [proj_api.h], [pj_init])

MIFI_USELIB([udunits2], [udunits2.h], [ut_new_system])
if test [ "x$with_udunits2" != "xyes" ]; then
    # check older udunits, at least one is required
    MIFI_USELIB([udunits], [udunits.h], [utInit])
    if test [ "x$with_udunits" != "xyes" ]; then
        # check for udunits2 with external expat
        MIFI_REQLIB([udunits2], [ut_new_system], [-lexpat])
    fi
fi

AM_PATH_XML2(2.5.0,,AC_MSG_ERROR([*** Libxml is required to build fimex; Available at http://www.libxml.org/.]))

AX_BOOST_BASE([1.32.0], [], [AC_MSG_ERROR([Boost libraries required!])])
AX_BOOST_REGEX
if test "x$ax_cv_boost_regex" != "xyes"; then
   AC_MSG_ERROR([Could not find libboost_regex, this is required!])
fi
AX_BOOST_PROGRAM_OPTIONS
if test "x$ax_cv_boost_program_options" != "xyes"; then
   AC_MSG_ERROR([Could not find libboost_program_options, this is required!])
fi
AX_BOOST_SYSTEM
AX_BOOST_FILESYSTEM
if test "x$ax_cv_boost_filesystem" != "xyes"; then
   AC_MSG_ERROR([Could not find libboost_filesystem, this is required!])
fi
AX_BOOST_DATE_TIME
if test "x$ax_cv_boost_date_time" != "xyes"; then
   AC_MSG_ERROR([Could not find libboost_date_time, this is required!])
fi
AX_BOOST_IOSTREAMS
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
AC_SUBST(OPENMP_CXXFLAGS)

#optional logging libraries
AC_ARG_ENABLE(log4cpp, [  --enable-log4cpp           enable logging via log4cpp [[default=no]]],[
if test "$enableval" = no; then
    use_log4cpp=no
  else
PKG_CHECK_MODULES([LOG4CPP],
                  [log4cpp],
                  AC_DEFINE([HAVE_LOG4CPP], [1], [define if log4cpp is available]),
                  AC_MSG_ERROR("unable to find requested log4cpp in pkg-config")
                 )
  fi
  ])

# optional io libraries
MIFI_USE_NETCDF()

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

# forcing jasper and png libraries on grib_api
# this might not be required for any build, but as long
# as grib_api does not provide a list over required libraries (e.g. pkgconfig, libtool)
# I will just try that
MIFI_USELIB([grib_api], [grib_api.h], [grib_set_string], [])
AM_CONDITIONAL([HAVE_GRIBAPI], [test "x$with_grib_api" = "xyes"])

MIFI_USELIB([pq], [libpq-fe.h], [PQconnectdb])
AM_CONDITIONAL([HAVE_LIBPQ], [test "x$with_pq" != xno])

MIFI_USELIB([metgm], [metgm.h], [mgm_read_group5_slice], [])
AM_CONDITIONAL([HAVE_METGM], [test "x$with_metgm" = "xyes"])

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
                 include/nanoflann/Makefile])
AC_CONFIG_FILES([test/testCharNc.sh],[chmod +x test/testCharNc.sh])
AC_CONFIG_FILES([test/testVerticalInterpolator.sh],[chmod +x test/testVerticalInterpolator.sh])
AC_CONFIG_FILES([test/testVerticalInterpolator2.sh],[chmod +x test/testVerticalInterpolator2.sh])
AC_CONFIG_FILES([test/testVerticalInterpolatorHeight.sh],[chmod +x test/testVerticalInterpolatorHeight.sh])
AC_CONFIG_FILES([test/testVerticalInterpolatorOceanSG2ToDepth.sh],[chmod +x test/testVerticalInterpolatorOceanSG2ToDepth.sh])
AC_CONFIG_FILES([test/testQEmask.xml])
AC_CONFIG_FILES([test/testQualityExtractorFimex.sh],[chmod +x test/testQualityExtractorFimex.sh])
AC_CONFIG_FILES([test/testMerger.sh],[chmod +x test/testMerger.sh])
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
AC_MSG_RESULT([CPPFLAGS:         $CPPFLAGS $XML_CPPFLAGS $MIFI_UDUNITS_CPPFLAGS $MIFI_UDUNITS2_CPPFLAGS $MIFI_METGM_CPPFLAGS $MIFI_PQ_CPPFLAGS $MIFI_PROJ_CPPFLAGS $BOOST_CPPFLAGS $LOG4CPP_CFLAGS])
AC_MSG_RESULT([LDFLAGS:          $OPENMP_CXXFLAGS $LDFLAGS $XML_LDFLAGS $MIFI_UDUNITS_LDFLAGS $MIFI_UDUNITS2_LDFLAGS $MIFI_METGM_LDFLAGS $MIFI_PQ_LDFLAGS $MIFI_PROJ_LDFLAGS $MIFI_NETCDF_LDFLAGS $MIFI_GRIB_API_LDFLAGS $BOOST_LDFLAGS])
AC_MSG_RESULT([LIBS:             $LIBS $XML_LIBS $MIFI_UDUNITS_LIBS $MIFI_UDUNITS2_LIBS $MIFI_METGM_LIBS $MIFI_PQ_LIBS $MIFI_PROJ_LIBS $MIFI_NETCDF_LIBS $MIFI_GRIB_API_LIBS $MIFI_GRIB_API_CPPFLAGS $BOOST_REGEX_LIB $BOOST_FILESYSTEM_LIB $BOOST_SYSTEM_LIB $BOOST_DATE_TIME_LIB $BOOST_IOSTREAMS_LIB $BOOST_PROGRAM_OPTIONS_LIB $pgsql_LIB $LOG4CPP_LIBS])
AC_MSG_RESULT([DEFS:             $DEFS])
AC_MSG_RESULT([  ])
AC_MSG_RESULT([------------------------------------------------------------])
AC_MSG_RESULT([ ])

