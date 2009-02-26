AC_INIT([fimex],[0.12beta],[heiko.klein@met.no])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_PROG_CC_STDC
AC_PROG_CXX
#AC_PROG_RANLIB
AC_PROG_LIBTOOL

# C99 testing only available starting in autoconf 2.60
AC_PROG_CC_C99

AC_ARG_WITH([NetCDF],
            [AS_HELP_STRING([--with-NetCDF],
                            [use the NetCDF library])],
            [case "${enableval}" in
               yes) use_netcdf=true ;;
               no)  use_netcdf=false ;;
               *) AC_MSG_ERROR([bad value ${enableval} for --with-NetCDF]) ;;
             esac], [use_netcdf=true])
AM_CONDITIONAL([HAVE_NETCDF], [test x$use_netcdf = xtrue])
AC_ARG_WITH(udunits,
            [  --with-udunits=DIR      prefix for Unidata units (udunits) library files and headers],
            [if test "$withval" = "no"; then
               ac_udunits_path=
               $2
             elif test "$withval" = "yes"; then
               ac_udunits_path=/usr
             else
               ac_udunits_path="$withval"
             fi],
            [ac_udunits_path=/usr])
if test "$use_netcdf" = true; then
    HAVE_NETCDF_CHECK()
    AC_DEFINE([HAVE_NETCDF], [1], [have netcdf])
    METNO_HAVE_UDUNITS( , [AC_MSG_FAILURE([udunits not found. Please make sure that udunits is properly installed when compiling with NetCDF.]) ])
fi
AM_CONDITIONAL([HAVE_UDUNITS], [test x$have_udunits = xyes])

# proj is a strong requirements
MIFI_HAVE_PROJ4_CHECK()
MIFI_HAVE_PROJ4( , [AC_MSG_FAILURE([proj4 not found. Please make sure that proj4 is installed.]) ])
AM_CONDITIONAL([HAVE_PROJ4], [test x$have_proj4 = xyes])

AC_ARG_WITH(gribapi,
      [AS_HELP_STRING([--with-gribapi],[use grib_api library])],
         [if test "$withval" != "no"; then
             MIFI_HAVE_GRIBAPI_CHECK()
             MIFI_HAVE_GRIBAPI( , [AC_MSG_FAILURE([grib-api not found. Please make sure that grib-api is installed.]) ])
          fi],[])
AM_CONDITIONAL([HAVE_GRIBAPI], [test x$have_gribapi = xyes])


# Checks for libraries.
AC_CXX_HAVE_STD
AC_CXX_HAVE_STL

AM_PATH_XML2(2.5.0,,AC_MSG_ERROR([
*** Libxml is required to build fimex; Available at
http://www.libxml.org/.]))
AX_BOOST_BASE([1.32.0])
AX_BOOST_REGEX
AX_BOOST_PROGRAM_OPTIONS
AX_BOOST_UNIT_TEST_FRAMEWORK

# Make sure pkg-config also considers $prefix/lib/pkgconfig.
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:$prefix/lib/pkgconfig

AC_LANG_PUSH(C++)
# Check compiler supporting openmp in C++
AX_OPENMP([AC_DEFINE(HAVE_OPENMP,1,[Define if OpenMP is enabled])
           CXXFLAGS="$CXXFLAGS $OPENMP_CXXFLAGS"],[])
AC_LANG_POP(C++)

# Check for the libmi library.
AC_ARG_ENABLE(felt, [  --enable-felt           enable felt-reader [[default=yes]]],[
  if test "$enableval" = no; then
    use_milib=no
  else
    use_milib=yes
  fi
  ],[ use_milib=yes ])
AM_CONDITIONAL([HAVE_MILIB], [test x$use_milib = xyes])

if test "$use_milib" = yes; then
# Check for the milib library.
PKG_CHECK_MODULES([milib], [milib])
METNO_CHECK_HEADERS([milib/milib.h], , , ,
                    [$milib_CFLAGS], [the milib package])

METNO_CHECK_LIB([mic], [mrfelt], , , , [$milib_LIBS],
                 [the milib package])
fi


AC_LANG_PUSH(C++)

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

AC_LANG_POP(C++)

#DX_INIT_DOXYGEN('Fimex', 'Doxyfile', 'share/doc')

AC_CONFIG_HEADERS([include/fimex/config.h])
AC_CONFIG_FILES([fimex.pc Makefile src/Makefile src/felt_reader/Makefile src/binSrc/Makefile test/Makefile share/etc/Makefile include/Makefile include/fimex/Makefile])
AC_OUTPUT
