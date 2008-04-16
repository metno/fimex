AC_INIT([utplukk], [0.2], [heiko.klein@met.no])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_PROG_CC_STDC
AC_PROG_CXX
#AC_PROG_RANLIB
AC_PROG_LIBTOOL

# C99 testing only available starting in autoconf 2.60
# AC_PROG_CC_C99

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


# Checks for libraries.
AC_CXX_HAVE_STD
AC_CXX_HAVE_STL

AM_PATH_XML2(2.5.0,,AC_MSG_ERROR([
*** Libxml is required to build utplukk; Available at
http://www.libxml.org/.]))
AX_BOOST_BASE([1.32.0])
AX_BOOST_REGEX
AX_BOOST_PROGRAM_OPTIONS
AX_BOOST_UNIT_TEST_FRAMEWORK

# Make sure pkg-config also considers $prefix/lib/pkgconfig.
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:$prefix/lib/pkgconfig

# Check for the libmi library.
AC_ARG_WITH([miLib],
            [AS_HELP_STRING([--with-miLib],
                            [use the mi library])],
            [case "${enableval}" in
               yes) use_milib=true ;;
               no)  use_milib=false ;;
               *) AC_MSG_ERROR([bad value ${enableval} for --with-miLib]) ;;
             esac], [use_milib=true])
AM_CONDITIONAL([HAVE_MILIB], [test x$use_milib = xtrue])

if test "$use_milib" = true; then
# Check for the milib library.
PKG_CHECK_MODULES([milib], [milib])
METNO_CHECK_HEADERS([milib/milib.h], , , ,
                    [$milib_CFLAGS], [the milib package])

METNO_CHECK_LIB([mic], [mrfelt], , , , [$milib_LIBS] -lg2c,
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

DX_INIT_DOXYGEN('Utplukk', 'Doxyfile', 'share/doc')

AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_FILES([utplukk.pc Makefile src/Makefile src/felt_reader/Makefile src/binSrc/Makefile test/Makefile])
AC_OUTPUT
