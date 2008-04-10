# autoconf macros for detecting NetCDF
#
# 20080214 MLS

AC_DEFUN([HAVE_NETCDF_CHECK],[
AC_ARG_WITH(NetCDF-lib-dir, 
      [  --with-NetCDF-lib-dir=DIR      prefix for NetCDF library files], 
            [if test "$withval" = "no"; then
               NC_LIBS_DIR=
             else
               NC_LIBS_DIR="$withval"
             fi], 
      [NC_LIBS_DIR=/usr/local/lib])

AC_ARG_WITH(NetCDF-include-dir, 
      [  --with-NetCDF-include-dir=DIR      prefix for NetCDF headers], 
            [if test "$withval" = "no"; then
               NC_INCLUDES_DIR=
             else
               NC_INCLUDES_DIR="$withval"
             fi], 
      [NC_INCLUDES_DIR=/usr/local/include])

AC_CHECKING([for NetCDF])

AC_MSG_RESULT([NetCDF lib location: $NC_LIBS_DIR])
AC_MSG_RESULT([NetCDF headers location: $NC_INCLUDES_DIR])

if eval "test ! -d $NC_INCLUDES_DIR"; then
    AC_MSG_ERROR([$NC_INCLUDES_DIR not found])
fi

if eval "test -d $NC_LIBS_DIR"; then
    NC_LDFLAGS="-L$NC_LIBS_DIR"
    NC_LIBS="-lnetcdf_c++ -lnetcdf"
else
    AC_MSG_ERROR([$NC_LIBS_DIR not found])
fi

save_CPPFLAGS="$CPPFLAGS"
save_LDFLAGS="$LDFLAGS"
save_LIBS="$LIBS"

CPPFLAGS="$CPPFLAGS -I$NC_INCLUDES_DIR"
LIBS="$LIBS $NC_LIBS"
LDFLAGS="$LDFLAGS $NC_LDFLAGS"

netcdf=no

AC_CHECKING([for netcdf.h])
AC_CHECK_HEADER([netcdf.h],
                [netcdf=yes], [netcdf=no])

AC_MSG_RESULT([NetCDF include result: $netcdf])
if test "x$netcdf" = xno; then
    AC_MSG_ERROR([NetCDF include not found])
fi

AC_LANG_CPLUSPLUS
AC_CHECKING([for libnetcdf_c++.a])
AC_CHECK_LIB([netcdf_c++], [main],
           [netcdf=yes], [netcdf=no])
AC_LANG_C

AC_MSG_RESULT([NetCDF link result: $netcdf])
if test "x$netcdf" = xno; then
    AC_MSG_ERROR([NetCDF library not found])
fi

CPPFLAGS="$save_CPPFLAGS"
LDFLAGS="$save_LDFLAGS"
LIBS="$save_LIBS"

AC_SUBST(NETCDF_CPPFLAGS, [-I$NC_INCLUDES_DIR])
AC_SUBST(NETCDF_LDFLAGS, [$NC_LDFLAGS])
AC_SUBST(NETCDF_LIBS, [$NC_LIBS])

])
