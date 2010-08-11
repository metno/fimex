############
# SYNOPSIS
#
#   MIFI_HAVE_PROJ([ACTION-IF-TRUE], [ACTION-IF-FALSE])
#
# DESCRIPTION
#
#   This macro will check for the existence of the gribapi library.
#   The check is done by checking for the header file proj_api.h and
#   the gribapi library object file.  A --with-gribapi option is
#   supported as well. 
#
#   The C preprocessor symbol HAVE_GRIBAPI_H will be
#   defined with AC_DEFINE([HAVE_GRIBAPI_H]) if the library is
#   available.  Forthermore, the variable have_gribapi will be set to
#   "yes" if the library is available.
#
# AUTHOR
#
#   Heiko Klein <Heiko.Klein@met.no>
############

# MIFI_HAVE_GRIBAPI([ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ------------------------------------------------------
AC_DEFUN([MIFI_HAVE_GRIBAPI], [
  AH_TEMPLATE([HAVE_GRIBAPI], [Define if gribapi is available])
    AC_LANG_PUSH(C)
    CPPFLAGS="$CPPFLAGS -I$GRIBAPI_INCLUDES_DIR"
    AC_CHECK_HEADER([grib_api.h],
                     [AC_DEFINE([HAVE_GRIBAPI_H], [1],
                        [Define to 1 if you have <grib_api.h>.])],
                     [AC_MSG_ERROR([grib_api.h not found])])
    LDFLAGS="$LDFLAGS -L$GRIBAPI_LIBS_DIR"
    AC_SEARCH_LIBS(grib_set_string,[grib_api],
        [
         have_gribapi=yes
        ],
		[AC_MSG_ERROR([-lgrib_api not found])]
        , [])
     AC_LANG_POP(C)
])

AC_DEFUN([MIFI_HAVE_GRIBAPI_CHECK],[
AC_ARG_WITH(gribapi-lib-dir, 
      [  --with-gribapi-lib-dir=DIR      prefix for grib_api library files], 
            [if test "$withval" = "no"; then
               GRIBAPI_LIBS_DIR=
             else
               GRIBAPI_LIBS_DIR="$withval"
             fi], 
      [GRIBAPI_LIBS_DIR=/usr/local/lib])

AC_ARG_WITH(gribapi-include-dir, 
      [  --with-gribapi-include-dir=DIR      prefix for grib_api headers], 
            [if test "$withval" = "no"; then
               GRIBAPI_INCLUDES_DIR=
             else
               GRIBAPI_INCLUDES_DIR="$withval"
             fi], 
      [GRIBAPI_INCLUDES_DIR=/usr/local/include])

]
)


