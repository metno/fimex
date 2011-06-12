############
# SYNOPSIS
#
#   MIFI_HAVE_PROJ([ACTION-IF-TRUE], [ACTION-IF-FALSE])
#
# DESCRIPTION
#
#   This macro will check for the existence of the metgmapi library.
#   The check is done by checking for the header file proj_api.h and
#   the metgmapi library object file.  A --with-metgmapi option is
#   supported as well. 
#
#   The C preprocessor symbol HAVE_METGMAPI_H will be
#   defined with AC_DEFINE([HAVE_METGMAPI_H]) if the library is
#   available.  Forthermore, the variable have_metgmapi will be set to
#   "yes" if the library is available.
#
# AUTHOR
#
#   Aleksandar Babic <aleksandarb@met.no>
############

# MIFI_HAVE_METGMAPI([ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ------------------------------------------------------
AC_DEFUN([MIFI_HAVE_METGMAPI], [
  AH_TEMPLATE([HAVE_METGMAPI], [Define if metgmapi is available])
    AC_LANG_PUSH(C)
    CPPFLAGS="$CPPFLAGS -I$METGMAPI_INCLUDES_DIR"
    AC_CHECK_HEADER([metgm.h],
                     [AC_DEFINE([HAVE_METGMAPI_H], [1],
                        [Define to 1 if you have <metgm.h>.])],
                     [AC_MSG_ERROR([metgm.h not found])])
    LDFLAGS="$LDFLAGS -L$METGMAPI_LIBS_DIR"
    AC_SEARCH_LIBS(mgm_new_handle,[metgm],
        [
         have_metgmapi=yes
        ],
                [AC_MSG_ERROR([-lmetgm not found])]
        , [])
     AC_LANG_POP(C)
])

AC_DEFUN([MIFI_HAVE_METGMAPI_CHECK],[
AC_ARG_WITH(metgmapi-lib-dir,
      [  --with-metgmapi-lib-dir=DIR      prefix for metgm library files],
            [if test "$withval" = "no"; then
               METGMAPI_LIBS_DIR=
             else
               METGMAPI_LIBS_DIR="$withval"
             fi], 
      [METGMAPI_LIBS_DIR=/usr/local/lib])

AC_ARG_WITH(metgmapi-include-dir,
      [  --with-metgmapi-include-dir=DIR      prefix for metgm headers],
            [if test "$withval" = "no"; then
               METGMAPI_INCLUDES_DIR=
             else
               METGMAPI_INCLUDES_DIR="$withval"
             fi], 
      [METGMAPI_INCLUDES_DIR=/usr/local/include])

]
)


