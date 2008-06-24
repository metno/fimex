############
# SYNOPSIS
#
#   MIFI_HAVE_PROJ([ACTION-IF-TRUE], [ACTION-IF-FALSE])
#
# DESCRIPTION
#
#   This macro will check for the existence of the proj4 library.
#   The check is done by checking for the header file proj_api.h and
#   the proj4 library object file.  A --with-proj4 option is
#   supported as well.  The following output variables are set with
#   AC_SUBST:
#
#     AC_SUBST(PROJ4_CPPFLAGS)
#     AC_SUBST(PROJ4_LDFLAGS)
#     AC_SUBST(PROJ4_LIBS)
#
#   You can use them like this in Makefile.am:
#
#     AM_CPPFLAGS = $(PROJ4_CPPFLAGS)
#     AM_LDFLAGS = $(PROJ4_LDFLAGS)
#     program_LDADD = $(PROJ4_LIBS)
#
#   Additionally, the C preprocessor symbol HAVE_PROJ4 will be
#   defined with AC_DEFINE([HAVE_PROJ4]) if the library is
#   available.  Forthermore, the variable have_proj4 will be set to
#   "yes" if the library is available.
#
# AUTHOR
#
#   Heiko Klein <Heiko.Klein@met.no>
############

# MIFI_HAVE_PROJ4([ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ------------------------------------------------------
AC_DEFUN([MIFI_HAVE_PROJ4], [
  AH_TEMPLATE([HAVE_PROJ4], [Define if proj4 is available])
    saved_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$CPPFLAGS -I$PROJ4_INCLUDES_DIR"
    AC_CHECK_HEADER([proj_api.h], [
      AC_LANG_PUSH(C)
      saved_LDFLAGS="$LDFLAGS"
      LDFLAGS="$LDFLAGS -L$PROJ4_LIBS_DIR"

      saved_LIBS="$LIBS"
      AC_CHECK_LIB([proj], [pj_free],
        [AC_SUBST(PROJ4_CPPFLAGS, [-I$PROJ4_INCLUDES_DIR])
         AC_SUBST(PROJ4_LDFLAGS, [-L$PROJ4_LIBS_DIR])
         AC_SUBST(PROJ4_LIBS, [-lproj])
         AC_DEFINE([HAVE_PROJ4])
         have_proj4=yes
         $1
        ], [
        :
        $2
        ])
      AC_LANG_POP(C)
      LIBS="$saved_LIBS"
      LDFLAGS="$saved_LDFLAGS"
    ], [
      :
      $2
    ])
    CPPFLAGS="$saved_CPPFLAGS"
])

AC_DEFUN([MIFI_HAVE_PROJ4_CHECK],[
AC_ARG_WITH(proj4-lib-dir, 
      [  --with-proj4-lib-dir=DIR      prefix for NetCDF library files], 
            [if test "$withval" = "no"; then
               PROJ4_LIBS_DIR=
             else
               PROJ4_LIBS_DIR="$withval"
             fi], 
      [PROJ4_LIBS_DIR=/usr/local/lib])

AC_ARG_WITH(proj4-include-dir, 
      [  --with-proj4-include-dir=DIR      prefix for NetCDF headers], 
            [if test "$withval" = "no"; then
               PROJ4_INCLUDES_DIR=
             else
               PROJ4_INCLUDES_DIR="$withval"
             fi], 
      [PROJ4_INCLUDES_DIR=/usr/local/include])

]
)


