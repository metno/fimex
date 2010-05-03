############
# SYNOPSIS
#
#   MIFI_REQLIB([LIBNAME],[FUNCTIONNAME])
#   MIFI_USELIB([LIBNAME],[FUNCTIONNAME])
#
# DESCRIPTION
#
#   These macro will check for the existence of a library.
#   The library will be added by a --with-LIBNAME option.
#   Libraries and Includes will be added by --with-LIBNAME=DIR or --with-LIBNAME=LIB/INCLUDE
# 
#   With 'REQLIB' configure will stop when the the library cannot be found. 'USELIB' will continue.
#  
#   The following output variables are set with AC_SUBST:
#
#     AC_SUBST(MIFI_LIBNAME_CPPFLAGS)
#     AC_SUBST(MIFI_LIBNAME_LDFLAGS)
#     AC_SUBST(MIFI_LIBNAME_LIBS)
#
#   Libs and includes will be automatically added to the CPPFLAGS, LDFLAGS and LIBS.
#
#
#   with_$libname will be set to yes if AC_CHECK_LIB works.
#
# AUTHOR
#
#   Heiko Klein <Heiko.Klein@met.no>
#   derived from work by Øystein Godøy and Thomas Lavergne
############

AC_DEFUN([MIFI_REQLIB],[
# Checks for libraries that are required.
saved_CPPFLAGS="$CPPFLAGS"
saved_LDFLAGS="$LDFLAGS"
saved_LIBS="$LIBS"
AC_ARG_WITH([$1],
    AC_HELP_STRING([--with-$1=DIR],
    [the location of mandatory lib$1 files and library either as DIR or INC,LIB]),
    ,
    [with_$1=yes])
case $with_$1 in
    yes)
     echo "Using system implementation of lib$1"
      LIBS="$LIBS -l$1"
     ;;
    no)
    AC_MSG_ERROR([lib$1 is required])
     ;;
    *,*)
      addincdir="`echo $with_$1 | cut -f1 -d,`"
      addlibdir="`echo $with_$1 | cut -f2 -d,`"
      CPPFLAGS="$CPPFLAGS -I$addincdir"
      LDFLAGS="$LDFLAGS -L$addlibdir"
      LIBS="$LIBS -l$1"
      ;;
    *)
      addincdir="$with_$1/include"
      addlibdir="$with_$1/lib"
      CPPFLAGS="$CPPFLAGS -I$addincdir"
      LDFLAGS="$LDFLAGS -L$addlibdir"
      LIBS="$LIBS -l$1"
      ;;
esac
mifi_have_feat=m4_toupper(MIFI_HAVE_LIB$1)
if test [ x$with_$1 != xno]; then
    AC_CHECK_LIB([$1],[$2], [with_$1=yes],
        [CPPFLAGS="$saved_CPPFLAGS";LDFLAGS="$saved_LDFLAGS";
         LIBS="$saved_LIBS";with_$1=no;
         AC_MSG_ERROR([Did not find lib$1, this is required to continue])])
fi
])

AC_DEFUN([MIFI_USELIB],[
# Checks for libraries that could be dropped.
saved_CPPFLAGS="$CPPFLAGS"
saved_LDFLAGS="$LDFLAGS"
saved_LIBS="$LIBS"
AC_ARG_WITH([$1],
    AC_HELP_STRING([--with-$1=DIR],
    [the location of optional lib$1 files and library either as DIR or INC,LIB]),
    ,
    [with_$1=yes])
case $with_$1 in
    yes)
     echo "Using system implementation of lib$1"
      LIBS="$LIBS -l$1"
     ;;
    no)
    AC_MSG_WARN([Building library with lib$1 dependent functions disabled])
     ;;
    *,*)
      addincdir="`echo $with_$1 | cut -f1 -d,`"
      addlibdir="`echo $with_$1 | cut -f2 -d,`"
      CPPFLAGS="$CPPFLAGS -I$addincdir"
      LDFLAGS="$LDFLAGS -L$addlibdir"
      LIBS="$LIBS -l$1"
      ;;
    *)
      addincdir="$with_$1/include"
      addlibdir="$with_$1/lib"
      CPPFLAGS="$CPPFLAGS -I$addincdir"
      LDFLAGS="$LDFLAGS -L$addlibdir"
      LIBS="$LIBS -l$1"
      ;;
esac
if test [ "x$with_$1" != "xno"]; then
    mifi_have_feat=m4_toupper(MIFI_HAVE_LIB$1)
    AC_CHECK_LIB([$1],[$2], [with_$1="yes"],
        [CPPFLAGS="$saved_CPPFLAGS";LDFLAGS="$saved_LDFLAGS";
         LIBS="$saved_LIBS";with_$1="no";
         AC_MSG_WARN([=======================================]);
         AC_MSG_WARN([Did not find lib$1]);
         AC_MSG_WARN([Disabling lib$1 dependent functions]);
         AC_MSG_WARN([=======================================])])
fi
])
