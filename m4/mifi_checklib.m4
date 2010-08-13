############
# SYNOPSIS
#
#   MIFI_REQLIB([LIBNAME],[FUNCTIONNAME], [EXTRA_LIBS])
#   MIFI_USELIB([LIBNAME],[FUNCTIONNAME], [EXTRA_LIBS])
#
# DESCRIPTION
#
#   These macro will check for the existence of a library.
#   The library will be added by a --with-LIBNAME option.
#   Libraries and Includes will be added by --with-LIBNAME=DIR or --with-LIBNAME=LIB/INCLUDE
# 
#   With 'REQLIB' configure will stop when the the library cannot be found. 'USELIB' will continue.
#  
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
mifi_LIBS="$3"
AC_ARG_WITH([$1],
    AC_HELP_STRING([--with-$1=DIR],
    [the location of mandatory lib$1 files and library either as DIR or INC,LIB]),
    ,
    [with_$1=yes])
case $with_$1 in
    yes)
     echo "Using system implementation of lib$1"
     ;;
    no)
     AC_MSG_ERROR([lib$1 is required])
     ;;
    *,*)
      addincdir="`echo $with_$1 | cut -f1 -d,`"
      addlibdir="`echo $with_$1 | cut -f2 -d,`"
      mifi_CPPFLAGS="-I$addincdir"
      mifi_LDFLAGS="-L$addlibdir"
      ;;
    *)
      addincdir="$with_$1/include"
      addlibdir="$with_$1/lib"
      mifi_CPPFLAGS="-I$addincdir"
      mifi_LDFLAGS="-L$addlibdir"
      ;;
esac
mifi_have_feat=m4_toupper(MIFI_HAVE_LIB$1)
LIBS="$LIBS $mifi_LIBS"
CPPFLAGS="$CPPFLAGS $mifi_CPPFLAGS"
LDFLAGS="$LDFLAGS $mifi_LDFLAGS"
if test [ x$with_$1 != xno]; then
    AC_CHECK_LIB([$1],[$2],
        [with_$1=yes;
         m4_toupper(MIFI_$1_LIBS)="-l$1 $mifi_LIBS";
         m4_toupper(MIFI_$1_CPPFLAGS)=$mifi_CPPFLAGS;
         m4_toupper(MIFI_$1_LDFLAGS)=$mifi_LDFLAGS;
         AC_SUBST(m4_toupper(MIFI_$1_LIBS))
         AC_SUBST(m4_toupper(MIFI_$1_CPPFLAGS))
         AC_SUBST(m4_toupper(MIFI_$1_LDFLAGS))
        ],
        [with_$1=no;
         AC_MSG_ERROR([Did not find lib$1, this is required to continue])])
fi
CPPFLAGS="$saved_CPPFLAGS";
LDFLAGS="$saved_LDFLAGS";
LIBS="$saved_LIBS";
])

AC_DEFUN([MIFI_USELIB],[
# Checks for libraries that can be dropped
saved_CPPFLAGS="$CPPFLAGS"
saved_LDFLAGS="$LDFLAGS"
saved_LIBS="$LIBS"
mifi_LIBS="$3"
AC_ARG_WITH([$1],
    AC_HELP_STRING([--with-$1=DIR],
    [the location of optional lib$1 files and library either as DIR or INC,LIB]),
    ,
    [with_$1=yes])
case $with_$1 in
    yes)
     echo "Using system implementation of lib$1"
     ;;
    no)
     AC_MSG_WARN([Building library with lib$1 dependent functions disabled])
     ;;
    *,*)
      addincdir="`echo $with_$1 | cut -f1 -d,`"
      addlibdir="`echo $with_$1 | cut -f2 -d,`"
      mifi_CPPFLAGS="-I$addincdir"
      mifi_LDFLAGS="-L$addlibdir"
      ;;
    *)
      addincdir="$with_$1/include"
      addlibdir="$with_$1/lib"
      mifi_CPPFLAGS="-I$addincdir"
      mifi_LDFLAGS="-L$addlibdir"
      ;;
esac
mifi_have_feat=m4_toupper(MIFI_HAVE_LIB$1)
LIBS="$LIBS $mifi_LIBS"
CPPFLAGS="$CPPFLAGS $mifi_CPPFLAGS"
LDFLAGS="$LDFLAGS $mifi_LDFLAGS"
if test [ x$with_$1 != xno]; then
    AC_CHECK_LIB([$1],[$2],
        [with_$1=yes;
         m4_toupper(MIFI_$1_LIBS)="-l$1 $mifi_LIBS";
         m4_toupper(MIFI_$1_CPPFLAGS)=$mifi_CPPFLAGS;
         m4_toupper(MIFI_$1_LDFLAGS)=$mifi_LDFLAGS;
         AC_SUBST(m4_toupper(MIFI_$1_LIBS))
         AC_SUBST(m4_toupper(MIFI_$1_CPPFLAGS))
         AC_SUBST(m4_toupper(MIFI_$1_LDFLAGS))
        ],
        [with_$1=no;
         AC_MSG_WARN([=======================================]);
         AC_MSG_WARN([Did not find lib$1]);
         AC_MSG_WARN([Disabling lib$1 dependent functions]);
         AC_MSG_WARN([=======================================])])
fi
CPPFLAGS="$saved_CPPFLAGS";
LDFLAGS="$saved_LDFLAGS";
LIBS="$saved_LIBS";
])

