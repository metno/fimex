############
# SYNOPSIS
#
#   MIFI_USE_NETCDF()
#
# DESCRIPTION
#
#   These macro will check for the existence of the netcdf library.
#   The library will be added by a --with-netcdf option.
#   Libraries and Includes will be added by --with-netcdf=DIR  -- with-netcd=../nc-config or
#   --with-LIBNAME=LIB/INCLUDE
#
#
#   Libs and includes will be automatically added to the CPPFLAGS, LDFLAGS and LIBS.
#   The HAVE_NETCDF macro will be set.
#   In addition, HAVE_NETCDF4 macro will be set if nc4/hdf5 exists
#
#
#   with_netcdf will be set to yes if AC_CHECK_LIB works.
#
# AUTHOR
#
#   Heiko Klein <Heiko.Klein@met.no>
############

AC_DEFUN([MIFI_USE_NETCDF],[
# Checks for libraries that can be dropped
saved_CPPFLAGS="$CPPFLAGS"
saved_LDFLAGS="$LDFLAGS"
saved_LIBS="$LIBS"
AC_ARG_WITH([netcdf],
    AC_HELP_STRING([--with-netcdf=DIR],
    [the location of optional libnetcdf files and library either as path to nc-config or DIR or INC,LIB]),
    [],
    [with_netcdf=check])
case $with_netcdf in
    yes)
     AC_MSG_NOTICE([Using system implementation of libnetcdf])
     ;;
    check)
     AC_MSG_NOTICE([Checking system implementation of libnetcdf])
     ;;
    no)
     AC_MSG_NOTICE([Building library with libnetcdf dependent functions disabled])
     ;;
    *,*)
      addincdir="`echo $with_netcdf | cut -f1 -d,`"
      addlibdir="`echo $with_netcdf | cut -f2 -d,`"
      mifi_CPPFLAGS="-I$addincdir"
      mifi_LDFLAGS="-L$addlibdir"
      ;;
    *)
      if [[ -d $with_netcdf ]]; then
          addincdir="$with_netcdf/include"
          addlibdir="$with_netcdf/lib"
          mifi_CPPFLAGS="-I$addincdir"
          mifi_LDFLAGS="-L$addlibdir"
      else
          mifi_CPPFLAGS=`$with_netcdf --cflags`
          mifi_LDFLAGS=`$with_netcdf --libs`
      fi
      ;;
esac
mifi_have_feat=MIFI_HAVE_LIBNETCDF
LIBS="$LIBS $mifi_LIBS"
CPPFLAGS="$CPPFLAGS $mifi_CPPFLAGS"
LDFLAGS="$LDFLAGS $mifi_LDFLAGS"
if test [ x$with_netcdf != xno]; then
    AC_CHECK_HEADERS([netcdf.h],
        [],
        [
         if test [ x$with_netcdf == xcheck ]; then
            AC_MSG_NOTICE([disabling libnetcdf: netcdf.h not found]);
            with_netcdf=no;
         else
            AC_MSG_ERROR([netcdf.h not found, please fix or disable --with-netcdf]);
         fi
        ])
fi
if test [ x$with_netcdf != xno]; then
    AC_CHECK_LIB([netcdf],[nc_create],
        [with_netcdf=yes;
         MIFI_NETCDF_LIBS="-lnetcdf";
         MIFI_NETCDF_CPPFLAGS=$mifi_CPPFLAGS;
         MIFI_NETCDF_LDFLAGS=$mifi_LDFLAGS;
         AC_SUBST(MIFI_NETCDF_LIBS)
         AC_SUBST(MIFI_NETCDF_CPPFLAGS)
         AC_SUBST(MIFI_NETCDF_LDFLAGS)
        ],
        [
         if test [ x$with_netcdf == xcheck ]; then
            AC_MSG_NOTICE([disabling libnetcdf: not found]);
            with_netcdf=no;
         else
            AC_MSG_ERROR([libnetcdf not found, please fix or disable --with-netcdf]);
         fi
        ])
fi
AM_CONDITIONAL([HAVE_NETCDF], [test "x$with_netcdf" == xyes])

# check netcdf4 features in library
AC_CHECK_LIB([netcdf], [nc_def_var_deflate],
             [AC_DEFINE(HAVE_NETCDF_HDF5_LIB, [1], [netcdf comes with netcdf4 functions/hdf5 support])],
             [])

CPPFLAGS="$saved_CPPFLAGS";
LDFLAGS="$saved_LDFLAGS";
LIBS="$saved_LIBS";

              
])

