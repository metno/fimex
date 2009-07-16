##### http:///                                  -*- Autoconf -*-
#
# WARNING
#
#   This file is a copy of
#   'common_build_files/m4/metno_have_udunits.m4'.  The next time
#   'common_build_files/distribute.sh' is run with the appropriate
#   arguments, all changes to this file will disappear.  Please edit
#   the original.
#
# SYNOPSIS
#
#   METNO_HAVE_UDUNITS([ACTION-IF-TRUE], [ACTION-IF-FALSE])
#
# DESCRIPTION
#
#   This macro will check for the existence of the Unidata units
#   (udunits) library (http://www.unidata.ucar.edu/packages/udunits/).
#   The check is done by checking for the header file udunits.h and
#   the udunits library object file.  A --with-udunits option is
#   supported as well.  The following output variables are set with
#   AC_SUBST:
#
#     AC_SUBST(UDUNITS_CPPFLAGS)
#     AC_SUBST(UDUNITS_LDFLAGS)
#     AC_SUBST(UDUNITS_LIBS)
#
#   You can use them like this in Makefile.am:
#
#     AM_CPPFLAGS = $(UDUNITS_CPPFLAGS)
#     AM_LDFLAGS = $(UDUNITS_LDFLAGS)
#     program_LDADD = $(UDUNITS_LIBS)
#
#   Additionally, the C preprocessor symbol HAVE_UDUNITS will be
#   defined with AC_DEFINE([HAVE_UDUNITS]) if the library is
#   available.  Forthermore, the variable have_udunits will be set to
#   "yes" if the library is available.
#
# AUTHOR
#
#   Martin Thorsen Ranang <mtr@linpro.no>
#
# LAST MODIFICATION
#
#   $Date$
#
# ID
#
#   $Id$
#
# COPYLEFT
#
#   Copyright (c) 2007 Meteorologisk institutt <diana@met.no>
#
#   This program is free software: you can redistribute it and/or
#   modify it under the terms of the GNU General Public License as
#   published by the Free Software Foundation, either version 3 of the
#   License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program. If not, see
#   <http://www.gnu.org/licenses/>.
#

# METNO_HAVE_UDUNITS([ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ------------------------------------------------------
AC_DEFUN([METNO_HAVE_UDUNITS], [
  AH_TEMPLATE([HAVE_UDUNITS], [Define if udunits is available])
  if test "$ac_udunits_path" != ""; then
    saved_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$CPPFLAGS -I$ac_udunits_path/include"
    AC_CHECK_HEADER([udunits.h], [
      AC_LANG_PUSH(C)
      saved_LDFLAGS="$LDFLAGS"
      LDFLAGS="$LDFLAGS -L$ac_udunits_path/lib"

      # Udunits needs a couple of (mathematical) functions.  The
      # following macro _will_ affect LIBS (before it is saved).
      AC_CHECK_LIB([m], [floor])
      AC_CHECK_FUNCS([fmod log10 ceil pow])
      
      saved_LIBS="$LIBS"
      have_udunits=no
      AC_CHECK_LIB([udunits], [utInit],
        [AC_SUBST(UDUNITS_CPPFLAGS, [-I$ac_udunits_path/include])
         AC_SUBST(UDUNITS_LDFLAGS, [-L$ac_udunits_path/lib])
         AC_SUBST(UDUNITS_LIBS, [-ludunits])
         AC_DEFINE([HAVE_UDUNITS])
         have_udunits=yes
         $1
        ], [])
      if test $have_udunits == "no"; then
         # check for udunits2 whith compatibility layer
         AC_CHECK_LIB([udunits2], [utInit],
            [AC_SUBST(UDUNITS_CPPFLAGS, [-I$ac_udunits_path/include])
               AC_SUBST(UDUNITS_LDFLAGS, [-L$ac_udunits_path/lib])
               AC_SUBST(UDUNITS_LIBS, [-ludunits])
               AC_DEFINE([HAVE_UDUNITS])
               have_udunits=yes
               $1
            ], [])         
      fi
      AC_LANG_POP(C)
      LIBS="$saved_LIBS"
      LDFLAGS="$saved_LDFLAGS"
      if test $have_udunits == "no"; then
         echo "error: udunits not found. Please make sure that udunits is properly installed when compiling with NetCDF.";
         exit 1;
      fi
    ], [
      :
      $2
    ])
    CPPFLAGS="$saved_CPPFLAGS"
  fi
])
