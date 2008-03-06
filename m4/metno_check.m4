##### http:///
#
# WARNING
#
#   This file is a copy of 'common_build_files/m4/metno_check.m4'.
#   The next time 'common_build_files/distribute.sh' is run with the
#   appropriate arguments, all changes to this file will disappear.
#   Please edit the original.
#
# SYNOPSIS
#
#   METNO_CHECK_HEADERS([HEADER-FILE...], 
#                       [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
#                       [INCLUDES], [CPPFLAGS = $CPPFLAGS], [DESCRIPTION])
#
#   METNO_CHECK_LIB(LIBRARY, [FUNCTION],
#                   [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
#                   [OTHER-LIBRARIES], [LIBS = $LIBS], [Description])
#
# DESCRIPTION
#
#   METNO_CHECK_HEADERS is an alternative to AC_CHECK_HEADERS that
#   allows specification of CPPFLAGS, to check for headerfiles with a
#   context as specified by e.g. pkg-config.  METNO_CHECK_HEADERS does
#   not permanently change CPPFLAGS.
#
#   METNO_CHECK_LIB is an alternative to AC_CHECK_LIB that allows
#   specification of LIBS, to check for a library within a context as
#   specified by e.g. pkg-config.  As opposed to AC_CHECK_LIB,
#   METNO_CHECK_LIB does not permanently change LIBS.
#
# AUTHORS
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
#   Copyright (c) 2006 Meterologisk Institutt <diana@met.no>
#
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License as
#   published by the Free Software Foundation; either version 2 of the
#   License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
#   02111-1307, USA.
#
#   As a special exception, the respective Autoconf Macro's copyright
#   owner gives unlimited permission to copy, distribute and modify the
#   configure scripts that are the output of Autoconf when processing
#   the Macro. You need not follow the terms of the GNU General Public
#   License when using or distributing such scripts, even though
#   portions of the text of the Macro appear in them. The GNU General
#   Public License (GPL) does govern all other use of the material that
#   constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the
#   Autoconf Macro released by the Autoconf Macro Archive. When you
#   make and distribute a modified version of the Autoconf Macro, you
#   may extend this special exception to the GPL to apply to your
#   modified version as well.
#

# METNO_CHECK_HEADERS([HEADER-FILE...], 
#                     [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
#                     [INCLUDES], [CPPFLAGS = $CPPFLAGS], [DESCRIPTION])
# -----------------------------------------------------------------------
AC_DEFUN([METNO_CHECK_HEADERS], [{
  # Backup the original CPPFLAGS value.
  metno_save_CPPFLAGS="$CPPFLAGS"
  # Inlcude the necessary -I flags.
  CPPFLAGS="$CPPFLAGS $5"
  # Do the check.
  m4_ifval([$6], 
           [AC_CHECK_HEADERS([$1], [$2], 
                             AC_MSG_FAILURE(dnl
[Could not find some of the headers from $6.

Please make sure that $6 is properly installed.]),
                             [$4])],
	   [AC_CHECK_HEADERS([$1], [$2], [$3], [$4])])
  # Reset the CPPFLAGS value.
  CPPFLAGS="$metno_save_CPPFLAGS"
}]) # METNO_CHECK_HEADERS

# METNO_CHECK_LIB(LIBRARY, [FUNCTION],
#                 [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
#                 [OTHER-LIBRARIES], [LIBS = $LIBS], [Description])
# --------------------------------------
AC_DEFUN([METNO_CHECK_LIB], [{
  # Backup the original LIBS value.
  metno_save_LIBS="$LIBS"
  # Inlcude the necessary -l, and -L flags.
  LIBS="$LIBS $6"
  # Do the check.
  m4_ifval([$7],
           [AC_CHECK_LIB([$1], [$2], [$3], 
                         AC_MSG_FAILURE(dnl
[The function $2 in $1 from $7 could not be found.

Please make sure that $7 is properly installed.]),
                         [$5])],
	   [AC_CHECK_LIB([$1], [$2], [$3], [$4], [$5])])
  # Reset the LIBS value.
  LIBS="$metno_save_LIBS"
}]) # METNO_CHECK_LIB
