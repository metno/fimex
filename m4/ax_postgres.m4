#
#
#   Copyright (C) 2007 met.no
#   
#   Contact information:
#   Norwegian Meteorological Institute
#   Box 43 Blindern
#   0313 OSLO
#   NORWAY
#   E-mail: fimex@met.no
# 
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, 
#   MA  02110-1301, USA
#
#   Checks for the presence of postgres
#

# Check for PostgreSQL
# Usage: AX_POSTGRES_CHECK()
AC_DEFUN([AX_POSTGRES_CHECK],
[
# Set up option
AC_ARG_WITH(
	[pq],
	AS_HELP_STRING([--with-pq=PATH], 
	[Specify the pg_config binary. (if empty, pg_config in the default path is used) (disabled by default)]),
	[if test "x$with_pq" != "xyes"; then 
            PGCONFIG="${with_pq}"
         fi],
        [with_pq="no"]
)

AS_IF([test "x$with_pq" != xno],
[AC_PATH_PROG(PGCONFIG,pg_config)
        if test -f "${PGCONFIG}"; then
	     CPPFLAGS="$CPPFLAGS -I`${PGCONFIG} --includedir`"
	     LDFLAGS="$LDFLAGS -L`${PGCONFIG} --libdir`"
	     LIBS="$LIBS -lpq"
	     AC_CHECK_HEADER([postgresql/libpq-fe.h],
					[AC_DEFINE([HAVE_LIBPQ], [1],
                         [Define if you have libpq])],
					[AC_MSG_ERROR([
-------------------------------------------------------------------------
    Could not locate pq.h x$with_pq $PGCONFIG $pgconfig
    This indicates that Postgres may not be correctly installed for AX
    on your site. AX requires the development files for server-side
    programming of PostgreSQL to be installed.
    
    CPPFLAGS=$CPPFLAGS
-------------------------------------------------------------------------
            ])
	])
        else
	     AC_MSG_ERROR([
$PGCONFIG $pgconfig
-------------------------------------------------------------------------
   Unable to find pg_config. If Postgres is installed, make sure 
   pg_config is in your PATH, or specify the path in which postgres 
   is installed with --with-pgsql=PATH
-------------------------------------------------------------------------
	               ])
        fi]

)

])


