/*
 * Fimex, CDMconstants.c
 *
 * (C) Copyright 2010, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 *  Created on: May 5, 2010
 *      Author: Heiko Klein
 */

#include "fimex/CDMconstants.h"
#include "../config.h"

const char* fimexVersion()
{
    return VERSION;
}

int fimexHasNetcdf()
{
#ifdef MIFI_HAVE_NETCDF
    return 1;
#else
    return 0;
#endif
}
int fimexHasGribApi()
{
#ifdef HAVE_GRIBAPI_H
    return 1;
#else
    return 0;
#endif
}
int fimexHasFelt()
{
#ifdef HAVE_FELT
    return 1;
#else
    return 0;
#endif

}
