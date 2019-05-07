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
#include "fimex_config.h"
#include <vector>
#include <iterator>
#include <algorithm>
#include <string>

const char* fimexVersion()
{
    return MIFI_VERSION_STRING;
}

unsigned int mifi_version_major()
{
    return MIFI_VERSION_MAJOR;
}
unsigned int mifi_version_minor()
{
    return MIFI_VERSION_MINOR;
}
unsigned int mifi_version_patch()
{
    return MIFI_VERSION_PATCH;
}
unsigned int mifi_version_status()
{
    return MIFI_VERSION_STATUS;
}
