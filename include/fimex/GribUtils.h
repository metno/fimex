/*
 * Fimex, GribUtils.h
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Dec 11, 2009
 *      Author: Heiko Klein
 */

#ifndef GRIBUTILS_H_
#define GRIBUTILS_H_

#include "fimex/GridDefinition.h"

#include <stdexcept>

// forward declaration
struct grib_handle;

/**
 *  macro to call #mifi_grib_check with correct line and file
 */
#define MIFI_GRIB_CHECK(error, msg) mifi_grib_check(error, msg, __LINE__, __FILE__);

/**
 * runtime-exception checker for grib_check
 */
void mifi_grib_check(int error, const char* msg, int line, const char* file);


namespace MetNoFimex
{

/**
 * get the orientation of the data
 * @param gh grib-handle
 */
GridDefinition::Orientation gribGetGridOrientation(std::shared_ptr<grib_handle> gh);

/**
 * convert the stepUnits to seconds, i.e. D (day) = 60*60*24
 *
 * @warning this method is not highly accurate for stepUnits >= month due to missing calendar support (month = 30days, year = 365 days)
 */
unsigned long gribStepUnits2seconds(const std::string& stepUnits);
/**
 * find the largest unit covering the seconds, i.e. the opposite of gribStepUnits2seconds()
 */
std::string gribSeconds2stepUnits(unsigned long seconds);


}

#endif /* GRIBUTILS_H_ */
