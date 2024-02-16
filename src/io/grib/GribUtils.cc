/*
 * Fimex, GribUtils.cc
 *
 * (C) Copyright 2009-2024, met.no
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

#include "GribUtils.h"

#include "fimex/CDMException.h"

#include <sstream>

#include <grib_api.h>

void mifi_grib_check(int error, const char* msg, int line, const char* file)
{
    if (error) {
        const char* errMsg = grib_get_error_message(error);
        std::ostringstream oss;
        oss << "gribError occured in " << file << " at line " << line;
        oss << " : " << errMsg;
        if (msg != 0) {
            oss << "; " << msg;
        }
        throw std::runtime_error(oss.str());
    }
}

namespace MetNoFimex {

GridDefinition::Orientation gribGetGridOrientation(std::shared_ptr<grib_handle> gh)
{
    unsigned long mode = 0;
    long val = 0;
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "iScansNegatively", &val), "iScansNegatively");
    if (val) {
        mode |= GridDefinition::ScanStartRight;
    }
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "jScansPositively", &val), "jScansPositively");
    if (val) {
        mode |= GridDefinition::ScanStartBottom;
    }
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "jPointsAreConsecutive", &val), "jPointsAreConsecutive");
    if (val) {
        mode |= GridDefinition::ScanIsVertical;
    }
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "alternativeRowScanning", &val), "alternativeRowScanning");
    if (val) {
        mode |= GridDefinition::ScanIsAlternating;
    }

    return static_cast<GridDefinition::Orientation>(mode);
}

unsigned long gribStepUnits2seconds(const std::string& stepUnits)
{
    unsigned long seconds;
    if (stepUnits == "s") {
        seconds = 1;
    } else if (stepUnits == "m") {
        seconds = 60;
    } else if (stepUnits == "h") {
        seconds = 60 * 60;
    } else if (stepUnits == "3h") {
        seconds = 3 * 60 * 60;
    } else if (stepUnits == "6h") {
        seconds = 6 * 60 * 60;
    } else if (stepUnits == "12h") {
        seconds = 12 * 60 * 60;
    } else if (stepUnits == "D") {
        seconds = 24 * 60 * 60;
    } else if (stepUnits == "M") {
        seconds = 24 * 60 * 60 * 30;
    } else if (stepUnits == "Y") {
        seconds = 24 * 60 * 60 * 365;
    } else if (stepUnits == "10Y") {
        seconds = 24 * 60 * 60 * 365 * 10;
    } else if (stepUnits == "30Y") {
        seconds = 24 * 60 * 60 * 365 * 30;
    } else if (stepUnits == "C") {
        seconds = 24UL * 60 * 60 * 365 * 100;
    } else {
        throw CDMException("found undefined stepUnits in gribReader: " + stepUnits);
    }
    return seconds;
}

std::string gribSeconds2stepUnits(unsigned long seconds)
{
    if (seconds <= 0) {
        throw CDMException("cannot find grib-stepUnits when offset-seconds <= 0");
    }
    std::string stepUnits;
    if ((seconds % (24UL * 60 * 60 * 365 * 100)) == 0) {
        stepUnits = "C";
    } else if ((seconds % (24 * 60 * 60 * 365 * 30)) == 0) {
        stepUnits = "30Y";
    } else if ((seconds % (24 * 60 * 60 * 365 * 10)) == 0) {
        stepUnits = "10Y";
    } else if ((seconds % (24 * 60 * 60 * 365)) == 0) {
        stepUnits = "Y";
    } else if ((seconds % (24 * 60 * 60 * 30)) == 0) {
        stepUnits = "M";
    } else if ((seconds % (24 * 60 * 60)) == 0) {
        stepUnits = "D";
    } else if ((seconds % (12 * 60 * 60)) == 0) {
        stepUnits = "12h";
    } else if ((seconds % (6 * 60 * 60)) == 0) {
        stepUnits = "6h";
    } else if ((seconds % (3 * 60 * 60)) == 0) {
        stepUnits = "3h";
    } else if ((seconds % (60 * 60)) == 0) {
        stepUnits = "h";
    } else if ((seconds % (60)) == 0) {
        stepUnits = "m";
    } else {
        stepUnits = "s";
    }

    return stepUnits;
}

// see https://codes.ecmwf.int/grib/format/grib2/ctables/4/4/
/*
0	Minute
1	Hour
2	Day
3	Month
4	Year
5	Decade (10 years)
6	Normal (30 years)
7	Century (100 years)
10	3 hours
11	6 hours
12	12 hours
13	Second
*/
unsigned long gribStepUnitsFromText(const std::string& stepUnits)
{
    if (stepUnits == "s") {
        return 13;
    } else if (stepUnits == "m") {
        return 0;
    } else if (stepUnits == "h") {
        return 1;
    } else if (stepUnits == "3h") {
        return 10;
    } else if (stepUnits == "6h") {
        return 11;
    } else if (stepUnits == "12h") {
        return 12;
    } else if (stepUnits == "D") {
        return 2;
    } else if (stepUnits == "M") {
        return 3;
    } else if (stepUnits == "Y") {
        return 4;
    } else if (stepUnits == "10Y") {
        return 5;
    } else if (stepUnits == "30Y") {
        return 6;
    } else if (stepUnits == "C") {
        return 7;
    } else {
        throw CDMException("unknown stepUnits text '" + stepUnits + "'");
    }
}

std::string gribStepUnitsToText(unsigned long units)
{
    switch (units) {
    case 13: return "s";
    case 0:  return "m";
    case 1:  return "h";
    case 10: return "3h";
    case 11: return "6h";
    case 12: return "12h";
    case 2:  return "D";
    case 3:  return "M";
    case 4:  return "Y";
    case 5:  return  "10Y";
    case 6:  return  "30Y";
    case 7:  return  "C";
    default: {
        std::ostringstream msg;
        msg << "unknown stepUnits value " << units;
        throw CDMException(msg.str());
    }
    }
}

} // namespace MetNoFimex
