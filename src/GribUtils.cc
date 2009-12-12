/*
 * Fimex, GribUtils.cc
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

#include "fimex/config.h"
#ifdef HAVE_GRIBAPI_H
#include "fimex/GribUtils.h"
#include <sstream>
#include <grib_api.h>

void mifi_grib_check(int error, const char* msg, int line, const char* file) throw(std::runtime_error)
{
    if (error) {
        const char* errMsg = grib_get_error_message(error);
        std::ostringstream oss;
        oss << "gribError occured in " << file << " at line "<< line;
        oss << " : " << errMsg;
        if (msg != 0) {
            oss << "; " << msg;
        }
        throw std::runtime_error(oss.str());
    }
}

namespace MetNoFimex {

GridDefinition::Orientation gribGetGridOrientation(boost::shared_ptr<grib_handle> gh)
{
    unsigned long mode = 0;
    long val = 0;
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "iScansNegatively", &val), "iScansNegatively");
    if (val) {
        mode |= GridDefinition::ScanStartRight;
    }
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "jScansPositively", &val), "jScansPositively");
    if (!val) {
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

}
#endif /* HAVE_GRIBAPI_h */
