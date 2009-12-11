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
#include "GribUtils.h"

#include <grib_api.h>

namespace MetNoFimex {

GridDefinition::Orientation gribGetGridOrientation(boost::shared_ptr<grib_handle> gh)
{
    unsigned long mode = 0;
    long val = 0;
    long error = 0;
    error = grib_get_long(gh.get(), "iScansNegatively", &val);
    if ((error == GRIB_SUCCESS) && val) {
        mode |= GridDefinition::ScanStartRight;
    }
    val = 0;
    error = grib_get_long(gh.get(), "jScansNegatively", &val);
    if ((error == GRIB_SUCCESS) && val) {
            mode |= GridDefinition::ScanStartBottom;
    }
    val = 0;
    error = grib_get_long(gh.get(), "jPointsAreConsecutive", &val);
    if ((error == GRIB_SUCCESS) && val) {
        mode |= GridDefinition::ScanIsVertical;
    }
    val = 0;
    error = grib_get_long(gh.get(), "alternativeRowScanning", &val);
    if ((error == GRIB_SUCCESS) && val) {
        mode |= GridDefinition::ScanIsAlternating;
    }

    return static_cast<GridDefinition::Orientation>(mode);
}

}
#endif /* HAVE_GRIBAPI_h */
