/*
 * Fimex, PolarStereographicProjection.cc
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
 *  Created on: Apr 29, 2010
 *      Author: Heiko Klein
 */

#include "PolarStereographicProjection.h"
#include <boost/regex.hpp>
#include <cmath>
#include "fimex/Utils.h"
#include "fimex/Data.h"

namespace MetNoFimex
{

using namespace std;

bool PolarStereographicProjection::acceptsProj4(const std::string& proj4Str)
{
    if (proj4ProjectionMatchesName(proj4Str, "stere")) {
        boost::smatch what;
        if (boost::regex_search(proj4Str, what, boost::regex("\\+lat_0=(\\S+)"))) {
            int lat0 = string2type<double>(what[1].str());
            if (fabs(fabs(lat0)-90) < 1e-4) {
                return true; // +90/-90
            }
        }
    }
    return false;
}

std::vector<CDMAttribute> PolarStereographicProjection::parametersFromProj4(const std::string& proj4Str)
{
    vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str)) return attrs;

    // get Stereographic attributes
    attrs = StereographicProjection::parametersFromProj4(proj4Str);
    // switch grid_mapping_name to polar_stereographic
    vector<CDMAttribute>::iterator found = find_if(attrs.begin(), attrs.end(), CDMNameEqual("grid_mapping_name"));
    assert(found != attrs.end());
    *found = CDMAttribute("grid_mapping_name", "polar_stereographic");
    // switch longitude_of_projection_origin to straight_vertical_longitude_from_pole
    found = find_if(attrs.begin(), attrs.end(), CDMNameEqual("longitude_of_projection_origin"));
    if (found != attrs.end()) {
        double lonVal = found->getData()->asConstDouble()[0];
        *found = CDMAttribute("straight_vertical_longitude_from_pole", lonVal);
    }
    return attrs;
}

}
