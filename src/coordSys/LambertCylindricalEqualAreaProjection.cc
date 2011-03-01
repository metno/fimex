/*
 * Fimex, LambertCylindricalEqualAreaProjection.cc
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Mar 01, 2011
 *      Author: Heiko Klein
 */

#include "fimex/coordSys/LambertCylindricalEqualAreaProjection.h"
#include <boost/regex.hpp>
#include "fimex/Utils.h"


namespace MetNoFimex
{

using namespace std;

LambertCylindricalEqualAreaProjection::LambertCylindricalEqualAreaProjection()
: ProjectionImpl("lambert_cylindrical_equal_area", false)
{
}

bool LambertCylindricalEqualAreaProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, "cea");
}

std::vector<CDMAttribute> LambertCylindricalEqualAreaProjection::parametersFromProj4(const std::string& proj4Str)
{
    vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str)) return attrs;

    attrs.push_back(CDMAttribute("grid_mapping_name", "lambert_cylindrical_equal_area"));

    boost::smatch what;
    double lon0 = 0.;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lon_0=(\\S+)"))) {
        lon0 = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("longitude_of_central_meridian", lon0));

    if (boost::regex_search(proj4Str, what, boost::regex("\\+lat_ts=(\\S+)"))) {
        double lat_ts = string2type<double>(what[1].str());
        attrs.push_back(CDMAttribute("standard_parallel", lat_ts));
    }
    if (boost::regex_search(proj4Str, what, boost::regex("\\+k=(\\S+)"))) {
        double k = string2type<double>(what[1].str());
        attrs.push_back(CDMAttribute("scale_factor_at_projection_origin", k));
    }

    proj4GetEarthAttributes(proj4Str, attrs);
    attrs.push_back(CDMAttribute("proj4", proj4Str));
    return attrs;
}

std::ostream& LambertCylindricalEqualAreaProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    oproj << "+proj=cea";
    addParameterToStream(oproj, "longitude_of_central_meridian", " +lon_0=");
    addParameterToStream(oproj, "standard_parallel", " +lat_ts="); // only polar-stereographic, exclusive with k
    addParameterToStream(oproj, "scale_factor_at_projection_origin", " +k=");

    return oproj;
}

}
