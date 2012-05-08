/*
 * Fimex, StereographicProjection.cc
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

#include "fimex/coordSys/StereographicProjection.h"
#include <boost/regex.hpp>
#include <cmath>
#include <sstream>
#include "fimex/mifi_constants.h"


namespace MetNoFimex
{

using namespace std;

StereographicProjection::StereographicProjection()
: ProjectionImpl("stereographic", false)
{
}

bool StereographicProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, "stere");
}

std::vector<CDMAttribute> StereographicProjection::parametersFromProj4(const std::string& proj4Str)
{
    vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str)) return attrs;

    boost::smatch what;
    double lon0, lat0, k;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lon_0=(\\S+)"))) {
        lon0 = std::strtod(what[1].str().c_str(), (char **)NULL);
    } else {
        lon0 = 0;
    }
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lat_0=(\\S+)"))) {
        lat0 = std::strtod(what[1].str().c_str(), (char **)NULL);
    } else {
        lat0 = 0;
    }
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lat_ts=(\\S+)"))) {
        double lat_ts = std::strtod(what[1].str().c_str(), (char **)NULL);
        k = (1+sin(MIFI_PI * lat_ts / 180)) / 2;

    } else if (boost::regex_search(proj4Str, what, boost::regex("\\+k=(\\S+)"))) {
        k = std::strtod(what[1].str().c_str(), (char **)NULL);
    } else {
        k = 1;
    }

    attrs.push_back(CDMAttribute("grid_mapping_name", "stereographic"));
    attrs.push_back(CDMAttribute("scale_factor_at_projection_origin", k));
    attrs.push_back(CDMAttribute("longitude_of_projection_origin", lon0));
    attrs.push_back(CDMAttribute("latitude_of_projection_origin", lat0));

    proj4GetEarthAttributes(proj4Str, attrs);
    attrs.push_back(CDMAttribute("proj4", proj4Str));
    return attrs;
}

std::ostream& StereographicProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    oproj << "+proj=stere";
    addParameterToStream(oproj, "latitude_of_projection_origin", " +lat_0=");
    addParameterToStream(oproj, "straight_vertical_longitude_from_pole", " +lon_0="); // polar-stereographic
    addParameterToStream(oproj, "longitude_of_projection_origin", " +lon_0="); // stereographic
    addParameterToStream(oproj, "scale_factor_at_projection_origin", " +k=");
    addParameterToStream(oproj, "standard_parallel", " +lat_ts="); // only polar-stereographic, exclusive with k
    return oproj;
}

}
