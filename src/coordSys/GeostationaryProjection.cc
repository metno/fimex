geostationary/*
 * Fimex, GeostationaryProjection.cc
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
 *  Created on: May 12, 2017
 *      Author: Martin Raspaud
 */

#include "fimex/coordSys/GeostationaryProjection.h"
#include <boost/regex.hpp>
#include "fimex/Utils.h"

namespace MetNoFimex
{

using namespace std;

GeostationaryProjection::GeostationaryProjection()
: ProjectionImpl("geostationary", false)
{
}

bool GeostationaryProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, "geos");
}

std::vector<CDMAttribute> GeostationaryProjection::parametersFromProj4(const std::string& proj4Str)
{
    vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str)) return attrs;

    attrs.push_back(CDMAttribute("grid_mapping_name", "geostationary"));

    boost::smatch what;
    double lon0 = 0.;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lon_0=(\\S+)"))) {
        lon0 = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("longitude_of_projection_origin", lon0));

    double lat0 = 0.;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lat_0=(\\S+)"))) {
        lat0 = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("latitude_of_projection_origin", lat0));

    double h = 0.;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+h=(\\S+)"))) {
        h = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("perspective_point_height", h));

    string sweep = "y";
    if (boost::regex_search(proj4Str, what, boost::regex("\\+sweep=(\\S+)"))) {
        sweep = what[1].str();
    }

    proj4GetEarthAttributes(proj4Str, attrs);
    attrs.push_back(CDMAttribute("proj4", proj4Str));
    return attrs;
}

std::ostream& GeostationaryProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    oproj << "+proj=geos";
    addParameterToStream(oproj, "longitude_of_projection_origin", " +lon_0=");
    addParameterToStream(oproj, "latitude_of_projection_origin", " +lat_0=");
    addParameterToStream(oproj, "perspective_point_height", " +h=");
    addParameterToStream(oproj, "sweep_angle_axis", " +sweep=");

    return oproj;
}

}
