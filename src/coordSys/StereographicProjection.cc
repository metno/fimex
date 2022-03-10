/*
 * Fimex, StereographicProjection.cc
 *
 * (C) Copyright 2010-2022, met.no
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
#include "fimex/mifi_constants.h"
#include <cmath>
#include <regex>
#include <sstream>

namespace MetNoFimex {

StereographicProjection::StereographicProjection()
    : ProjectionImpl("stereographic", false)
{
}

StereographicProjection::StereographicProjection(const std::string& name)
    : ProjectionImpl(name, false)
{
}
StereographicProjection::~StereographicProjection() {}

bool StereographicProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, "stere");
}

std::vector<CDMAttribute> StereographicProjection::parametersFromProj4(const std::string& proj4Str)
{
    std::vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str))
        return attrs;

    attrs.push_back(CDMAttribute("grid_mapping_name", "stereographic"));

    std::smatch what;
    if (std::regex_search(proj4Str, what, std::regex("\\+lon_0=(\\S+)"))) {
        double lon0 = std::strtod(what[1].str().c_str(), (char**)NULL);
        attrs.push_back(CDMAttribute("longitude_of_projection_origin", lon0));
    }
    if (std::regex_search(proj4Str, what, std::regex("\\+lat_0=(\\S+)"))) {
        double lat0 = std::strtod(what[1].str().c_str(), (char**)NULL);
        attrs.push_back(CDMAttribute("latitude_of_projection_origin", lat0));
    }

    if (std::regex_search(proj4Str, what, std::regex("\\+lat_ts=(\\S+)"))) {
        double lat_ts = std::strtod(what[1].str().c_str(), (char **)NULL);
        attrs.push_back(CDMAttribute("standard_parallel", lat_ts));

    } else if (std::regex_search(proj4Str, what, std::regex("\\+k=(\\S+)"))) {
        double k = std::strtod(what[1].str().c_str(), (char**)NULL);
        attrs.push_back(CDMAttribute("scale_factor_at_projection_origin", k));
    }

    proj4GetEarthAttributes(proj4Str, attrs);
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

} // namespace MetNoFimex
