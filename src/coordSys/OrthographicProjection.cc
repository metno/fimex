/*
 * Fimex, OrthographicProjection.cc
 *
 * (C) Copyright 2011-2022, met.no
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

#include "fimex/coordSys/OrthographicProjection.h"

#include "fimex/String2Type.h"

#include <regex>

namespace MetNoFimex {

static const std::string PROJ_ORTHO = "ortho";
static const std::string KEY_LON0 = "lon_0";
static const std::string KEY_LAT0 = "lat_0";

double extractProj4InitValue(const std::string& proj4Str, const std::string& key, double dflt = 0)
{
    std::smatch what;
    if (std::regex_search(proj4Str, what, std::regex("\\+" + key + "=(\\S+)")))
        return string2type<double>(what[1].str());
    else
        return dflt;
}

OrthographicProjection::OrthographicProjection()
    : ProjectionImpl("orthographic", false)
{
}

OrthographicProjection::~OrthographicProjection() {}

bool OrthographicProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, PROJ_ORTHO);
}

std::vector<CDMAttribute> OrthographicProjection::parametersFromProj4(const std::string& proj4Str)
{
    std::vector<CDMAttribute> attrs;
    if (acceptsProj4(proj4Str)) {
        attrs.push_back(CDMAttribute("grid_mapping_name", "orthographic"));
        attrs.push_back(CDMAttribute("longitude_of_projection_origin", extractProj4InitValue(proj4Str, "lon_0")));
        attrs.push_back(CDMAttribute("latitude_of_projection_origin", extractProj4InitValue(proj4Str, "lat_0")));

        proj4GetEarthAttributes(proj4Str, attrs);
        attrs.push_back(CDMAttribute("proj4", proj4Str));
    }
    return attrs;
}

std::ostream& OrthographicProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    oproj << "+proj=" << PROJ_ORTHO;
    addParameterToStream(oproj, "longitude_of_projection_origin", " +" + KEY_LON0 + "=");
    addParameterToStream(oproj, "latitude_of_projection_origin", " +" + KEY_LAT0 + "=");

    return oproj;
}

} // namespace MetNoFimex
