/*
 * Fimex, AzimuthalEquidistantProjection.cc
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

#include "fimex/coordSys/AzimuthalEquidistantProjection.h"
#include "fimex/Utils.h"
#include <regex>

namespace MetNoFimex
{

using namespace std;

AzimuthalEquidistantProjection::AzimuthalEquidistantProjection()
: ProjectionImpl("azimuthal_equidistant", false)
{
}

bool AzimuthalEquidistantProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, "aeqd");
}

std::vector<CDMAttribute> AzimuthalEquidistantProjection::parametersFromProj4(const std::string& proj4Str)
{
    vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str)) return attrs;

    attrs.push_back(CDMAttribute("grid_mapping_name", "azimuthal_equidistant"));

    std::smatch what;
    double lon0 = 0.;
    if (std::regex_search(proj4Str, what, std::regex("\\+lon_0=(\\S+)"))) {
        lon0 = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("longitude_of_projection_origin", lon0));

    double lat0 = 0.;
    if (std::regex_search(proj4Str, what, std::regex("\\+lat_0=(\\S+)"))) {
        lat0 = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("latitude_of_projection_origin", lat0));

    proj4GetEarthAttributes(proj4Str, attrs);
    attrs.push_back(CDMAttribute("proj4", proj4Str));
    return attrs;
}

std::ostream& AzimuthalEquidistantProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    oproj << "+proj=aeqd";
    addParameterToStream(oproj, "longitude_of_projection_origin", " +lon_0=");
    addParameterToStream(oproj, "latitude_of_projection_origin", " +lat_0=");

    return oproj;
}

}
