/*
 * Fimex, MercatorProjection.cc
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
 *  Created on: Apr 30, 2010
 *      Author: Heiko Klein
 */

#include "fimex/coordSys/MercatorProjection.h"

#include "fimex/String2Type.h"

#include <regex>

namespace MetNoFimex
{
    using namespace std;

MercatorProjection::MercatorProjection()
: ProjectionImpl("mercator", false)
{}

MercatorProjection::~MercatorProjection()
{}

bool MercatorProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, "merc");
}

std::vector<CDMAttribute> MercatorProjection::parametersFromProj4(const std::string& proj4Str)
{
    vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str)) return attrs;

    attrs.push_back(CDMAttribute("grid_mapping_name", "mercator"));

    // longitude at origin
    double longOfProjOrigin = 0.;
    std::smatch what;
    if (std::regex_search(proj4Str, what, std::regex("\\+lon_0=(\\S+)"))) {
        longOfProjOrigin = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("longitude_of_projection_origin", longOfProjOrigin));

    // standard_paralll or scale_factor
    if (std::regex_search(proj4Str, what, std::regex("\\+lat_ts=(\\S+)"))) {
        double standardParallel = string2type<double>(what[1].str());
        attrs.push_back(CDMAttribute("standard_parallel", standardParallel));
    }
    if (std::regex_search(proj4Str, what, std::regex("\\+k=(\\S+)"))) {
        attrs.push_back(CDMAttribute("scale_factor_at_projection_origin", string2type<double>(what[1].str())));
    }

    proj4GetEarthAttributes(proj4Str, attrs);
    attrs.push_back(CDMAttribute("proj4", proj4Str));
    return attrs;
}

std::ostream& MercatorProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    oproj << "+proj=merc";
    addParameterToStream(oproj, "longitude_of_projection_origin", " +lon_0=");
    addParameterToStream(oproj, "standard_parallel", " +lat_ts=");
    addParameterToStream(oproj, "scale_factor_at_projection_origin", " +k_0=");
    return oproj;
}

}
