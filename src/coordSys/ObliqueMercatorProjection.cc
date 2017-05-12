/*
 * Fimex, ObliqueMercatorProjection.cc
 *
 * (C) Copyright 2017, SMHI
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

#include "fimex/coordSys/ObliqueMercatorProjection.h"
#include <boost/regex.hpp>
#include <proj_api.h>
#include "fimex/Utils.h"
#include "fimex/Logger.h"

namespace MetNoFimex
{
    using namespace std;

static LoggerPtr logger = getLogger("fimex.ObliqueMercatorProjection");

ObliqueMercatorProjection::ObliqueMercatorProjection()
: ProjectionImpl("oblique_mercator", false)
{}

ObliqueMercatorProjection::~ObliqueMercatorProjection()
{}

bool ObliqueMercatorProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, "omerc");
}

std::vector<CDMAttribute> ObliqueMercatorProjection::parametersFromProj4(const std::string& proj4Str)
{
    vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str)) return attrs;

    attrs.push_back(CDMAttribute("grid_mapping_name", "oblique_mercator"));

    // longitude at origin
    double longOfProjOrigin = 0.;
    boost::smatch what;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lonc=(\\S+)"))) {
        longOfProjOrigin = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("longitude_of_projection_origin", longOfProjOrigin));

    // standard_paralll or scale_factor
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lat_0=(\\S+)"))) {
        double latOfProjOrigin = string2type<double>(what[1].str());
        attrs.push_back(CDMAttribute("latitude_of_projection_origin", latOfProjOrigin));
    }

    // azimuth of central line
    double azimuthOfCentralLine = 0.;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+alpha=(\\S+)"))) {
        azimuthOfCentralLine = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("azimuth_of_central_line", azimuthOfCentralLine));


    if (boost::regex_search(proj4Str, what, boost::regex("\\+k=(\\S+)"))) {
        attrs.push_back(CDMAttribute("scale_factor_at_projection_origin", string2type<double>(what[1].str())));
    }

    if (boost::regex_search(proj4Str, what, boost::regex("\\+no_rot"))) {
            attrs.push_back(CDMAttribute("no_rotation", ""));
        }

    proj4GetEarthAttributes(proj4Str, attrs);
    attrs.push_back(CDMAttribute("proj4", proj4Str));
    return attrs;
}

std::ostream& ObliqueMercatorProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    oproj << "+proj=omerc";
    addParameterToStream(oproj, "longitude_of_projection_origin", " +lonc=");
    addParameterToStream(oproj, "latitude_of_projection_origin", " +lat_0=");
    addParameterToStream(oproj, "azimuth_of_central_line", " +alpha=");
    addParameterToStream(oproj, "scale_factor_at_projection_origin", " +k_0=");
    addParameterToStream(oproj, "no_rotation", " +no_rot");
    return oproj;
}

}
