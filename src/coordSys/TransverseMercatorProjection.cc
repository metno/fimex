/*
 * Fimex, TransverseMercatorProjection.cc
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
 *  Created on: Jun 8, 2010
 *      Author: Heiko Klein
 */

#include "fimex/coordSys/TransverseMercatorProjection.h"
#include <boost/regex.hpp>
#include "fimex/Utils.h"
#include "fimex/Logger.h"

namespace MetNoFimex
{
    using namespace std;

static LoggerPtr logger = getLogger("fimex.TransverseMercatorProjection");

TransverseMercatorProjection::TransverseMercatorProjection()
: ProjectionImpl("transverse_mercator", false)
{}

TransverseMercatorProjection::~TransverseMercatorProjection()
{}

bool TransverseMercatorProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, "tmerc") || proj4ProjectionMatchesName(proj4Str, "utm") || proj4ProjectionMatchesName(proj4Str, "gstmerc");
}

std::vector<CDMAttribute> TransverseMercatorProjection::parametersFromProj4(const std::string& proj4Str)
{
    vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str)) return attrs;

    attrs.push_back(CDMAttribute("grid_mapping_name", "transverse_mercator"));

    if (proj4ProjectionMatchesName(proj4Str, "utm")) {
        attrs.push_back(CDMAttribute("scale_factor_at_central_meridian", 0.9996));
        attrs.push_back(CDMAttribute("latitude_of_projection_origin", 0));
        attrs.push_back(CDMAttribute("false_easting", 500000));
        boost::smatch what;
        double longOfProjOrigin = 0;
        if (boost::regex_search(proj4Str, what, boost::regex("\\+zone=(\\d+)"))) {
            short zone = string2type<short>(what[1].str());
            if (zone <= 0) zone = 1;
            else if (zone > 60) zone = 60;
            longOfProjOrigin = (zone-1)*6 - 180 + 3;
            attrs.push_back(CDMAttribute("longitude_of_central_meridian", longOfProjOrigin));
        }
        if (boost::regex_search(proj4Str, what, boost::regex("\\+south"))) {
            attrs.push_back(CDMAttribute("false_northing", 10000000));
        }
        LOG4FIMEX(logger, Logger::WARN, "proj4 utm projection is only valid 6 deg. from center-longitude. Consider using: +proj=gstmerc +k=0.9996 +lon_0="<< longOfProjOrigin << "+x_0=500000");
    } else { // projection given as tmerc or gstmerc
        if (proj4ProjectionMatchesName(proj4Str, "tmerc")) {
            LOG4FIMEX(logger, Logger::WARN, "proj4 tmerc projection is only valid 6 deg. from center-longitude. Consider using: +proj=gstmerc");
        }
        // longitude at origin
        double longOfProjOrigin = 0.;
        boost::smatch what;
        if (boost::regex_search(proj4Str, what, boost::regex("\\+lon_0=(\\S+)"))) {
            longOfProjOrigin = string2type<double>(what[1].str());
        }
        attrs.push_back(CDMAttribute("longitude_of_central_meridian", longOfProjOrigin));

        // standard_paralll or scale_factor
        if (boost::regex_search(proj4Str, what, boost::regex("\\+lat_0=(\\S+)"))) {
            double latOfProjOrigin = string2type<double>(what[1].str());
            attrs.push_back(CDMAttribute("latitude_of_projection_origin", latOfProjOrigin));
        }

        if (boost::regex_search(proj4Str, what, boost::regex("\\+k=(\\S+)"))) {
            attrs.push_back(CDMAttribute("scale_factor_at_central_meridian", string2type<double>(what[1].str())));
        }
    }
    proj4GetEarthAttributes(proj4Str, attrs);
    attrs.push_back(CDMAttribute("proj4", proj4Str));
    return attrs;
}

std::ostream& TransverseMercatorProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    oproj << "+proj=gstmerc";
    addParameterToStream(oproj, "longitude_of_central_meridian", " +lon_0=");
    addParameterToStream(oproj, "latitude_of_projection_origin", " +lat_0=");
    addParameterToStream(oproj, "scale_factor_at_central_meridian", " +k_0=");
    return oproj;
}

}
