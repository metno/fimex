/*
 * Fimex, RotatedLatitudeLongitudeProjection.cc
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

#include "fimex/coordSys/RotatedLatitudeLongitudeProjection.h"
#include <boost/regex.hpp>

namespace MetNoFimex
{

using namespace std;

RotatedLatitudeLongitudeProjection::RotatedLatitudeLongitudeProjection()
: ProjectionImpl("rotated_latitude_longitude", true)
{
}

bool RotatedLatitudeLongitudeProjection::acceptsProj4(const std::string& proj4Str)
{
    if (proj4ProjectionMatchesName(proj4Str, "ob_tran")) {
        boost::smatch what;
        if (boost::regex_search(proj4Str, what, boost::regex("\\+o_proj=(\\S+)"))) {
            string orgProj = what[1].str();
            if (orgProj == "latlong" || orgProj == "longlat" || orgProj == "eqc") {
                return true;
            }
        }
    }
    return false;
}

std::vector<CDMAttribute> RotatedLatitudeLongitudeProjection::parametersFromProj4(const std::string& proj4Str)
{
    vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str)) return attrs;

    attrs.push_back(CDMAttribute("grid_mapping_name", "rotated_latidude_longitude"));
    double north_pole_lat = 90;
    double north_pole_lon = 0;
    boost::smatch what;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+o_lat_p=(\\S+)"))) {
        north_pole_lat = string2type<double>(what[1].str());
    }
    // ignore optional o_lon_b (rotation after lat rotation)
    // since it doesn't match with FGDC parameters
    // just use lon_0 (lon rotation in the original system)
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lon_0=(\\S+)"))) {
        north_pole_lon = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("grid_north_pole_longitude", 180+north_pole_lon));
    attrs.push_back(CDMAttribute("grid_north_pole_latitude", north_pole_lat));


    proj4GetEarthAttributes(proj4Str, attrs);
    attrs.push_back(CDMAttribute("proj4", proj4Str));
    return attrs;
}

std::ostream& RotatedLatitudeLongitudeProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    oproj << "+proj=ob_tran +o_proj=longlat";
    std::vector<CDMAttribute>::const_iterator foundAttr = std::find_if(params_.begin(), params_.end(), CDMNameEqual("grid_north_pole_longitude"));
    if (foundAttr != params_.end()) {
        if (foundAttr->getData()->size() > 0) {
            oproj << " +lon_0=" << (foundAttr->getData()->asConstDouble()[0]-180);
        }
    }
    addParameterToStream(oproj, "grid_north_pole_latitude", " +o_lat_p=");
    addParameterToStream(oproj, "north_pole_grid_longitude", " +o_lon_b=");
    return oproj;
}

}

