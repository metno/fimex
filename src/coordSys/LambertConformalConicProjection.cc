/*
 * Fimex, LambertConformalConicProjection.cc
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

#include "fimex/coordSys/LambertConformalConicProjection.h"
#include <boost/regex.hpp>

namespace MetNoFimex
{

using namespace std;

LambertConformalConicProjection::LambertConformalConicProjection()
: ProjectionImpl("lambert_conformal_conic", false)
{
}

bool LambertConformalConicProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, "lcc");
}

std::vector<CDMAttribute> LambertConformalConicProjection::parametersFromProj4(const std::string& proj4Str)
{
    vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str)) return attrs;

    attrs.push_back(CDMAttribute("grid_mapping_name", "lambert_conformal_conic"));

    double lat1 = 0.;
    double lat2 = 0.;
    boost::smatch what;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lat_1=(\\S+)"))) {
        lat1 = string2type<double>(what[1].str());
    }
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lat_2=(\\S+)"))) {
        lat2 = string2type<double>(what[1].str());
    }
    if (lat1 == lat2) {
        attrs.push_back(CDMAttribute("standard_parallel", lat1));
    } else {
        boost::shared_ptr<Data> stdParallels = createData(CDM_DOUBLE, 2);
        stdParallels->setValue(0, lat1);
        stdParallels->setValue(1, lat2);
        CDMAttribute("standard_parallel", CDM_DOUBLE, stdParallels);
    }
    attrs.push_back(CDMAttribute("scale_factor_at_projection_origin", string2type<double>(what[1].str())));

    double lon0 = 0.;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lon_0=(\\S+)"))) {
        lon0 = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("longitude_of_central_meridian", lon0));

    double lat0 = 0.;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lat_0=(\\S+)"))) {
        lat0 = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("latitude_of_projection_origin", lat0));

    proj4GetEarthAttributes(proj4Str, attrs);
    return attrs;
}

std::ostream& LambertConformalConicProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    oproj << "+proj=lcc";
    std::vector<CDMAttribute>::const_iterator foundAttr = std::find_if(params_.begin(), params_.end(), CDMNameEqual("standard_parallel"));
    if (foundAttr != params_.end()) {
        // standard_parallel - There may be 1 or 2 values.
        boost::shared_ptr<Data> spData = foundAttr->getData();
        oproj << " +lat_1=" << spData->asConstDouble()[0];
        if (spData->size() > 2) {
            oproj << " +lat_2=" << spData->asConstDouble()[1];
        } else {
            oproj << " +lat_2=" << spData->asConstDouble()[0];
        }
    } else {
        oproj << " +lat_1=0 +lat_2=0";
    }
    addParameterToStream(oproj, "longitude_of_central_meridian", " +lon_0=");
    addParameterToStream(oproj, "latitude_of_projection_origin", " +lat_0=");

    return oproj;
}

}
