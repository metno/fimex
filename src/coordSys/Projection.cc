/*
 * Fimex, Projection.cc
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Apr 27, 2010
 *      Author: Heiko Klein
 */

#include "fimex/coordSys/Projection.h"
#include <boost/regex.hpp>

// list over supported projections
#include "StereographicProjection.h"
#include "PolarStereographicProjection.h"
#include "LatitudeLongitudeProjection.h"
#include "LambertConformalConicProjection.h"
#include "RotatedLatitudeLongitudeProjection.h"

// include projects.h since I need to access some of projs internals (proj -V)
// PJ_LIB__ required for LP.phi, LP.lam
#include <projects.h>


namespace MetNoFimex
{

bool Projection::operator==(const  Projection& b) const
{
    return toString() == b.toString();
}


std::ostream& operator<<(std::ostream& out, const Projection& proj)
{
    out << proj.toString();
    return out;
}


boost::shared_ptr<Projection> Projection::create(std::vector<CDMAttribute> attrs)
{
    std::vector<CDMAttribute>::const_iterator projAttr = std::find_if(attrs.begin(), attrs.end(), CDMNameEqual("grid_mapping_name"));
    boost::shared_ptr<Projection> proj;
    if (projAttr == attrs.end()) {
        proj = boost::shared_ptr<Projection>(new LatitudeLongitudeProjection());
    } else {
        std::string projName(projAttr->getStringValue());
        if (projName == "stereographic") {
            proj =  boost::shared_ptr<Projection>(new StereographicProjection());
        } else if (projName == "polar_stereographic") {
            proj =  boost::shared_ptr<Projection>(new PolarStereographicProjection());
        } else if (projName == "rotated_latitude_longitude") {
            proj =  boost::shared_ptr<Projection>(new RotatedLatitudeLongitudeProjection());
        } else if (projName == "lambert_conformal_conic") {
            proj =  boost::shared_ptr<Projection>(new LambertConformalConicProjection());
        } else {
            throw CDMException("unsupported projection: " + projName);
        }
    }
    proj->addParameters(attrs);
    return proj;
}

boost::shared_ptr<Projection> Projection::createByProj4(const std::string& projStr)
{
    std::vector<CDMAttribute> attrs;
    if (LatitudeLongitudeProjection::acceptsProj4(projStr)) {
        attrs = LatitudeLongitudeProjection::parametersFromProj4(projStr);
    } else if (RotatedLatitudeLongitudeProjection::acceptsProj4(projStr)) {
        attrs = RotatedLatitudeLongitudeProjection::parametersFromProj4(projStr);
    } else if (PolarStereographicProjection::acceptsProj4(projStr)) {
        attrs = PolarStereographicProjection::parametersFromProj4(projStr);
    } else if (StereographicProjection::acceptsProj4(projStr)) {
        attrs = StereographicProjection::parametersFromProj4(projStr);
    } else if (LambertConformalConicProjection::acceptsProj4(projStr)) {
        attrs = LambertConformalConicProjection::parametersFromProj4(projStr);
    }

    if (attrs.size() == 0) {
        std::cerr << "translation of proj4 '" << projStr << "' to FGDC/CF not supported" << std::endl;
        throw CDMException("proj-string "+projStr+" to FGDC/CF not supported");
    }
    return Projection::create(attrs);
}

#if 0
boost::shared_ptr<Projection> Projection::createByProj4(const std::string& projStr)
{
    // TODO: this function should be splitted and moved to the projection implementation classes
    // in the same way as the create function

    // init projections
    // make sure that pj is freed when going out of scope
    boost::shared_ptr<PJ> pj(pj_init_plus(projStr.c_str()), pj_free);
    if (!pj.get()) {
        std::cerr << "pj_init error: " << pj_errno << " " << pj_strerrno(pj_errno) << std::endl;
        throw std::exception(); // not initialized
    }
    LP lp, lpOrg;
    boost::smatch what;
    if (boost::regex_search(projStr, what, boost::regex("\\+lon_0=(\\S+)"))) {
        lpOrg.u = std::strtod(what[1].str().c_str(), (char **)NULL);
        lp.u = DEG_TO_RAD * lpOrg.u;
    } else {
        lpOrg.u = 0;
        lp.u = 0;
    }
    if (boost::regex_search(projStr, what, boost::regex("\\+lat_0=(\\S+)"))) {
        lpOrg.v = std::strtod(what[1].str().c_str(), (char **)NULL);
        lp.v = DEG_TO_RAD * lpOrg.v;
        // work around HALFPI which is singularity in proj (Proj BUGZILLA: 1605)
        double delta = std::fabs(lp.v) - HALFPI;
        const double DerivDelta(1e-5);
        if (std::fabs(delta) < DerivDelta) {
            lp.v = (lp.v > 0) ? (HALFPI - DerivDelta) : (-1*HALFPI + DerivDelta);
        }
    } else {
        lpOrg.v = 0;
        lp.v = 0;
    }

    std::string projType;
    if (boost::regex_search(projStr, what, boost::regex("\\+proj=(\\S+)"))) {
        projType = what[1].str();
    } else {
        std::cerr << "no projection found" << std::endl;
        throw std::exception();
    }

    std::vector<CDMAttribute> attrList;
    attrList.push_back(CDMAttribute("proj4", projStr));
    if (projType == "stere") {
        // stereographic projection
        // pj_factors
        FACTORS factors;
        factors.code = 0; // flag what to calculate. 0 means calc everything? undocumented, default: random
        if (pj_factors(lp, pj.get(), 0., &factors) != 0) {
            std::cerr << "pj_factors error: " << pj_errno << " " << pj_strerrno(pj_errno) << std::endl;
            throw std::exception();
        }
        attrList.push_back(CDMAttribute("grid_mapping_name", "stereographic"));
        attrList.push_back(CDMAttribute("scale_factor_at_projection_origin", factors.k));
        attrList.push_back(CDMAttribute("longitude_of_projection_origin", lpOrg.u));
        attrList.push_back(CDMAttribute("latitude_of_projection_origin", lpOrg.v));
    } else if (projType == "ob_tran") {
        // rotated pole, at least for felt?
        std::string orgProj;
        if (boost::regex_search(projStr, what, boost::regex("\\+o_proj=(\\S+)"))) {
            orgProj = what[1].str();
        } else {
            std::cerr << "no o_proj found" << std::endl;
            throw std::exception();
        }
        if (orgProj == "latlong" || orgProj == "longlat" || orgProj == "eqc") {
            double north_pole_lat = 90;
            double north_pole_lon = 0;
            if (boost::regex_search(projStr, what, boost::regex("\\+o_lat_p=(\\S+)"))) {
                north_pole_lat = string2type<double>(what[1].str());
            }
            // ignore optional o_lon_b (rotation after lat rotation)
            // since it doesn't match with FGDC parameters
            // just use lon_0 (lon rotation in the original system)
            if (boost::regex_search(projStr, what, boost::regex("\\+lon_0=(\\S+)"))) {
                north_pole_lon = string2type<double>(what[1].str());
            }
            attrList.push_back(CDMAttribute("grid_mapping_name", "rotated_latitude_longitude"));
            attrList.push_back(CDMAttribute("grid_north_pole_longitude", 180+north_pole_lon));
            attrList.push_back(CDMAttribute("grid_north_pole_latitude", north_pole_lat));
        }

    } else if (projType == "merc") {
        attrList.push_back(CDMAttribute("grid_mapping_name", "mercator"));

        // longitude at origin
        double longOfProjOrigin = 0.;
        if (boost::regex_search(projStr, what, boost::regex("\\+lon_0=(\\S+)"))) {
            longOfProjOrigin = string2type<double>(what[1].str());
        }
        attrList.push_back(CDMAttribute("longitude_of_projection_origin", longOfProjOrigin));

        // standard_paralll or scale_factor
        if (boost::regex_search(projStr, what, boost::regex("\\+lat_ts=(\\S+)"))) {
            double standardParallel = string2type<double>(what[1].str());
            attrList.push_back(CDMAttribute("standard_parallel", standardParallel));
        }
        if (boost::regex_search(projStr, what, boost::regex("\\+k=(\\S+)"))) {
            attrList.push_back(CDMAttribute("scale_factor_at_projection_origin", string2type<double>(what[1].str())));
        }
    } else if (projType == "lcc") {
        // lambert conic conformal
        attrList.push_back(CDMAttribute("grid_mapping_name", "lambert_conformal_conic"));

        double lat1 = 0.;
        double lat2 = 0.;
        if (boost::regex_search(projStr, what, boost::regex("\\+lat_1=(\\S+)"))) {
            lat1 = string2type<double>(what[1].str());
        }
        if (boost::regex_search(projStr, what, boost::regex("\\+lat_2=(\\S+)"))) {
            lat2 = string2type<double>(what[1].str());
        }
        if (lat1 == lat2) {
            attrList.push_back(CDMAttribute("standard_parallel", lat1));
        } else {
            boost::shared_ptr<Data> stdParallels = createData(CDM_DOUBLE, 2);
            stdParallels->setValue(0, lat1);
            stdParallels->setValue(1, lat2);
            CDMAttribute("standard_parallel", CDM_DOUBLE, stdParallels);
        }
        attrList.push_back(CDMAttribute("scale_factor_at_projection_origin", string2type<double>(what[1].str())));

        double lon0 = 0.;
        if (boost::regex_search(projStr, what, boost::regex("\\+lon_0=(\\S+)"))) {
            lon0 = string2type<double>(what[1].str());
        }
        attrList.push_back(CDMAttribute("longitude_of_central_meridian", lon0));

        double lat0 = 0.;
        if (boost::regex_search(projStr, what, boost::regex("\\+lat_0=(\\S+)"))) {
            lat0 = string2type<double>(what[1].str());
        }
        attrList.push_back(CDMAttribute("latitude_of_projection_origin", lat0));
    } else {
        std::cerr << "translation of proj4 '" << projStr << "' to FGDC/CF not supported" << std::endl;
        throw CDMException("proj-string "+projStr+" to FGDC/CF not supported");
    }

    // convert x_0 to false_easting, y_0 to false_northing
    if (boost::regex_search(projStr, what, boost::regex("\\+x_0=(\\S+)"))) {
        attrList.push_back(CDMAttribute("false_easting", string2type<double>(what[1].str())));
    }
    if (boost::regex_search(projStr, what, boost::regex("\\+y_0=(\\S+)"))) {
        attrList.push_back(CDMAttribute("false_northing", string2type<double>(what[1].str())));
    }

    boost::shared_ptr<Projection> proj = create(attrList);
    return proj;
}
#endif


}
