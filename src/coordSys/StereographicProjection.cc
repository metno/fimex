/*
 * Fimex, StereographicProjection.cc
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

#include "fimex/coordSys/StereographicProjection.h"
#include <boost/regex.hpp>
#include <cmath>
#include <sstream>
// include projects.h since I need to access some of projs internals (proj -V)
// PJ_LIB__ required for LP.phi, LP.lam
#include <projects.h>


namespace MetNoFimex
{

using namespace std;

StereographicProjection::StereographicProjection()
: ProjectionImpl("stereographic", false)
{
}

bool StereographicProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, "stere");
}

std::vector<CDMAttribute> StereographicProjection::parametersFromProj4(const std::string& proj4Str)
{
    vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str)) return attrs;

    // need to init projections to calculate some factors
    // make sure that pj is freed when going out of scope
    boost::shared_ptr<PJ> pj(pj_init_plus(proj4Str.c_str()), pj_free);
    if (!pj.get()) {
        ostringstream buffer;
        buffer << "pj_init error: " << pj_errno << " " << pj_strerrno(pj_errno) << std::endl;
        throw CDMException(buffer.str());
    }
    LP lp, lpOrg;
    boost::smatch what;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lon_0=(\\S+)"))) {
        lpOrg.u = std::strtod(what[1].str().c_str(), (char **)NULL);
        lp.u = DEG_TO_RAD * lpOrg.u;
    } else {
        lpOrg.u = 0;
        lp.u = 0;
    }
    if (boost::regex_search(proj4Str, what, boost::regex("\\+lat_0=(\\S+)"))) {
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
    FACTORS factors;
    factors.code = 0; // flag what to calculate. 0 means calc everything? undocumented, default: random
    if (pj_factors(lp, pj.get(), 0., &factors) != 0) {
        std::cerr << "pj_factors error: " << pj_errno << " " << pj_strerrno(pj_errno) << std::endl;
        throw std::exception();
    }

    attrs.push_back(CDMAttribute("grid_mapping_name", "stereographic"));
    attrs.push_back(CDMAttribute("scale_factor_at_projection_origin", factors.k));
    attrs.push_back(CDMAttribute("longitude_of_projection_origin", lpOrg.u));
    attrs.push_back(CDMAttribute("latitude_of_projection_origin", lpOrg.v));

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
    addParameterToStream(oproj, "false_easting", " +x_0=");
    addParameterToStream(oproj, "false_northing", " +y_0=");
    return oproj;
}

}
