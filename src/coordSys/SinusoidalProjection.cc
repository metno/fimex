/*
 * Fimex, SinusoidalProjection.cc
 *
 * Copyright (C) 2019-2022 met.no
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
 */

#include "fimex/coordSys/SinusoidalProjection.h"

#include "fimex/Logger.h"
#include "fimex/String2Type.h"

#include <regex>

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.SinusoidalProjection");

static const std::string CF_NAME = "sinusoidal";
static const std::string PROJ_NAME = "sinu";

SinusoidalProjection::SinusoidalProjection()
    : ProjectionImpl(CF_NAME, false)
{
}

SinusoidalProjection::~SinusoidalProjection() {}

bool SinusoidalProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, PROJ_NAME);
}

std::vector<CDMAttribute> SinusoidalProjection::parametersFromProj4(const std::string& proj4Str)
{
    std::vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str))
        return attrs;

    attrs.push_back(CDMAttribute("grid_mapping_name", CF_NAME));

    // longitude at origin
    double longOfProjOrigin = 0.;
    std::smatch what;
    if (std::regex_search(proj4Str, what, std::regex("\\+lon_0=(\\S+)"))) {
        longOfProjOrigin = string2type<double>(what[1].str());
    }
    attrs.push_back(CDMAttribute("longitude_of_projection_origin", longOfProjOrigin));

    proj4GetEarthAttributes(proj4Str, attrs);
    attrs.push_back(CDMAttribute("proj4", proj4Str));
    return attrs;
}

std::ostream& SinusoidalProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    oproj << "+proj=" + PROJ_NAME;
    addParameterToStream(oproj, "longitude_of_projection_origin", " +lon_0=");
    return oproj;
}

} // namespace MetNoFimex
