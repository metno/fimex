/*
 * Fimex, LatitudeLongitudeProjection.cc
 *
 * (C) Copyright 2010-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

#include "fimex/coordSys/LatitudeLongitudeProjection.h"

#include <ostream>

namespace MetNoFimex {

std::string LatitudeLongitudeProjection::NAME()
{
    return "latitude_longitude";
}

LatitudeLongitudeProjection::LatitudeLongitudeProjection()
: ProjectionImpl(LatitudeLongitudeProjection::NAME(), true)
{
}

LatitudeLongitudeProjection::~LatitudeLongitudeProjection() {}

bool LatitudeLongitudeProjection::acceptsProj4(const std::string& proj4Str)
{
    return proj4ProjectionMatchesName(proj4Str, "latlong") || proj4ProjectionMatchesName(proj4Str, "longlat");
}

std::vector<CDMAttribute> LatitudeLongitudeProjection::parametersFromProj4(const std::string& proj4Str)
{
    std::vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str))
        return attrs;

    attrs.push_back(CDMAttribute("grid_mapping_name", "latitude_longitude"));

    proj4GetEarthAttributes(proj4Str, attrs);
    return attrs;
}

std::ostream& LatitudeLongitudeProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    return oproj << "+proj=latlong";
}

} // namespace MetNoFimex
