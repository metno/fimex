/*
 * Fimex, UnknownToFgdcProjection.cc
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

#include "fimex/coordSys/UnknownToFgdcProjection.h"
#include "fimex/Utils.h"
#include <regex>

namespace MetNoFimex
{

using namespace std;

UnknownToFgdcProjection::UnknownToFgdcProjection()
: ProjectionImpl("unknown_to_fgdc", false)
{
}

bool UnknownToFgdcProjection::acceptsProj4(const std::string& proj4Str)
{
    return std::regex_search(proj4Str, std::regex("\\+proj=(\\S+)"));
}

std::vector<CDMAttribute> UnknownToFgdcProjection::parametersFromProj4(const std::string& proj4Str)
{
    vector<CDMAttribute> attrs;
    if (!acceptsProj4(proj4Str)) return attrs;

    attrs.push_back(CDMAttribute("grid_mapping_name", "unknown_to_fgdc"));

    proj4GetEarthAttributes(proj4Str, attrs);
    attrs.push_back(CDMAttribute("proj4", proj4Str));
    return attrs;
}

std::ostream& UnknownToFgdcProjection::getProj4ProjectionPart(std::ostream& oproj) const
{
    throw CDMException("UnknownToFgdcProjection does not have any valid parameters to translate, everything must be given in the proj4-str ");
    return oproj;
}

}
