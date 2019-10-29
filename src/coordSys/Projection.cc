/*
 * Fimex, Projection.cc
 *
 * (C) Copyright 2009-2019, met.no
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

#include "fimex/CDMException.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/StringUtils.h"
#include "fimex/interpolation.h"

// list over supported projections
#include "fimex/coordSys/AlbersConicalEqualAreaProjection.h"
#include "fimex/coordSys/AzimuthalEquidistantProjection.h"
#include "fimex/coordSys/GeostationaryProjection.h"
#include "fimex/coordSys/LambertAzimuthalEqualAreaProjection.h"
#include "fimex/coordSys/LambertConformalConicProjection.h"
#include "fimex/coordSys/LambertCylindricalEqualAreaProjection.h"
#include "fimex/coordSys/LatitudeLongitudeProjection.h"
#include "fimex/coordSys/MercatorProjection.h"
#include "fimex/coordSys/ObliqueMercatorProjection.h"
#include "fimex/coordSys/OrthographicProjection.h"
#include "fimex/coordSys/PolarStereographicProjection.h"
#include "fimex/coordSys/RotatedLatitudeLongitudeProjection.h"
#include "fimex/coordSys/SinusoidalProjection.h"
#include "fimex/coordSys/StereographicProjection.h"
#include "fimex/coordSys/TransverseMercatorProjection.h"
#include "fimex/coordSys/UnknownToFgdcProjection.h"
#include "fimex/coordSys/VerticalPerspectiveProjection.h"

#include <memory>
#include <regex>

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.Projection");

bool Projection::operator==(const  Projection& b) const
{
    return toString() == b.toString();
}

std::ostream& operator<<(std::ostream& out, const Projection& proj)
{
    out << proj.toString();
    return out;
}

Projection::~Projection() {}

void Projection::convertToLonLat(std::vector<double>& xVals, std::vector<double>& yVals) const
{
    // check input
    if (xVals.empty())
        return;
    if (xVals.size() != yVals.size())
        throw CDMException("convertToLonLat: xVals.size() != yVals.size()");

    // convert to radian if required
    if (isDegree()) {
        transform_deg_to_rad(xVals);
        transform_deg_to_rad(yVals);
    }

    // run projection
    std::string fromProj = getProj4String();
    std::string toProj = "+proj=latlong " + getProj4EarthString();
    if (fromProj == toProj)
        return;
    if (MIFI_OK != mifi_project_values(fromProj.c_str(), toProj.c_str(), &xVals[0], &yVals[0], static_cast<int>(xVals.size()))) {
        throw CDMException("convertToLonLat: unable to convert from '" +fromProj + "' to '"+toProj+"'");
    }

    // convert to degree
    transform_rad_to_deg(xVals);
    transform_rad_to_deg(yVals);
}

void Projection::convertFromLonLat(std::vector<double>& xVals, std::vector<double>& yVals) const
{
    // check input
    if (xVals.empty())
        return;
    if (xVals.size() != yVals.size())
        throw CDMException("convertFromLonLat: xVals.size() != yVals.size()");

    // convert to radian
    transform_deg_to_rad(xVals);
    transform_deg_to_rad(yVals);

    // run projection
    std::string fromProj = "+proj=latlong " + getProj4EarthString();
    std::string toProj = getProj4String();
    if (fromProj == toProj)
        return;
    if (MIFI_OK != mifi_project_values(fromProj.c_str(), toProj.c_str(), &xVals[0], &yVals[0], static_cast<int>(xVals.size()))) {
        throw CDMException("convertFromLonLat: unable to convert from '" +fromProj + "' to '"+toProj+"'");
    }

    // convert to degree if required
    if (isDegree()) {
        transform_rad_to_deg(xVals);
        transform_rad_to_deg(yVals);
    }
}

Projection_p Projection::create(const std::vector<CDMAttribute>& attrs)
{
    std::vector<CDMAttribute>::const_iterator projAttr = std::find_if(attrs.begin(), attrs.end(), CDMNameEqual("grid_mapping_name"));
    Projection_p proj;
    if (projAttr == attrs.end()) {
        proj = std::make_shared<LatitudeLongitudeProjection>();
    } else {
        std::string projName(projAttr->getStringValue());
        if (projName == "albers_conical_equal_area") {
            proj = std::make_shared<AlbersConicalEqualAreaProjection>();
        } else if (projName == "azimuthal_equidistant") {
            proj = std::make_shared<AzimuthalEquidistantProjection>();
        } else if (projName == "geostationary") {
            proj = std::make_shared<GeostationaryProjection>();
        } else if (projName == "lambert_azimuthal_equal_area") {
            proj = std::make_shared<LambertAzimuthalEqualAreaProjection>();
        } else if (projName == "lambert_conformal_conic") {
            proj = std::make_shared<LambertConformalConicProjection>();
        } else if (projName == "lambert_cylindrical_equal_area") {
            proj = std::make_shared<LambertCylindricalEqualAreaProjection>();
        } else if (projName == "latitude_longitude") {
            proj = std::make_shared<LatitudeLongitudeProjection>();
        } else if (projName == "mercator") {
            proj = std::make_shared<MercatorProjection>();
        } else if (projName == "oblique_mercator") {
            proj = std::make_shared<ObliqueMercatorProjection>();
        } else if (projName == "orthographic") {
            proj = std::make_shared<OrthographicProjection>();
        } else if (projName == "polar_stereographic") {
            proj = std::make_shared<PolarStereographicProjection>();
        } else if (projName == "rotated_latitude_longitude") {
            proj = std::make_shared<RotatedLatitudeLongitudeProjection>();
        } else if (projName == "sinusoidal") {
            proj = std::make_shared<SinusoidalProjection>();
        } else if (projName == "stereographic") {
            proj = std::make_shared<StereographicProjection>();
        } else if (projName == "transverse_mercator") {
            proj = std::make_shared<TransverseMercatorProjection>();
        } else if (projName == "vertical_perspective") {
            proj = std::make_shared<VerticalPerspectiveProjection>();
        } else if (projName == "unknown_to_fgdc") {
            proj = std::make_shared<UnknownToFgdcProjection>();
        } else {
            throw CDMException("unsupported projection: " + projName);
        }
    }
    proj->addParameters(attrs);
    return proj;
}

Projection_p Projection::createByProj4(const std::string& projStr)
{
    std::vector<CDMAttribute> attrs;
    if (AlbersConicalEqualAreaProjection::acceptsProj4(projStr)) {
        attrs = AlbersConicalEqualAreaProjection::parametersFromProj4(projStr);
    } else if (AzimuthalEquidistantProjection::acceptsProj4(projStr)) {
        attrs = AzimuthalEquidistantProjection::parametersFromProj4(projStr);
    } else if (GeostationaryProjection::acceptsProj4(projStr)) {
        attrs = GeostationaryProjection::parametersFromProj4(projStr);
    } else if (LambertAzimuthalEqualAreaProjection::acceptsProj4(projStr)) {
        attrs = LambertAzimuthalEqualAreaProjection::parametersFromProj4(projStr);
    } else if (LambertConformalConicProjection::acceptsProj4(projStr)) {
        attrs = LambertConformalConicProjection::parametersFromProj4(projStr);
    } else if (LambertCylindricalEqualAreaProjection::acceptsProj4(projStr)) {
        attrs = LambertCylindricalEqualAreaProjection::parametersFromProj4(projStr);
    } else if (LatitudeLongitudeProjection::acceptsProj4(projStr)) {
        attrs = LatitudeLongitudeProjection::parametersFromProj4(projStr);
    } else if (MercatorProjection::acceptsProj4(projStr)) {
        attrs = MercatorProjection::parametersFromProj4(projStr);
    } else if (ObliqueMercatorProjection::acceptsProj4(projStr)) {
        attrs = ObliqueMercatorProjection::parametersFromProj4(projStr);
    } else if (OrthographicProjection::acceptsProj4(projStr)) {
        attrs = OrthographicProjection::parametersFromProj4(projStr);
    } else if (PolarStereographicProjection::acceptsProj4(projStr)) {
        attrs = PolarStereographicProjection::parametersFromProj4(projStr);
    } else if (RotatedLatitudeLongitudeProjection::acceptsProj4(projStr)) {
        attrs = RotatedLatitudeLongitudeProjection::parametersFromProj4(projStr);
    } else if (SinusoidalProjection::acceptsProj4(projStr)) {
        attrs = SinusoidalProjection::parametersFromProj4(projStr);
    } else if (StereographicProjection::acceptsProj4(projStr)) {
        attrs = StereographicProjection::parametersFromProj4(projStr);
    } else if (TransverseMercatorProjection::acceptsProj4(projStr)) {
        attrs = TransverseMercatorProjection::parametersFromProj4(projStr);
    } else if (VerticalPerspectiveProjection::acceptsProj4(projStr)) {
        attrs = VerticalPerspectiveProjection::parametersFromProj4(projStr);
    } else if (UnknownToFgdcProjection::acceptsProj4(projStr)) {
        LOG4FIMEX(logger, Logger::WARN, "projStr not supported by FGDC, trying UnknownToFgdcProjection for '" << projStr << "'");
        attrs = UnknownToFgdcProjection::parametersFromProj4(projStr);
    }

    if (attrs.size() == 0) {
        LOG4FIMEX(logger, Logger::ERROR, "translation of proj4 '" << projStr << "' to FGDC/CF not supported");
        throw CDMException("proj-string "+projStr+" to FGDC/CF not supported");
    }
    return Projection::create(attrs);
}

std::string replaceProj4Earthfigure(const std::string& proj4, const std::string& newEarthfigure)
{
    using namespace std;
    if (newEarthfigure == "") {
        return proj4;
    }
    vector<string> parts = tokenize(proj4, " ");
    vector<string> newParts;
    for (size_t i = 0; i < parts.size(); i++) {
        if (parts.at(i).find("+a=") != string::npos) continue;
        if (parts.at(i).find("+b=") != string::npos) continue;
        if (parts.at(i).find("+e=") != string::npos) continue;
        if (parts.at(i).find("+R=") != string::npos) continue;
        if (parts.at(i).find("+ellps=") != string::npos) continue;
        if (parts.at(i).find("+datum=") != string::npos) continue;
        newParts.push_back(parts.at(i));
    }
    newParts.push_back(newEarthfigure);
    return join(newParts.begin(), newParts.end(), " ");
}

} // namespace MetNoFimex
