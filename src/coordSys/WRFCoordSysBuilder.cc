/*
 * Fimex, WRFCoordSysBuilder.cc
 *
 * (C) Copyright 2012, met.no
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
 *  Created on: Mar 28, 2012
 *      Author: Heiko Klein
 */

#include "WRFCoordSysBuilder.h"
#include "fimex/CDM.h"
#include "fimex/CDMAttribute.h"
#include "fimex/Data.h"
#include "fimex/mifi_constants.h"
#include "fimex/Logger.h"
#include "proj_api.h"
#include <cmath>

namespace MetNoFimex
{

WRFCoordSysBuilder::WRFCoordSysBuilder()
{
}

WRFCoordSysBuilder::~WRFCoordSysBuilder()
{
}

bool WRFCoordSysBuilder::isMine(const CDM& cdm)
{
    if (!cdm.hasDimension("south_north"))
        return false;

    CDMAttribute attr;
    if (!cdm.getAttribute(cdm.globalAttributeNS(), "MAP_PROJ", attr))
        return false;

    return true;
}

static double findAttributeDouble(const CDM& cdm, std::string attName)
{
    CDMAttribute attr;
    if (!cdm.getAttribute(cdm.globalAttributeNS(), "MAP_PROJ", attr))
        return MIFI_UNDEFINED_D;
    else
        return attr.getData()->asDouble()[0];

}

std::vector<boost::shared_ptr<const CoordinateSystem> > WRFCoordSysBuilder::listCoordinateSystems(CDM& cdm)
{
    LoggerPtr logger = getLogger("fimex.coordSys.WRFCoordSysBuilder");
    std::vector<boost::shared_ptr<const CoordinateSystem> > coordSys;
    if (!isMine(cdm))
        return coordSys;
    /*
    The definition for map projection options:
    map_proj =  1: Lambert Conformal
                2: Polar Stereographic
                3: Mercator
                6: latitude and longitude (including global)
     */
    double lat1 = findAttributeDouble(cdm, "TRUELAT1");
    double lat2 = findAttributeDouble(cdm, "TRUELAT2");
    double centralLat = findAttributeDouble(cdm, "CEN_LAT");  // center of grid
    double centralLon = findAttributeDouble(cdm, "CEN_LON");  // center of grid
    double standardLon = findAttributeDouble(cdm, "STAND_LON"); // true longitude
    double standardLat = findAttributeDouble(cdm, "MOAD_CEN_LAT");

    std::stringstream proj4;
    int map_proj = cdm.getAttribute(cdm.globalAttributeNS(), "MAP_PROJ").getData()->asInt()[0];
    switch (map_proj) {
    case 1:
        proj4 << "+proj=lcc +lon_0="<<standardLon<<" +lat_0="<<standardLat<<" +lat_1="<<lat1<<" +lat_2="<<lat2;
        break;
    case 2:
        proj4 << "+proj=stere";
        {
            double lon0 = (mifi_isnand(standardLon)) ? centralLon : standardLon;
            double lat0 = (mifi_isnand(centralLat)) ? lat2 : centralLat;
            double k = (1+fabs(sin(DEG_TO_RAD*lat1)))/2.;
            proj4 << " +lon_0="<<lon0<<" +lat_0="<<lat0<<" +k="<<k;
        }
        break;
    case 3:
        proj4 << "+proj=merc +lon_0="<<standardLon<<" +lat_0="<<standardLat;
        break;
    default:
        LOG4FIMEX(logger, Logger::WARN, "unknown projection-id: " << map_proj);
        return coordSys;
    }
    boost::shared_ptr<Projection> proj = Projection::createByProj4(proj4.str());
    // TODO: build axes and staggered coordinate system
    return coordSys;
}

} /* namespace MetNoFimex */
