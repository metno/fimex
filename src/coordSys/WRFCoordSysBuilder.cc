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
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/CDM.h"
#include "fimex/CDMAttribute.h"
#include "fimex/Data.h"
#include "fimex/mifi_constants.h"
#include "fimex/Logger.h"
#include "fimex/interpolation.h"
#include "proj_api.h"
#include <cmath>
#include <vector>
#include <map>

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
    using namespace std;
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
    proj4 << " +R="<<MIFI_EARTH_RADIUS_M<< " +no_defs";
    boost::shared_ptr<Projection> proj = Projection::createByProj4(proj4.str());
    double centerX = centralLon * DEG_TO_RAD;
    double centerY = centralLat * DEG_TO_RAD;
    mifi_project_values(MIFI_WGS84_LATLON_PROJ4, proj4.str().c_str(), &centerX, &centerY, 1);
    // build axes and staggered coordinate system
    CoordinateSystem::AxisPtr timeAxis, westAxis, stagWestAxis, northAxis, stagNorthAxis;
    if (cdm.hasDimension("west_east")) {
        vector<string> shape;
        shape.push_back("west_east");
        int dx = cdm.getAttribute(cdm.globalAttributeNS(), "DX").getData()->asInt()[0];
        size_t dimSize = cdm.getDimension(shape.at(0)).getLength();
        boost::shared_array<float> vals(new float[dimSize]);
        for (size_t i = 0; i < dimSize; i++) {
            vals[i] = centerX - dx * (dimSize - 1) / 2;
        }
        cdm.addVariable(CDMVariable(shape.at(0),CDM_FLOAT, shape));
        cdm.addAttribute(shape.at(0), CDMAttribute("units", "m"));
        cdm.addAttribute(shape.at(0), CDMAttribute("standard_name", "projection_x_axis"));
        cdm.getVariable(shape.at(0)).setData(createData(dimSize, vals));
        westAxis = CoordinateSystem::AxisPtr(new CoordinateAxis(cdm.getVariable(shape.at(0))));
        westAxis->setAxisType(CoordinateAxis::GeoX);
    }
    if (cdm.hasDimension("west_east_stag")) {
        vector<string> shape;
        shape.push_back("west_east_stag");
        int dx = cdm.getAttribute(cdm.globalAttributeNS(), "DX").getData()->asInt()[0];
        size_t dimSize = cdm.getDimension(shape.at(0)).getLength();
        boost::shared_array<float> vals(new float[dimSize]);
        for (size_t i = 0; i < dimSize; i++) {
            vals[i] = centerX - dx * (dimSize - 1) / 2;
        }
        cdm.addVariable(CDMVariable(shape.at(0),CDM_FLOAT, shape));
        cdm.addAttribute(shape.at(0), CDMAttribute("units", "m"));
        cdm.addAttribute(shape.at(0), CDMAttribute("standard_name", "projection_x_axis"));
        cdm.getVariable(shape.at(0)).setData(createData(dimSize, vals));
        stagWestAxis = CoordinateSystem::AxisPtr(new CoordinateAxis(cdm.getVariable(shape.at(0))));
        stagWestAxis->setAxisType(CoordinateAxis::GeoX);
    }
    if (cdm.hasDimension("south_north")) {
        vector<string> shape;
        shape.push_back("south_north");
        int dx = cdm.getAttribute(cdm.globalAttributeNS(), "DY").getData()->asInt()[0];
        size_t dimSize = cdm.getDimension(shape.at(0)).getLength();
        boost::shared_array<float> vals(new float[dimSize]);
        for (size_t i = 0; i < dimSize; i++) {
            vals[i] = centerX - dx * (dimSize - 1) / 2;
        }
        cdm.addVariable(CDMVariable(shape.at(0),CDM_FLOAT, shape));
        cdm.addAttribute(shape.at(0), CDMAttribute("units", "m"));
        cdm.addAttribute(shape.at(0), CDMAttribute("standard_name", "projection_y_axis"));
        cdm.getVariable(shape.at(0)).setData(createData(dimSize, vals));
        northAxis = CoordinateSystem::AxisPtr(new CoordinateAxis(cdm.getVariable(shape.at(0))));
        northAxis->setAxisType(CoordinateAxis::GeoY);
    }
    if (cdm.hasDimension("south_north_stag")) {
        vector<string> shape;
        shape.push_back("south_north_stag");
        int dx = cdm.getAttribute(cdm.globalAttributeNS(), "DY").getData()->asInt()[0];
        size_t dimSize = cdm.getDimension(shape.at(0)).getLength();
        boost::shared_array<float> vals(new float[dimSize]);
        for (size_t i = 0; i < dimSize; i++) {
            vals[i] = centerX - dx * (dimSize - 1) / 2;
        }
        cdm.addVariable(CDMVariable(shape.at(0),CDM_FLOAT, shape));
        cdm.addAttribute(shape.at(0), CDMAttribute("units", "m"));
        cdm.addAttribute(shape.at(0), CDMAttribute("standard_name", "projection_y_axis"));
        cdm.getVariable(shape.at(0)).setData(createData(dimSize, vals));
        stagNorthAxis = CoordinateSystem::AxisPtr(new CoordinateAxis(cdm.getVariable(shape.at(0))));
        stagNorthAxis->setAxisType(CoordinateAxis::GeoY);
    }

    if (cdm.hasDimension("Time")) {
        vector<string> shape;
        shape.push_back("Time");
        string start = cdm.getAttribute(cdm.globalAttributeNS(), "START_DATE").getStringValue();
        vector<string> datetime = tokenize(start, "_");
        string units = "hours since " + datetime.at(0) + " " + datetime.at(1) + " +0000";
        size_t dimSize = cdm.getDimension(shape.at(0)).getLength();
        boost::shared_array<float> vals(new float[dimSize]);
        for (size_t i = 0; i < dimSize; i++) {
            vals[i] = 3 * i; // TODO, this is fake, the time should be read from the data
        }
        cdm.addVariable(CDMVariable(shape.at(0),CDM_FLOAT, shape));
        cdm.addAttribute(shape.at(0), CDMAttribute("units", units));
        cdm.getVariable(shape.at(0)).setData(createData(dimSize, vals));
        // ref-time
        string reftime = "forecast_reference_time";
        cdm.addVariable(CDMVariable(reftime, CDM_INT, vector<string>(0)));
        string ref = cdm.getAttribute(cdm.globalAttributeNS(), "SIMULATION_START_DATE").getStringValue();
        vector<string> refdatetime = tokenize(start, "_");
        string refunits = "hours since " + refdatetime.at(0) + " " + refdatetime.at(1) + " +0000";
        cdm.addAttribute(reftime, CDMAttribute("units", refunits));
        cdm.addAttribute(reftime, CDMAttribute("standard_name", reftime));
        boost::shared_array<float> refvals(new float[0]);
        refvals[0] = 0;
        cdm.getVariable(reftime).setData(createData(0, refvals));
        timeAxis = CoordinateSystem::AxisPtr(new CoordinateAxis(cdm.getVariable(shape.at(0))));
        timeAxis->setAxisType(CoordinateAxis::Time);
    }

    // now, create the coordinate-systems depending on the variables shape
    map<string, boost::shared_ptr<CoordinateSystem> > cs;
    const CDM::VarVec vars = cdm.getVariables();
    for (CDM::VarVec::const_iterator vit = vars.begin(); vit != vars.end(); ++vit) {
        vector<string> shape = vit->getShape();
        // only adding CoordinateSystems for x y * systems
        if (((find(shape.begin(), shape.end(), "west_east") != shape.end()) || (find(shape.begin(), shape.end(), "west_east_stag") != shape.end()))
            && ((find(shape.begin(), shape.end(), "south_north") != shape.end()) || (find(shape.begin(), shape.end(), "south_north_stag") != shape.end()))) {
            string shapeId = join(shape.begin(), shape.end());
            if (cs.find(shapeId) == cs.end()) {
                boost::shared_ptr<CoordinateSystem> coord(new CoordinateSystem(getName()));
                coord->setProjection(proj);
                for (vector<string>::iterator dimIt = shape.begin(); dimIt != shape.end(); dimIt++) {
                    if (*dimIt == "west_east") {
                        coord->setAxis(westAxis);
                    } else if (*dimIt == "west_east_stag") {
                        coord->setAxis(stagWestAxis);
                    } else if (*dimIt == "south_north") {
                        coord->setAxis(northAxis);
                    } else if (*dimIt == "south_north_stag") {
                        coord->setAxis(stagNorthAxis);
                    } else if (*dimIt == "Time") {
                        coord->setAxis(timeAxis);
                    } else {
                        CoordinateSystem::AxisPtr other;
                        if (cdm.hasVariable(*dimIt)) {
                            other = CoordinateSystem::AxisPtr(new CoordinateAxis(cdm.getVariable(*dimIt)));
                            other->setExplicit(true);
                        } else {
                            // add a dimension without a variable with a 'virtual' variable
                            vector<string> myshape(1, *dimIt);
                            other = CoordinateSystem::AxisPtr(new CoordinateAxis(CDMVariable(*dimIt, CDM_INT, myshape)));
                            other->setExplicit(true);
                        }
                        coord->setAxis(other);
                    }
                }
                cs[shapeId] = coord;
            }
            boost::shared_ptr<CoordinateSystem> coord = cs[shapeId];
            coord->setCSFor(vit->getName(), true);
            coord->setComplete(vit->getName(), true);
        }
    }
    for (map<string, boost::shared_ptr<CoordinateSystem> >::iterator csIt = cs.begin(); csIt != cs.end(); ++csIt) {
        coordSys.push_back(csIt->second);
    }


    return coordSys;
}

} /* namespace MetNoFimex */
