/*
 * Fimex, WRFCoordSysBuilder.cc
 *
 * (C) Copyright 2012-2022, met.no
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
#include "fimex/CDMReader.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/SliceBuilder.h"
#include "fimex/StringUtils.h"
#include "fimex/TimeUtils.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/interpolation.h"
#include "fimex/mifi_constants.h"

#include "reproject.h"

#include <cassert>
#include <cmath>
#include <map>
#include <memory>
#include <vector>

namespace MetNoFimex
{

static Logger_p logger = getLogger("fimex.WRFCoordSysBuilder");

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

void WRFCoordSysBuilder::enhanceVectorProperties(CDMReader_p reader)
{
    CDM& cdm = reader->getInternalCDM();
    std::vector<std::pair<std::string, std::string> > xy_vectors;
    xy_vectors.push_back(std::make_pair("U", "V"));
    xy_vectors.push_back(std::make_pair("UAH", "VAH"));
    for (size_t i = 0; i < xy_vectors.size(); i++) {
        if (cdm.hasVariable(xy_vectors.at(i).first) && cdm.hasVariable(xy_vectors.at(i).second)) {
            CDMVariable& xv = cdm.getVariable(xy_vectors.at(i).first);
            CDMVariable& yv = cdm.getVariable(xy_vectors.at(i).second);
            if (xv.isSpatialVector()) continue;
            if (yv.isSpatialVector()) continue;
            xv.setAsSpatialVector(yv.getName(), CDMVariable::SPATIAL_VECTOR_X);
            yv.setAsSpatialVector(xv.getName(), CDMVariable::SPATIAL_VECTOR_Y);
            LOG4FIMEX(logger, Logger::INFO, "making "<< xv.getName() << "," << yv.getName() << " a vector");
        }
    }
}

static double findAttributeDouble(const CDM& cdm, std::string attName)
{
    CDMAttribute attr;
    if (!cdm.getAttribute(cdm.globalAttributeNS(), attName, attr))
        return MIFI_UNDEFINED_D;
    else
        return attr.getData()->asDouble()[0];

}

// function for compatibility between the old (only CDM) and new (only CDMReader) interface
static CoordinateSystem_cp_v wrfListCoordinateSystems(CDM& cdm, CDMReader_p reader)
{
    assert(reader.get() != 0);
    // reader might be 0
    using namespace std;
    CoordinateSystem_cp_v coordSys;
    if (!WRFCoordSysBuilder().isMine(cdm))
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
    double poleLat = findAttributeDouble(cdm, "POLE_LAT");
    double poleLon = findAttributeDouble(cdm, "POLE_LON");

    std::stringstream proj4;
    bool isLatLon = false;
    int map_proj = cdm.getAttribute(cdm.globalAttributeNS(), "MAP_PROJ").getData()->asInt()[0];
    switch (map_proj) {
    case 1:
        proj4 << "+proj=lcc +lon_0="<<standardLon<<" +lat_0="<<standardLat<<" +lat_1="<<lat1<<" +lat_2="<<lat2;
        break;
    case 2:
        proj4 << "+proj=stere";
        {
            double lon0 = (mifi_isnan(standardLon)) ? centralLon : standardLon;
            double lat0 = (mifi_isnan(centralLat)) ? lat2 : centralLat;
            double k = (1 + fabs(sin(deg_to_rad(lat1)))) / 2.;
            proj4 << " +lon_0="<<lon0<<" +lat_0="<<lat0<<" +k="<<k;
        }
        break;
    case 3:
        proj4 << "+proj=merc +lon_0="<<standardLon<<" +lat_0="<<standardLat;
        break;
    case 6:
        // rotert sperical
        proj4 << "+proj=ob_tran +o_proj=longlat +lon_0="<<(-1*standardLon) << " +o_lon_p=" << (180-poleLon) << " +o_lat_p=" << poleLat;
        isLatLon = true;
        break;
    default:
        LOG4FIMEX(logger, Logger::WARN, "unknown projection-id: " << map_proj);
        return coordSys;
    }
    const int R0 = 6370000; // WRF earth radius = 6370km
    proj4 << " +R=" << R0 << " +no_defs";
    //cerr << proj4.str() << endl;
    Projection_p proj = Projection::createByProj4(proj4.str());
    double centerX = deg_to_rad(centralLon);
    double centerY = deg_to_rad(centralLat);
    reproject::reproject_values(MIFI_WGS84_LATLON_PROJ4, proj4.str(), &centerX, &centerY, 1);
    if (isLatLon) {
        centerX = rad_to_deg(centerX);
        centerY = rad_to_deg(centerY);
    }

    // TODO: the following variables might be called uninitialized when the coordinate-system
    //       is build twice

    // build axes and staggered coordinate system
    CoordinateAxis_p timeAxis, westAxis, stagWestAxis, northAxis, stagNorthAxis, bottomAxis, stagBottomAxis;
    if (cdm.hasDimension("west_east")) {
        vector<string> shape;
        shape.push_back("west_east");
        if (!cdm.hasVariable(shape.at(0))) {
            float dx = cdm.getAttribute(cdm.globalAttributeNS(), "DX").getData()->asFloat()[0];
            if (isLatLon) {
                dx = rad_to_deg(dx) / R0;
            }
            size_t dimSize = cdm.getDimension(shape.at(0)).getLength();
            double startX = centerX - dx * (dimSize - 1) / 2;
            shared_array<float> vals(new float[dimSize]);
            for (size_t i = 0; i < dimSize; i++) {
                vals[i] = startX + i * dx;
            }
            cdm.addVariable(CDMVariable(shape.at(0),CDM_FLOAT, shape));
            if (isLatLon) {
                cdm.addAttribute(shape.at(0), CDMAttribute("units", "degree"));
                cdm.addAttribute(shape.at(0), CDMAttribute("standard_name", "grid_longitude"));
            } else {
                cdm.addAttribute(shape.at(0), CDMAttribute("units", "m"));
                cdm.addAttribute(shape.at(0), CDMAttribute("standard_name", "projection_x_axis"));
            }
            cdm.getVariable(shape.at(0)).setData(createData(dimSize, vals));
        }
        westAxis = std::make_shared<CoordinateAxis>(cdm.getVariable(shape.at(0)));
        westAxis->setAxisType(CoordinateAxis::GeoX);
        westAxis->setExplicit(true);
    }
    if (cdm.hasDimension("west_east_stag")) {
        vector<string> shape;
        shape.push_back("west_east_stag");
        if (!cdm.hasVariable(shape.at(0))) {
            float dx = cdm.getAttribute(cdm.globalAttributeNS(), "DX").getData()->asFloat()[0];
            if (isLatLon) {
                dx = rad_to_deg(dx) / R0;
            }
            size_t dimSize = cdm.getDimension(shape.at(0)).getLength();
            double startX = centerX - dx * (dimSize - 1) / 2;
            shared_array<float> vals(new float[dimSize]);
            for (size_t i = 0; i < dimSize; i++) {
                vals[i] = startX + i * dx;
            }
            cdm.addVariable(CDMVariable(shape.at(0),CDM_FLOAT, shape));
            if (isLatLon) {
                cdm.addAttribute(shape.at(0), CDMAttribute("units", "degree"));
                cdm.addAttribute(shape.at(0), CDMAttribute("standard_name", "grid_longitude"));
            } else {
                cdm.addAttribute(shape.at(0), CDMAttribute("units", "m"));
                cdm.addAttribute(shape.at(0), CDMAttribute("standard_name", "projection_x_axis"));
            }
            cdm.getVariable(shape.at(0)).setData(createData(dimSize, vals));
        }
        stagWestAxis = std::make_shared<CoordinateAxis>(cdm.getVariable(shape.at(0)));
        stagWestAxis->setAxisType(CoordinateAxis::GeoX);
        stagWestAxis->setExplicit(true);
    }
    if (cdm.hasDimension("south_north")) {
        vector<string> shape;
        shape.push_back("south_north");
        if (!cdm.hasVariable(shape.at(0))) {
            float dy = cdm.getAttribute(cdm.globalAttributeNS(), "DY").getData()->asFloat()[0];
            if (isLatLon) {
                dy = rad_to_deg(dy) / R0;
            }
            size_t dimSize = cdm.getDimension(shape.at(0)).getLength();
            double startY = centerY - dy * (dimSize - 1) / 2;
            shared_array<float> vals(new float[dimSize]);
            for (size_t i = 0; i < dimSize; i++) {
                vals[i] = startY + i * dy;
            }
            cdm.addVariable(CDMVariable(shape.at(0), CDM_FLOAT, shape));
            if (isLatLon) {
                cdm.addAttribute(shape.at(0), CDMAttribute("units", "degree"));
                cdm.addAttribute(shape.at(0),
                        CDMAttribute("standard_name", "grid_latitude"));
            } else {
                cdm.addAttribute(shape.at(0), CDMAttribute("units", "m"));
                cdm.addAttribute(shape.at(0),
                        CDMAttribute("standard_name", "projection_y_axis"));
            }
            cdm.getVariable(shape.at(0)).setData(createData(dimSize, vals));
        }
        northAxis = std::make_shared<CoordinateAxis>(cdm.getVariable(shape.at(0)));
        northAxis->setAxisType(CoordinateAxis::GeoY);
        northAxis->setExplicit(true);
    }
    if (cdm.hasDimension("south_north_stag")) {
        vector<string> shape;
        shape.push_back("south_north_stag");
        if (!cdm.hasVariable(shape.at(0))) {
            float dy = cdm.getAttribute(cdm.globalAttributeNS(), "DY").getData()->asFloat()[0];
            if (isLatLon) {
                dy = rad_to_deg(dy) / R0;
            }
            size_t dimSize = cdm.getDimension(shape.at(0)).getLength();
            double startY = centerY - dy * (dimSize - 1) / 2;
            shared_array<float> vals(new float[dimSize]);
            for (size_t i = 0; i < dimSize; i++) {
                vals[i] = startY + i * dy;
            }
            cdm.addVariable(CDMVariable(shape.at(0), CDM_FLOAT, shape));
            if (isLatLon) {
                cdm.addAttribute(shape.at(0), CDMAttribute("units", "degree"));
                cdm.addAttribute(shape.at(0),
                        CDMAttribute("standard_name", "grid_latitude"));
            } else {
                cdm.addAttribute(shape.at(0), CDMAttribute("units", "m"));
                cdm.addAttribute(shape.at(0),
                        CDMAttribute("standard_name", "projection_y_axis"));
            }
            cdm.getVariable(shape.at(0)).setData(createData(dimSize, vals));
        }
        stagNorthAxis = std::make_shared<CoordinateAxis>(cdm.getVariable(shape.at(0)));
        stagNorthAxis->setAxisType(CoordinateAxis::GeoY);
        stagNorthAxis->setExplicit(true);
    }
    if (cdm.hasDimension("bottom_top")) {
        vector<string> shape;
        shape.push_back("bottom_top");
        if (!cdm.hasVariable(shape.at(0))) {
            size_t dimSize = cdm.getDimension(shape.at(0)).getLength();
            shared_array<float> vals(new float[dimSize]);
            for (size_t i = 0; i < dimSize; i++) {
                vals[i] = i;
            }
            cdm.addVariable(CDMVariable(shape.at(0), CDM_FLOAT, shape));
            cdm.addAttribute(shape.at(0), CDMAttribute("units", "1"));
            cdm.getVariable(shape.at(0)).setData(createData(dimSize, vals));
        }
        bottomAxis = std::make_shared<CoordinateAxis>(cdm.getVariable(shape.at(0)));
        bottomAxis->setAxisType(CoordinateAxis::GeoZ);
        bottomAxis->setExplicit(true);
    }
    if (cdm.hasDimension("bottom_top_stag")) {
        vector<string> shape;
        shape.push_back("bottom_top_stag");
        if (!cdm.hasVariable(shape.at(0))) {
            size_t dimSize = cdm.getDimension(shape.at(0)).getLength();
            shared_array<float> vals(new float[dimSize]);
            for (size_t i = 0; i < dimSize; i++) {
                vals[i] = i;
            }
            cdm.addVariable(CDMVariable(shape.at(0), CDM_FLOAT, shape));
            cdm.addAttribute(shape.at(0), CDMAttribute("units", "1"));
            cdm.getVariable(shape.at(0)).setData(createData(dimSize, vals));
        }
        stagBottomAxis = std::make_shared<CoordinateAxis>(cdm.getVariable(shape.at(0)));
        stagBottomAxis->setAxisType(CoordinateAxis::GeoZ);
        stagBottomAxis->setExplicit(true);
    }

    // determine the Time
    const string Time("Time");
    if (cdm.hasDimension(Time) && !cdm.hasVariable(Time)) {
        vector<string> shape;
        shape.push_back(Time);
        if (!cdm.hasVariable(Time)) {
            string units;
            size_t dimSize = cdm.getDimension(Time).getLength();
            shared_array<float> vals(new float[dimSize]);
            if (reader) {
                // try to guess the time-steps
                string start = cdm.getAttribute(cdm.globalAttributeNS(),
                        "START_DATE").getStringValue();
                vector<string> datetime = tokenize(start, "_");
                units = "hours since " + datetime.at(0) + " "
                        + datetime.at(1) + " +0000";

                float timeStep = 180.;
                CDMAttribute timeStepAttr;
                if (cdm.getAttribute(cdm.globalAttributeNS(), "TIME_STEP_MN", timeStepAttr)) {
                    // attribute from ncml
                    timeStep = timeStepAttr.getData()->asFloat()[0];
                } else {
                    LOG4FIMEX(logger, Logger::WARN,
                            "Could not find attribute TIME_STEP_MN, guessing 180minutes time-steps");
                }
                timeStep /= 60;
                for (size_t i = 0; i < dimSize; i++) {
                    vals[i] = timeStep * i;
                }
            } else {
                // try to read the time from the data-reader TIMES-variable
                // Times is usually given as 'YYYY-MM-DD_HH:MM:SS' in UTM as string
                if (cdm.hasVariable("Times")) {
                    SliceBuilder sb(cdm, "Times");
                    size_t timeSize = cdm.getDimension("Time").getLength();
                    time_point ptFirst;
                    for (size_t i = 0; i < timeSize; ++i) {
                        sb.setStartAndSize("Time", i, 1);
                        DataPtr data = reader->getDataSlice("Times", sb);
                        // define units and set time in that unit as float to vals
                        string thisTime = data->asString();
                        if (thisTime.find("_") != string::npos) {
                            thisTime = thisTime.replace(thisTime.find("_"), 1, " "); // replace _ with space
                        }
                        time_point pt = make_time_from_string(thisTime);
                        if (i == 0) {
                            ptFirst = pt;
                            units = "hours since " + make_time_string_extended(ptFirst);
                        }
                        vals[i] = std::chrono::duration_cast<std::chrono::seconds>(pt - ptFirst).count() / 3600.0;
                    }
                } else {
                    LOG4FIMEX(logger, Logger::ERROR, "no 'Times' variable in WRF found, time-dimension will give wrong results");
                }

            }
            cdm.addVariable(CDMVariable(Time, CDM_FLOAT, shape));
            cdm.addAttribute(Time, CDMAttribute("units", units));
            cdm.getVariable(Time).setData(createData(dimSize, vals));
        }
        // ref-time
        string reftime = "forecast_reference_time";
        if (!cdm.hasVariable(reftime)) {
            cdm.addVariable(CDMVariable(reftime, CDM_INT, vector<string>(0)));
            string ref = cdm.getAttribute(cdm.globalAttributeNS(),
                    "SIMULATION_START_DATE").getStringValue();
            vector<string> refdatetime = tokenize(ref, "_");
            string refunits = "hours since " + refdatetime.at(0) + " "
                    + refdatetime.at(1) + " +0000";
            cdm.addAttribute(reftime, CDMAttribute("units", refunits));
            cdm.addAttribute(reftime, CDMAttribute("standard_name", reftime));
            shared_array<float> refvals(new float[1]);
            refvals[0] = 0;
            cdm.getVariable(reftime).setData(createData(1, refvals));
        }
    }
    timeAxis = std::make_shared<CoordinateAxis>(cdm.getVariable(Time));
    timeAxis->setAxisType(CoordinateAxis::Time);
    timeAxis->setExplicit(true);

    // now, create the coordinate-systems depending on the variables shape
    map<string, CoordinateSystem_p> cs;
    const CDM::VarVec vars = cdm.getVariables();
    for (CDM::VarVec::const_iterator vit = vars.begin(); vit != vars.end(); ++vit) {
        vector<string> shape = vit->getShape();
        // only adding CoordinateSystems for x y * systems
        if (((find(shape.begin(), shape.end(), "west_east") != shape.end()) || (find(shape.begin(), shape.end(), "west_east_stag") != shape.end()))
            && ((find(shape.begin(), shape.end(), "south_north") != shape.end()) || (find(shape.begin(), shape.end(), "south_north_stag") != shape.end()))) {
            string shapeId = join(shape.begin(), shape.end());
            if (cs.find(shapeId) == cs.end()) {
                CoordinateSystem_p coord = std::make_shared<CoordinateSystem>(WRFCoordSysBuilder().getName());
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
                        coord->addDependencyVariable("Times"); // Times required for Time-Axis in WRF
                    } else if (*dimIt == "bottom_top") {
                        coord->setAxis(bottomAxis);
                    } else if (*dimIt == "bottom_top_stag") {
                        coord->setAxis(stagBottomAxis);
                    } else {
                        if (!cdm.hasVariable(*dimIt)) {
                            // add a dimension without a variable with a 'virtual' variable
                            vector<string> myshape(1, *dimIt);
                            cdm.addVariable(CDMVariable(*dimIt, CDM_INT, myshape));
                            size_t dimSize = cdm.getDimension(*dimIt).getLength();
                            shared_array<float> vals(new float[dimSize]);
                            for (size_t i = 0; i < dimSize; i++) {
                                vals[i] = i;
                            }
                            cdm.getVariable(*dimIt).setData(createData(dimSize, vals));
                        }
                        CoordinateAxis_p other = std::make_shared<CoordinateAxis>(cdm.getVariable(*dimIt));
                        other->setExplicit(true);
                        coord->setAxis(other);
                    }
                }
                coord->setSimpleSpatialGridded(true);
                cs[shapeId] = coord;
            }
            CoordinateSystem_p coord = cs[shapeId];
            coord->setCSFor(vit->getName(), true);
            coord->setComplete(vit->getName(), true);
        }
    }
    for (map<string, CoordinateSystem_p>::iterator csIt = cs.begin(); csIt != cs.end(); ++csIt) {
        coordSys.push_back(csIt->second);
    }


    return coordSys;
}
CoordinateSystem_cp_v WRFCoordSysBuilder::listCoordinateSystems(CDM& cdm)
{
#if 0
    // this will cause a failed assert(reader != 0)
    return wrfListCoordinateSystems(cdm, CDMReader_p());
#else
    LOG4FIMEX(logger, Logger::ERROR,
            "cannot list WRF coordinate systems from a CDM, please pass a CDMReader");
    return CoordinateSystem_cp_v();
#endif
}

CoordinateSystem_cp_v WRFCoordSysBuilder::listCoordinateSystems(CDMReader_p reader)
{
    return wrfListCoordinateSystems(reader->getInternalCDM(), reader);
}

} /* namespace MetNoFimex */
