/*
 * Fimex, CF1xCoordSysBuilder.cc
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

#include "coordSys/CF1_xCoordSysBuilder.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMAttribute.h"
#include "fimex/Utils.h"
#include "fimex/Units.h"
#include <set>

namespace MetNoFimex
{

using namespace std;

CF1_xCoordSysBuilder::CF1_xCoordSysBuilder()
{
}

CF1_xCoordSysBuilder::~CF1_xCoordSysBuilder()
{
}

bool CF1_xCoordSysBuilder::isMine(const CDM& cdm)
{
    CDMAttribute conventionAttr;
    string conventions;
    if (cdm.getAttribute(cdm.globalAttributeNS(), "Conventions", conventionAttr)) {
        conventions = conventionAttr.getStringValue();
    } else if (cdm.getAttribute(cdm.globalAttributeNS(), "conventions", conventionAttr)) {
        conventions = conventionAttr.getStringValue();
    }
    if ((conventions.find("CF-1.") != string::npos) ||
            (conventions.find("cf-1.") != string::npos)) {
        return true;
    }
    if ((conventions.find("COARDS") != string::npos)) {
        return true;
    }
    return false;
}

/**
 * get the coordinates for a variable var
 *
 * @param cdm data model
 * @param var variable
 * @return all (auxiliary) coordinate axes used for the variable
 */
set<string> getCoordinateNamesCF1_x(const CDM& cdm, const CDMVariable& var) {
    set<string> coords;
    CDMAttribute attr;
    if (cdm.getAttribute(var.getName(), "coordinates", attr)) {
        vector<string> coord = tokenize(attr.getStringValue(), " ");
        coords.insert(coord.begin(), coord.end());
    }
    return coords;
}

/**
 * get the coordinateAxis for a variable var
 *
 * @param cdm data model
 * @param var variable
 * @return all (auxiliary, non-auxiliary) coordinate axes used for the variable
 */
set<string> getCoordinateAxesNamesCF1_x(const CDM& cdm, const CDMVariable& var)
{
    vector<string> dims = var.getShape();
    set<string> axes(dims.begin(), dims.end());

    set<string> coords = getCoordinateNamesCF1_x(cdm, var);
    axes.insert(coords.begin(), coords.end());

    return axes;
}


static CoordinateAxis::AxisType getAxisTypeCF1_x(const CDM& cdm, const string& varName)
{
    CDMAttribute attr;
    // test standard name
    if (cdm.getAttribute(varName, "standard_name", attr)) {
        string sname = attr.getStringValue();
        string lcSName = string2lowerCase(sname);
        if ( lcSName == "longitude" )
            return CoordinateAxis::Lon;
        if ( lcSName == "latitude" )
            return CoordinateAxis::Lat;
        if ( lcSName == "forecast_reference_time" )
            return CoordinateAxis::ReferenceTime;
        if ( lcSName == "projection_x_coordinate" ||
             lcSName == "grid_longitude" )
            return CoordinateAxis::GeoX;
        if ( lcSName == "projection_y_coordinate" ||
             lcSName == "grid_latitude" )
            return CoordinateAxis::GeoY;
        if ( lcSName == "atmosphere_sigma_coordinate" ||
             lcSName == "atmosphere_hybrid_sigma_pressure_coordinate" ||
             lcSName == "atmosphere_hybrid_height_coordinate" ||
             lcSName == "atmosphere_sleve_coordinate" ||
             lcSName == "ocean_sigma_coordinate" ||
             lcSName == "ocean_s_coordinate" ||
             lcSName == "ocean_sigma_z_coordinate" ||
             lcSName == "ocean_double_sigma_coordinate" ||
             lcSName == "ocean_s_coordinate_g1" ||
             lcSName == "ocean_s_coordinate_g2" )
            return CoordinateAxis::GeoZ;
    }
    // test unit
    Units uc;
    if (cdm.getAttribute(varName, "units", attr)) {
        string unit = attr.getStringValue();
        string lcUnit = string2lowerCase(unit);
        if ( lcUnit == "degrees_east" ||
             lcUnit == "degrees_e" ||
             lcUnit == "degreese" ||
             lcUnit == "degree_east" ||
             lcUnit == "degree_e" ||
             lcUnit == "degreee" )
            return CoordinateAxis::Lon;

        if ( lcUnit == "degrees_north" ||
             lcUnit == "degrees_n" ||
             lcUnit == "degreesn" ||
             lcUnit == "degree_north" ||
             lcUnit == "degree_n" ||
             lcUnit == "degreen" )
            return CoordinateAxis::Lat;

        if (uc.isTime(unit))
            return CoordinateAxis::Time;

        if (uc.areConvertible("mbar", unit))
            return CoordinateAxis::Pressure;
        // deprecated in CF, but still there
        if ( lcUnit == "level" ||
             lcUnit == "layer" ||
             lcUnit == "sigma_level")
            return CoordinateAxis::GeoZ;
    }

    // test 'positive' attribute
    if (cdm.getAttribute(varName, "positive", attr)) {
        if (cdm.getAttribute(varName, "units", attr)) {
            string unit = attr.getStringValue();
            if (uc.areConvertible("m", unit)) {
                return CoordinateAxis::Height;
            } else {
                return CoordinateAxis::GeoZ;
            }
        } else {
            return CoordinateAxis::GeoZ;
        }
    }

    // test axis
    if (cdm.getAttribute(varName, "axis", attr)) {
        string axis = attr.getStringValue();
        string lcAxis = string2lowerCase(axis);
        if (lcAxis == "x")
            return CoordinateAxis::GeoX;
        if (lcAxis == "y")
            return CoordinateAxis::GeoY;
        if (lcAxis == "z") {
            if (cdm.getAttribute(varName, "units", attr)) {
                string unit = attr.getStringValue();
                if (uc.areConvertible(unit, "mbar"))
                    return CoordinateAxis::Pressure;
                if (uc.areConvertible(unit, "m"))
                    return CoordinateAxis::Height;
            }
            // no known units
            return CoordinateAxis::GeoZ;
        }
        if (lcAxis == "t")
            return CoordinateAxis::Time;
    }

    return CoordinateAxis::Undefined;
}


static string getPtrName(const boost::shared_ptr<const CDMNamedEntity>& ptr)
{
    return ptr->getName();
}

std::vector<boost::shared_ptr<const CoordinateSystem> > CF1_xCoordSysBuilder::listCoordinateSystems(boost::shared_ptr<CDMReader> reader)
{
    return listCoordinateSystems(reader->getInternalCDM());
}


std::vector<boost::shared_ptr<const CoordinateSystem> > CF1_xCoordSysBuilder::listCoordinateSystems(CDM& cdm)
{
    typedef CoordinateSystem::AxisPtr AxisPtr;
    // step 1: find all coordinate axes, that are dimensions and coordinates
    map<string, AxisPtr> coordinateAxes;
    {
        set<string> tmpCoordinateAxes;
        const CDM::DimVec& dims = cdm.getDimensions();
        for (CDM::DimVec::const_iterator dim = dims.begin(); dim != dims.end(); ++dim) {
            tmpCoordinateAxes.insert(dim->getName());
        }
        vector<string> vars = cdm.findVariables("coordinates", ".*");
        for (vector<string>::iterator varIt = vars.begin(); varIt != vars.end(); ++varIt) {
            vector<string> coordinates = tokenize(cdm.getAttribute(*varIt, "coordinates").getStringValue(), " ");
            tmpCoordinateAxes.insert(coordinates.begin(), coordinates.end());
        }
        // create CoordinateAxis
        for (set<string>::iterator coord = tmpCoordinateAxes.begin(); coord != tmpCoordinateAxes.end(); ++coord) {
            if (cdm.hasVariable(*coord)) {
                coordinateAxes[*coord] = AxisPtr(new CoordinateAxis(cdm.getVariable(*coord)));
            } else {
                // add a dimension without a variable with a 'virtual' variable
                vector<string> shape(1, *coord);
                coordinateAxes[*coord] = AxisPtr(new CoordinateAxis(CDMVariable(*coord, CDM_INT, shape)));
            }
        }
    }

    // step 2: set the axis type of the axes of each coordinate system
    {
        const CDM::DimVec& dims = cdm.getDimensions();
        for (map<string, AxisPtr >::iterator ca = coordinateAxes.begin(); ca != coordinateAxes.end(); ++ca) {
            AxisPtr& axis = ca->second;
            CoordinateAxis::AxisType type = getAxisTypeCF1_x(cdm, axis->getName());
            axis->setAxisType(type);
            if (find_if(dims.begin(), dims.end(), CDMNameEqual(axis->getName())) == dims.end()) {
                axis->setExplicit(false);
            } else {
                axis->setExplicit(true);
            }
        }
    }

    // step 3: find all used coordinate systems, that are distinct combinations
    // of axes, CoordinateSystem's less operator makes sure of that in the set
    // the coordinateSystems here are intermediate, since the axes still don't have types
    map<string, CoordinateSystem> coordSystems;
    {
        const CDM::VarVec& vars = cdm.getVariables();
        for (CDM::VarVec::const_iterator varIt = vars.begin(); varIt != vars.end(); ++varIt) {
            if (coordinateAxes.find(varIt->getName()) != coordinateAxes.end()) {
                // variable is coordinateAxis, i.e. it doesn't need a coordinateSystem
            } else {
                set<string> axes = getCoordinateAxesNamesCF1_x(cdm, *varIt);
                set<string> coords = getCoordinateAxesNamesCF1_x(cdm, *varIt);
                if (axes.size()) {
                    CoordinateSystem cs("CF-1.X");
                    for (set<string>::iterator axis = axes.begin(); axis != axes.end(); ++axis) {
                        map<string, AxisPtr >::iterator foundAxis = coordinateAxes.find(*axis);
                        if (foundAxis != coordinateAxes.end()) {
                            // only dims with variable can be coordinate axis
                            if (coords.find(foundAxis->second->getName()) == coords.end()) {
                                // dimension
                                cs.setAxis(foundAxis->second);
                            } else {
                                // coordinates
                                cs.setAuxiliaryAxis(foundAxis->second);
                            }
                        }
                    }
                    coordSystems.insert(make_pair(cs.id(), cs));
                }
            }
        }
    }


    // step 4: add additional information to each CoordinateSystem,
    //         e.g. isComplete, isCSFor, isSimpleSpatial, ... (transformations?)
    // isComplete, isCSFor
    {
        const CDM::VarVec& vars = cdm.getVariables();
        for (map<string,CoordinateSystem>::iterator cit = coordSystems.begin(); cit != coordSystems.end(); ++cit) {
            CoordinateSystem::ConstAxisList csAxesPtr = cit->second.getAxes();
            vector<string> csAxes;
            transform(csAxesPtr.begin(), csAxesPtr.end(), back_inserter(csAxes), getPtrName);
            for (CDM::VarVec::const_iterator varIt = vars.begin(); varIt != vars.end(); ++varIt) {
                set<string> varAxes = getCoordinateAxesNamesCF1_x(cdm, *varIt);
                if (includes(varAxes.begin(), varAxes.end(),
                             csAxes.begin(), csAxes.end())) {
                    // all csAxes part of varAxes
                    cit->second.setComplete(varIt->getName(), true);
                } else {
                    cit->second.setComplete(varIt->getName(), false);
                }
                if (includes(csAxes.begin(), csAxes.end(),
                             varAxes.begin(), varAxes.end())) {
                    // all varAxes part of csAxes
                    cit->second.setCSFor(varIt->getName(), true);
                } else {
                    cit->second.setCSFor(varIt->getName(), false);
                }
            }
        }
    }
    // isSimpleSpatialGridded
    {
        for (map<string,CoordinateSystem>::iterator cit = coordSystems.begin(); cit != coordSystems.end(); ++cit) {
            CoordinateSystem& cs = cit->second;
            CoordinateSystem::ConstAxisPtr xAxis = cs.getGeoXAxis();
            CoordinateSystem::ConstAxisPtr yAxis = cs.getGeoYAxis();
            bool simpleX = false;
            bool simpleY = false;
            if (xAxis.get() != 0) {
                if (xAxis->getShape().size() == 1) {
                    simpleX = true;
                }
            }
            if (yAxis.get() != 0) {
                if (yAxis->getShape().size() == 1) {
                    simpleY = true;
                }
            }
            cs.setSimpleSpatialGridded(simpleX && simpleY);
        }
    }

    // transformations
    {
        const CDM::VarVec& vars = cdm.getVariables();
        for (map<string,CoordinateSystem>::iterator cit = coordSystems.begin(); cit != coordSystems.end(); ++cit) {
            CoordinateSystem& cs = cit->second;
            // set the projection of the first matching var
            for (CDM::VarVec::const_iterator var = vars.begin(); (var != vars.end() && !cs.hasProjection()); ++var) {
                if (cs.isCSFor(var->getName()) && cs.isComplete(var->getName())) {
                    CDMAttribute mappingAttr;
                    if (cdm.getAttribute(var->getName(), "grid_mapping", mappingAttr)) {
                        std::string varName = mappingAttr.getStringValue();
                        if (cdm.hasVariable(varName)) {
                            boost::shared_ptr<Projection> proj = Projection::create(cdm.getAttributes(varName));
                            cs.setProjection(proj);
                        }
                    } else {
                        // LON/LAT systems don't need  grid_mapping, detect by lat/lon axes
                        // lat/lon must be axes (i.e. 'simple') otherwise not a projection
                        if (cs.isSimpleSpatialGridded() &&
                            cs.hasAxisType(CoordinateAxis::Lon) && cs.hasAxisType(CoordinateAxis::Lat)) {
                            if ( cs.getGeoXAxis()->getName() == cs.findAxisOfType(CoordinateAxis::Lon)->getName() &&
                                 cs.getGeoYAxis()->getName() == cs.findAxisOfType(CoordinateAxis::Lat)->getName() ) {
                                CDMAttribute grid_mapping("grid_mapping_name", "latitude_longitude");
                                vector<CDMAttribute> attrs(1, grid_mapping);
                                boost::shared_ptr<Projection> proj = Projection::create(attrs);
                                cs.setProjection(proj);
                            }
                        }
                    }
                }
            }
        }
    }


    // return a const casted version of the coordSystems
    vector<boost::shared_ptr<const CoordinateSystem> > outCSs;
    for (map<string,CoordinateSystem>::iterator cit = coordSystems.begin(); cit != coordSystems.end(); ++cit) {
        outCSs.push_back(boost::shared_ptr<const CoordinateSystem>(new CoordinateSystem(cit->second)));
    }
    return outCSs;
}

} /* namespace MetNoFimex */
