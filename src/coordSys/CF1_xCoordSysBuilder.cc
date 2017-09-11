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
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/Units.h"
#include <set>
#include <boost/regex.hpp>
#include <boost/make_shared.hpp>
#include <fimex/coordSys/verticalTransform/AtmosphereSigma.h>
#include <fimex/coordSys/verticalTransform/Height.h>
#include <fimex/coordSys/verticalTransform/HybridSigmaPressure1.h>
#include <fimex/coordSys/verticalTransform/HybridSigmaPressure2.h>
#include <fimex/coordSys/verticalTransform/LnPressure.h>
#include <fimex/coordSys/verticalTransform/OceanSG1.h>
#include <fimex/coordSys/verticalTransform/OceanSG2.h>
#include <fimex/coordSys/verticalTransform/Pressure.h>


namespace MetNoFimex
{

static LoggerPtr logger = getLogger("fimex.CF1_xCoordSysBuilder");

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
static set<string> getCoordinateNamesCF1_x(const CDM& cdm, const CDMVariable& var)
{
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
static set<string> getCoordinateAxesNamesCF1_x(const CDM& cdm, const CDMVariable& var)
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
        if ( lcSName == "realization" )
            return CoordinateAxis::Realization;
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

std::vector<boost::shared_ptr<const CoordinateSystem> > CF1_xCoordSysBuilder::listCoordinateSystems(CDMReader_p reader)
{
    return listCoordinateSystems(reader->getInternalCDM());
}

static bool isCSForTerm(const CDM& cdm, const CoordinateSystem& cs, const std::string& term)
{
    if (term.empty())
        return true;

    CoordinateSystem::ConstAxisList csAxesPtr = cs.getAxes();
    set<string> csAxes;
    transform(csAxesPtr.begin(), csAxesPtr.end(), inserter(csAxes, csAxes.begin()), getPtrName);

    const set<string> varAxes = getCoordinateAxesNamesCF1_x(cdm, cdm.getVariable(term));
    for (set<string>::const_iterator vaxit = varAxes.begin(); vaxit != varAxes.end(); ++vaxit) {
        if (cdm.hasDimension(*vaxit)) {
            const CDMDimension& dim = cdm.getDimension(*vaxit);
            if (dim.getLength() == 1) {
                continue;
            }
            if (!csAxes.count(*vaxit)) {
                LOG4FIMEX(logger, Logger::INFO, "formula_term "<< term << " missing " << *vaxit << " in cs " << cs.id());
                return false;
            }
        }
    }
    return true;
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

            bool is_explicit = (find_if(dims.begin(), dims.end(), CDMNameEqual(axis->getName())) != dims.end());
            axis->setExplicit(is_explicit);
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
                set<string> coords = getCoordinateNamesCF1_x(cdm, *varIt);
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
            set<string> csAxes;
            transform(csAxesPtr.begin(), csAxesPtr.end(), inserter(csAxes, csAxes.begin()), getPtrName);
            for (CDM::VarVec::const_iterator varIt = vars.begin(); varIt != vars.end(); ++varIt) {
                set<string> varAxes = getCoordinateAxesNamesCF1_x(cdm, *varIt);
                // all csAxes part of varAxes
                bool is_complete = includes(varAxes.begin(), varAxes.end(),
                                            csAxes.begin(), csAxes.end());
                // all varAxes part of csAxes
                bool is_cs_for = includes(csAxes.begin(), csAxes.end(),
                                          varAxes.begin(), varAxes.end());
                cit->second.setComplete(varIt->getName(), is_complete);
                cit->second.setCSFor(varIt->getName(), is_cs_for);
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
                if (xAxis->isExplicit() && xAxis->getShape().size() == 1) {
                    simpleX = true;
                }
            }
            if (yAxis.get() != 0) {
                if (yAxis->isExplicit() && yAxis->getShape().size() == 1) {
                    simpleY = true;
                }
            }
            cs.setSimpleSpatialGridded(simpleX && simpleY);
        }
    }

    // horizontal transformations
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
                            cs.addDependencyVariable(varName);
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

    // vertical transformations
    for (map<string,CoordinateSystem>::iterator cit = coordSystems.begin(); cit != coordSystems.end(); ++cit) {
        CoordinateSystem& cs = cit->second;
        CoordinateSystem::ConstAxisPtr zAxis = cs.getGeoZAxis();
        if (zAxis.get() != 0) {
            if (cdm.hasVariable(zAxis->getName())) {
                switch (zAxis->getAxisType()) {
                case CoordinateAxis::Height:
                    cs.setVerticalTransformation(boost::shared_ptr<VerticalTransformation>(
                        new Height(zAxis->getName())));
                    break;
                case CoordinateAxis::Pressure:
                    cs.setVerticalTransformation(boost::shared_ptr<VerticalTransformation>(
                        new Pressure(zAxis->getName())));
                    break;
                default:
                    CDMAttribute formula;
                    map<string, string> terms;
                    if (cdm.getAttribute(zAxis->getName(), "formula_terms", formula)) {
                        string term = formula.getStringValue();
                        string::const_iterator begin = term.begin(), end = term.end();
                        boost::match_results<string::const_iterator> what;
                        while (boost::regex_search(begin, end, what, boost::regex("(\\S+)\\s*:\\s*(\\S+)"))) {
                            string term(what[1].first, what[1].second);
                            string var(what[2].first, what[2].second);
                            if (!var.empty() && cdm.hasVariable(var)) {
                                cs.addDependencyVariable(var);
                                terms[term] = var;
                            } else {
                                LOG4FIMEX(logger, Logger::WARN, "formula term '" << term << "' specifies unknown variable '" << var << "'");
                            }
                            begin = what[0].second;
                        }
                    }
                    CDMAttribute standardName;
                    if (cdm.getAttribute(zAxis->getName(), "standard_name", standardName)) {
                        const std::string  standardNameValue = standardName.getStringValue();

                        // * check that the CS is a CS for all the formula terms used
                        // * problem: surface fields often have their own z axis, which makes isCSFor return false!

                        if (standardNameValue == "atmosphere_hybrid_sigma_pressure_coordinate") {
                            if (!(isCSForTerm(cdm, cs, terms["p"]) && isCSForTerm(cdm, cs, terms["ps"]) && isCSForTerm(cdm, cs, terms["p0"])))
                                continue;
                            if (terms["ap"] == "") {
                                if (!isCSForTerm(cdm, cs, terms["a"]))
                                    continue;
                                cs.setVerticalTransformation(boost::shared_ptr<VerticalTransformation>(
                                        new HybridSigmaPressure2(terms["a"], terms["b"],terms["ps"],terms["p0"])));
                            } else {
                                if (!isCSForTerm(cdm, cs, terms["ap"]))
                                    continue;
                                cs.setVerticalTransformation(boost::shared_ptr<VerticalTransformation>(
                                        new HybridSigmaPressure1(terms["ap"], terms["b"],terms["ps"],terms["p0"])));
                            }
                        } else if (standardNameValue == "atmosphere_ln_pressure_coordinate") {
                            if (!(isCSForTerm(cdm, cs, terms["lev"]) && isCSForTerm(cdm, cs, terms["p0"])))
                                continue;
                            cs.setVerticalTransformation(boost::shared_ptr<VerticalTransformation>(
                                    new LnPressure(terms["lev"], terms["p0"])));
                        } else if (standardNameValue == "atmosphere_sigma_coordinate") {
                            if (!(isCSForTerm(cdm, cs, terms["sigma"]) && isCSForTerm(cdm, cs, terms["ptop"]) && isCSForTerm(cdm, cs, terms["ps"])))
                                continue;
                            cs.setVerticalTransformation(boost::shared_ptr<VerticalTransformation>(
                                    new AtmosphereSigma(terms["sigma"],terms["ptop"],terms["ps"])));
                        } else if (standardNameValue == "ocean_s_coordinate_g1") {
                            if (!(isCSForTerm(cdm, cs, terms["s"]) && isCSForTerm(cdm, cs, terms["C"]) && isCSForTerm(cdm, cs, terms["depth"])
                                  && isCSForTerm(cdm, cs, terms["depth_c"]) && isCSForTerm(cdm, cs, terms["eta"])))
                                continue;
                            cs.setVerticalTransformation(boost::make_shared<OceanSG1>(
                                    OceanSGVars(terms["s"], terms["C"], terms["depth"], terms["depth_c"], terms["eta"])));
                        } else if (standardNameValue == "ocean_s_coordinate_g2") {
                            if (!(isCSForTerm(cdm, cs, terms["s"]) && isCSForTerm(cdm, cs, terms["C"]) && isCSForTerm(cdm, cs, terms["depth"])
                                  && isCSForTerm(cdm, cs, terms["depth_c"]) && isCSForTerm(cdm, cs, terms["eta"])))
                                continue;
                            cs.setVerticalTransformation(boost::make_shared<OceanSG2>(
                                    OceanSGVars(terms["s"], terms["C"], terms["depth"], terms["depth_c"], terms["eta"])));
                        } else {
                            LOG4FIMEX(logger, Logger::INFO, "Vertical transformation for " << standardNameValue << "not implemented yet");
                        }
                    }
                    break;
                }
            }
        }
    }

    // axis-bounds
    for (map<string,CoordinateSystem>::iterator cit = coordSystems.begin(); cit != coordSystems.end(); ++cit) {
        CoordinateSystem& cs = cit->second;
        CoordinateSystem::ConstAxisList axes = cs.getAxes();
        for (CoordinateSystem::ConstAxisList::iterator aIt = axes.begin(); aIt != axes.end(); ++aIt) {
            string axis = (*aIt)->getName();
            if (cdm.hasVariable(axis)) {
                CDMAttribute bound;
                if (cdm.getAttribute(axis, "bounds", bound)) {
                    if (cdm.hasVariable(bound.getStringValue())) {
                        cs.addDependencyVariable(bound.getStringValue());
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

static void enhanceVectors(CDM& cdm, const std::string& xsn, const std::string& ysn,
                           const std::string& xdir, const std::string& ydir)
{
    vector<string> xVars = cdm.findVariables("standard_name", "\\Q"+xsn+"\\E.*");
    vector<string> yVars = cdm.findVariables("standard_name", "\\Q"+ysn+"\\E.*");
    for (vector<string>::iterator xVar = xVars.begin(); xVar != xVars.end(); ++xVar) {
        CDMVariable& xv = cdm.getVariable(*xVar);
        if (xv.isSpatialVector())
            continue;
        const vector<string>& xSh = xv.getShape();
        for (vector<string>::iterator yVar = yVars.begin(); yVar != yVars.end(); ++yVar) {
            CDMVariable& yv = cdm.getVariable(*yVar);
            if (yv.isSpatialVector())
                continue;
            const vector<string>& ySh = yv.getShape();
            if (xSh == ySh) {
                xv.setAsSpatialVector(*yVar, xdir);
                yv.setAsSpatialVector(*xVar, ydir);
                LOG4FIMEX(logger, Logger::INFO, "making "<< *xVar << "," << *yVar << " a vector");
                break;
            }
        }
    }
}

void CF1_xCoordSysBuilder::enhanceVectorProperties(CDMReader_p reader)
{
    CDM& cdm = reader->getInternalCDM();

    const std::string xdir("x"), ydir("y");
    enhanceVectors(cdm, "x_wind", "y_wind", xdir, ydir);
    enhanceVectors(cdm, "grid_eastward_wind", "grid_northward_wind", xdir, ydir);
    enhanceVectors(cdm, "sea_water_x_velocity", "sea_water_y_velocity", xdir, ydir);
    enhanceVectors(cdm, "x_sea_water_velocity", "y_sea_water_velocity", xdir, ydir);
    enhanceVectors(cdm, "barotropic_sea_water_x_velocity", "barotropic_sea_water_y_velocity", xdir, ydir);
    enhanceVectors(cdm, "bolus_sea_water_x_velocity", "bolus_sea_water_y_velocity", xdir, ydir);
    enhanceVectors(cdm, "surface_geostrophic_sea_water_x_velocity", "surface_geostrophic_sea_water_y_velocity", xdir, ydir);
    enhanceVectors(cdm, "barotropic_sea_water_x_velocity", "barotropic_sea_water_y_velocity", xdir, ydir);
    enhanceVectors(cdm, "sea_ice_x_velocity", "sea_ice_y_velocity", xdir, ydir);
    enhanceVectors(cdm, "sea_ice_x_transport", "sea_ice_y_transport", xdir, ydir);
    enhanceVectors(cdm, "sea_ice_x_displacement", "sea_ice_y_displacement", xdir, ydir);
    enhanceVectors(cdm, "sea_surface_wave_stokes_drift_x_velocity", "sea_surface_wave_stokes_drift_y_velocity", xdir, ydir); // preliminary from mailing-list
    enhanceVectors(cdm, "land_ice_x_velocity", "land_ice_y_velocity", xdir, ydir);
    enhanceVectors(cdm, "land_ice_basal_x_velocity", "land_ice_basal_y_velocity", xdir, ydir);
    enhanceVectors(cdm, "land_ice_vertical_mean_x_velocity", "land_ice_vertical_mean_y_velocity", xdir, ydir);
    enhanceVectors(cdm, "ocean_heat_x_transport", "ocean_heat_y_transport", xdir, ydir);
    enhanceVectors(cdm, "ocean_salt_x_transport", "ocean_salt_y_transport", xdir, ydir);
    enhanceVectors(cdm, "ocean_volume_x_transport", "ocean_volume_y_transport", xdir, ydir);

    const std::string londir("lon"), latdir("lat");
    enhanceVectors(cdm, "atmosphere_eastward_stress_due_to_gravity_wave_drag", "atmosphere_northward_stress_due_to_gravity_wave_drag", londir, latdir);
    enhanceVectors(cdm, "baroclinic_eastward_sea_water_velocity","baroclinic_northward_sea_water_velocity", londir, latdir);
    enhanceVectors(cdm, "barotropic_eastward_sea_water_velocity","barotropic_northward_sea_water_velocity", londir, latdir);
    enhanceVectors(cdm, "bolus_eastward_sea_water_velocity","bolus_northward_sea_water_velocity", londir, latdir);
    enhanceVectors(cdm, "downward_eastward_momentum_flux_in_air","downward_northward_momentum_flux_in_air", londir, latdir);
    enhanceVectors(cdm, "downward_eastward_momentum_flux_in_air_due_to_diffusion","downward_northward_momentum_flux_in_air_due_to_diffusion", londir, latdir);
    enhanceVectors(cdm, "downward_eastward_stress_at_sea_ice_base","downward_northward_stress_at_sea_ice_base", londir, latdir);
    enhanceVectors(cdm, "eastward_atmosphere_dry_static_energy_transport_across_unit_distance","northward_atmosphere_dry_static_energy_transport_across_unit_distance", londir, latdir);
    enhanceVectors(cdm, "eastward_atmosphere_water_transport_across_unit_distance","northward_atmosphere_water_transport_across_unit_distance", londir, latdir);
    enhanceVectors(cdm, "eastward_atmosphere_water_vapor_transport_across_unit_distance","northward_atmosphere_water_vapor_transport_across_unit_distance", londir, latdir);
    enhanceVectors(cdm, "eastward_derivative_of_northward_sea_ice_velocity","northward_derivative_of_northward_sea_ice_velocity", londir, latdir);
    enhanceVectors(cdm, "eastward_mass_flux_of_air","northward_mass_flux_of_air", londir, latdir);
    enhanceVectors(cdm, "eastward_momentum_flux_correction","northward_momentum_flux_correction", londir, latdir);
    enhanceVectors(cdm, "eastward_sea_ice_displacement", "northward_sea_ice_displacement", londir, latdir);
    enhanceVectors(cdm, "eastward_sea_ice_velocity","northward_sea_ice_velocity", londir, latdir);
    enhanceVectors(cdm, "eastward_sea_water_velocity","northward_sea_water_velocity", londir, latdir);
    enhanceVectors(cdm, "eastward_sea_water_velocity_assuming_no_tide","northward_sea_water_velocity_assuming_no_tide", londir, latdir);
    enhanceVectors(cdm, "eastward_transformed_eulerian_mean_air_velocity","northward_transformed_eulerian_mean_air_velocity", londir, latdir);
    enhanceVectors(cdm, "eastward_transformed_eulerian_mean_velocity","northward_transformed_eulerian_mean_velocity", londir, latdir);
    enhanceVectors(cdm, "eastward_water_vapor_flux_in_air","northward_water_vapor_flux_in_air", londir, latdir);
    enhanceVectors(cdm, "eastward_water_vapor_flux","northward_water_vapor_flux", londir, latdir);
    enhanceVectors(cdm, "eastward_water_vapor_transport_across_unit_distance_in_atmosphere_layer","northward_water_vapor_transport_across_unit_distance_in_atmosphere_layer", londir, latdir);
    enhanceVectors(cdm, "eastward_wind","northward_wind", londir, latdir);
    enhanceVectors(cdm, "eastward_wind_shear","northward_wind_shear", londir, latdir);
    enhanceVectors(cdm, "geostrophic_eastward_wind","geostrophic_northward_wind", londir, latdir);
        // integral_of_surface_downward_eastward_stress_wrt_time
        // northward_derivative_of_eastward_sea_ice_velocity
    enhanceVectors(cdm, "product_of_eastward_sea_water_velocity_and_salinity", "product_of_northward_sea_water_velocity_and_salinity", londir, latdir);
    enhanceVectors(cdm, "product_of_eastward_sea_water_velocity_and_temperature", "product_of_northward_sea_water_velocity_and_temperature", londir, latdir);
    enhanceVectors(cdm, "product_of_eastward_wind_and_air_temperature","product_of_northward_wind_and_air_temperature", londir, latdir);
    enhanceVectors(cdm, "product_of_eastward_wind_and_geopotential_height","product_of_northward_wind_and_geopotential_height", londir, latdir);
        // product_of_eastward_wind_and_northward_wind
    enhanceVectors(cdm, "product_of_eastward_wind_and_omega","product_of_northward_wind_and_omega", londir, latdir);
    enhanceVectors(cdm, "product_of_eastward_wind_and_specific_humidity", "product_of_northward_wind_and_specific_humidity", londir, latdir);
    enhanceVectors(cdm, "product_of_eastward_wind_and_upward_air_velocity","product_of_northward_wind_and_upward_air_velocity", londir, latdir);
    enhanceVectors(cdm, "surface_downward_eastward_stress","surface_downward_northward_stress", londir, latdir);
    enhanceVectors(cdm, "surface_eastward_sea_water_velocity","surface_northward_sea_water_velocity", londir, latdir);
    enhanceVectors(cdm, "surface_geostrophic_eastward_sea_water_velocity", "surface_geostrophic_northward_sea_water_velocity", londir, latdir);
    enhanceVectors(cdm, "surface_eastward_geostrophic_sea_water_velocity","surface_northward_geostrophic_sea_water_velocity", londir, latdir);
    enhanceVectors(cdm, "surface_geostrophic_eastward_sea_water_velocity_assuming_sea_level_for_geoid","surface_geostrophic_northward_sea_water_velocity_assuming_sea_level_for_geoid", londir, latdir);
    enhanceVectors(cdm, "surface_eastward_geostrophic_sea_water_velocity_assuming_sea_level_for_geoid","surface_northward_geostrophic_sea_water_velocity_assuming_sea_level_for_geoid", londir, latdir);
    enhanceVectors(cdm, "tendency_of_eastward_wind","tendency_of_northward_wind", londir, latdir);
    enhanceVectors(cdm, "tendency_of_eastward_wind_due_to_advection","tendency_of_northward_wind_due_to_advection", londir, latdir);
    enhanceVectors(cdm, "tendency_of_eastward_wind_due_to_convection","tendency_of_northward_wind_due_to_convection", londir, latdir);
    enhanceVectors(cdm, "tendency_of_eastward_wind_due_to_diffusion","tendency_of_northward_wind_due_to_diffusion", londir, latdir);
    enhanceVectors(cdm, "tendency_of_eastward_wind_due_to_eliassen_palm_flux_divergence","tendency_of_northward_wind_due_to_eliassen_palm_flux_divergence", londir, latdir);
    enhanceVectors(cdm, "tendency_of_eastward_wind_due_to_gravity_wave_drag","tendency_of_northward_wind_due_to_gravity_wave_drag", londir, latdir);
    enhanceVectors(cdm, "tendency_of_eastward_wind_due_to_nonorographic_gravity_wave_drag","tendency_of_northward_wind_due_to_nonorographic_gravity_wave_drag", londir, latdir);
    enhanceVectors(cdm, "tendency_of_eastward_wind_due_to_numerical_artefacts","tendency_of_northward_wind_due_to_numerical_artefacts", londir, latdir);
    enhanceVectors(cdm, "tendency_of_eastward_wind_due_to_orographic_gravity_wave_drag","tendency_of_northward_wind_due_to_orographic_gravity_wave_drag", londir, latdir);
    enhanceVectors(cdm, "upward_eastward_momentum_flux_in_air_due_to_nonorographic_eastward_gravity_waves","upward_northward_momentum_flux_in_air_due_to_nonorographic_northward_gravity_waves", londir, latdir);
    enhanceVectors(cdm, "upward_flux_of_eastward_momentum_due_to_nonorographic_eastward_gravity_waves","upward_flux_of_northward_momentum_due_to_nonorographic_northward_gravity_waves", londir, latdir);
    enhanceVectors(cdm, "upward_eastward_momentum_flux_in_air_due_to_nonorographic_westward_gravity_waves","upward_northward_momentum_flux_in_air_due_to_nonorographic_westward_gravity_waves", londir, latdir);
    enhanceVectors(cdm, "upward_flux_of_eastward_momentum_due_to_nonorographic_westward_gravity_waves","upward_flux_of_northward_momentum_due_to_nonorographic_westward_gravity_waves", londir, latdir);
    enhanceVectors(cdm, "upward_eastward_momentum_flux_in_air_due_to_orographic_gravity_waves", "upward_northward_momentum_flux_in_air_due_to_orographic_gravity_waves", londir, latdir);
    enhanceVectors(cdm, "upward_flux_of_eastward_momentum_due_to_orographic_gravity_waves","upward_flux_of_northward_momentum_due_to_orographic_gravity_waves", londir, latdir);
    enhanceVectors(cdm, "upward_eastward_stress_at_sea_ice_base", "upward_northward_stress_at_sea_ice_base", londir, latdir);
}

} /* namespace MetNoFimex */
