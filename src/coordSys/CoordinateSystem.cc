/*
 * Fimex, CoordinateSystem.cc
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Mar 15, 2010
 *      Author: Heiko Klein
 */

#include <algorithm>
#include <functional>
#include <iterator>
#include <boost/regex.hpp>
#include <set>
#include <map>
#include "fimex/CDM.h"
#include "fimex/Units.h"
#include "fimex/Utils.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/Logger.h"
#include "CoordSysImpl.h"

namespace MetNoFimex
{
using namespace std;

CoordinateSystem::CoordinateSystem()
: pimpl_(boost::shared_ptr<CoordSysImpl>(new CoordSysImpl()))
{
}

CoordinateSystem::CoordinateSystem(const std::string& conventionName)
: pimpl_(boost::shared_ptr<CoordSysImpl>(new CoordSysImpl()))
{
    pimpl_->conventionName_ = conventionName;
}

std::ostream& operator<<(std::ostream& out, const CoordinateSystem& cs)
{
    out << cs.getConventionName() << "=";
    CoordinateSystem::ConstAxisList axes = cs.getAxes();
    for (CoordinateSystem::ConstAxisList::const_iterator axis = axes.begin(); axis != axes.end(); ++axis) {
        out << (*axis)->getName() << ":" << (*axis)->getAxisType() << "";
        if ((axis+1) != axes.end()) out << ",";
    }
    if (cs.hasProjection()) {
        out << ";" << *(cs.getProjection());
    }
    return out;
}

std::string CoordinateSystem::id() const
{
    CoordinateSystem::ConstAxisList axes = getAxes();
    sort(axes.begin(), axes.end());
    return getConventionName() + ":" + joinPtr(axes.begin(), axes.end());

}


std::string CoordinateSystem::getConventionName() const
{
    return pimpl_->conventionName_;
}

void CoordinateSystem::setConventionName(const std::string& conventionName)
{
    pimpl_->conventionName_ = conventionName;
}

// search a string in a vector
inline bool isStringInVector(const vector<string>& vec, const std::string & str)
{
    return find(vec.begin(), vec.end(), str) != vec.end();
}

// add/remove a string to a vector, avoid doubles
inline void setStringToVector(vector<string>& vec, const std::string& str, bool set)
{
    vector<string>::iterator vecIt = find(vec.begin(), vec.end(), str);
    if (vecIt != vec.end()) { // str exists
        if (set) {
            // nothing required, already there
        } else {
            // remove varName
            vec.erase(vecIt);
        }
    } else {
        if (set) {
            vec.push_back(str);
        } else {
            // not there, nothing to remove
        }
    }
}

bool CoordinateSystem::isComplete(const std::string & varName) const
{
    return isStringInVector(pimpl_->isCompleteVec_, varName);
}



void CoordinateSystem::setComplete(const std::string & varName, bool set)
{
    setStringToVector(pimpl_->isCompleteVec_, varName, set);
}



bool CoordinateSystem::isCSFor(const std::string & varName) const
{
    return isStringInVector(pimpl_->isCSForVec_, varName);
}

void CoordinateSystem::setCSFor(const std::string & varName, bool set)
{
    setStringToVector(pimpl_->isCSForVec_, varName, set);
}


bool CoordinateSystem::isSimpleSpatialGridded() const
{
    return pimpl_->isSimpleSpatialGridded_;
}

void CoordinateSystem::setSimpleSpatialGridded(bool set)
{
    pimpl_->isSimpleSpatialGridded_ = set;
}

bool CoordinateSystem::hasProjection() const
{
    return pimpl_->proj_.get() != 0;
}
boost::shared_ptr<const Projection> CoordinateSystem::getProjection() const
{
    return pimpl_->proj_;
}
void CoordinateSystem::setProjection(boost::shared_ptr<const Projection> proj)
{
    pimpl_->proj_ = proj;
}


struct TypeCheck : public std::unary_function <CoordinateSystem::ConstAxisPtr, bool>{
    CoordinateAxis::AxisType type_;
    TypeCheck(CoordinateAxis::AxisType type) : type_(type) {}
    bool operator() (const CoordinateSystem::ConstAxisPtr& ca) const { return ca->isAxisType(type_); }
};

bool CoordinateSystem::hasAxisType(CoordinateAxis::AxisType type) const
{
    ConstAxisList& v = pimpl_->axes_;
    ConstAxisList::const_iterator found = find_if(v.begin(), v.end(), TypeCheck(type));
    return found != v.end();
}

CoordinateSystem::ConstAxisPtr CoordinateSystem::findAxisOfType(CoordinateAxis::AxisType type) const
{
    ConstAxisList& axes = pimpl_->axes_;
    ConstAxisList::iterator axis = find_if(axes.begin(), axes.end(), TypeCheck(type));
    if (axis != axes.end()) {
        return *axis;
    }
    // return 0/NULL Ptr
    return ConstAxisPtr();
}
CoordinateSystem::ConstAxisPtr CoordinateSystem::findAxisOfType(const vector<CoordinateAxis::AxisType>& types) const
{
    for (vector<CoordinateAxis::AxisType>::const_iterator typeIt = types.begin(); typeIt != types.end(); ++typeIt) {
        ConstAxisPtr axis = findAxisOfType(*typeIt);
        if (axis.get() != 0) return axis;
    }
    // return 0/NULL Ptr
    return ConstAxisPtr();
}
CoordinateSystem::ConstAxisPtr CoordinateSystem::getGeoXAxis() const
{
    vector<CoordinateAxis::AxisType> types;
    types.push_back(CoordinateAxis::GeoX);
    types.push_back(CoordinateAxis::Lon);
    return findAxisOfType(types);
}


CoordinateSystem::ConstAxisPtr CoordinateSystem::getGeoYAxis() const
{
    vector<CoordinateAxis::AxisType> types;
    types.push_back(CoordinateAxis::GeoY);
    types.push_back(CoordinateAxis::Lat);
    return findAxisOfType(types);
}

CoordinateSystem::ConstAxisPtr CoordinateSystem::getGeoZAxis() const
{
    vector<CoordinateAxis::AxisType> types;
    types.push_back(CoordinateAxis::GeoZ);
    types.push_back(CoordinateAxis::Height);
    types.push_back(CoordinateAxis::Pressure);
    return findAxisOfType(types);
}

CoordinateSystem::ConstAxisPtr CoordinateSystem::getTimeAxis() const
{
    vector<CoordinateAxis::AxisType> types;
    types.push_back(CoordinateAxis::Time);
    return findAxisOfType(types);
}

CoordinateSystem::ConstAxisList CoordinateSystem::getAxes() const
{
    return pimpl_->axes_;
}

void CoordinateSystem::setAxis(ConstAxisPtr axis)
{
    assert(axis.get() != 0);

    ConstAxisList& v = pimpl_->axes_;
    // remove axis with same name
    ConstAxisList::iterator found = find_if(v.begin(), v.end(), CDMNameEqualPtr(axis->getName()));
    while (found != v.end()) {
        v.erase(found);
        found = find_if(v.begin(), v.end(), CDMNameEqualPtr(axis->getName()));
    }
    // add new axis
    if (axis->getAxisType() != CoordinateAxis::Undefined &&
        hasAxisType(axis->getAxisType())) {
        ConstAxisPtr otherAxis = findAxisOfType(axis->getAxisType());
        throw CDMException("adding axis "+axis->getName()+" to CS: type already exists: " + CoordinateAxis::type2string(axis->getAxisType()) + " in " + otherAxis->getName());
    }
    v.push_back(axis);
}


class RegexMatch : public unary_function<string,bool>{
    const boost::regex& reg;
public:
    RegexMatch(const boost::regex& reg) : reg(reg) {}
    bool operator()(const string& str) {return boost::regex_match(str, reg);}
};

// declare cf coordinate system builder
std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystemsCF1_x(const CDM& cdm);

std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystems(const CDM& cdm)
{
    // the return value
    vector<boost::shared_ptr<const CoordinateSystem> > coordSystems;

    CDMAttribute conventionAttr;
    vector<string> conventions;
    if (cdm.getAttribute(cdm.globalAttributeNS(), "Conventions", conventionAttr)) {
        // NetCDF User Guide
        // space separated conventions
        conventions = tokenize(conventionAttr.getStringValue(), " ");
        if (conventions.size() < 2) {
            // eventually , separated conventions
            conventions = tokenize(conventionAttr.getStringValue(), ",");
        }
    } else {
        // handle conventions which don't have the conventions attribute, e.g. WRF
    }
    string logCat = "fimex/coordSys/CoordinateSystem";
    if (find_if(conventions.begin(), conventions.end(), RegexMatch(boost::regex("\\s*CF-1\\.\\d+\\s*"))) != conventions.end()) {
        LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "found convention: CF-1.X");
        // add all cf coordinate systems to the list
        vector<boost::shared_ptr<const CoordinateSystem> > cfCoordSystems = listCoordinateSystemsCF1_x(cdm);
        copy(cfCoordSystems.begin(), cfCoordSystems.end(), back_inserter(coordSystems));
    } else {
        LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "no convention found");
    }
    // TODO: support more conventions

    return coordSystems;
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
    CDMAttribute attr;
    if (cdm.getAttribute(var.getName(), "coordinates", attr)) {
        vector<string> coords = tokenize(attr.getStringValue(), " ");
        axes.insert(coords.begin(), coords.end());
    }
    return axes;
}

CoordinateAxis::AxisType getAxisTypeCF1_x(const CDM& cdm, const string& varName)
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
// implement cf coordinate system builder
std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystemsCF1_x(const CDM& cdm)
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
                if (axes.size()) {
                    CoordinateSystem cs("CF-1.X");
                    for (set<string>::iterator axis = axes.begin(); axis != axes.end(); ++axis) {
                        map<string, AxisPtr >::iterator foundAxis = coordinateAxes.find(*axis);
                        if (foundAxis != coordinateAxes.end()) {
                            // only dims with variable can be coordinate axis
                            cs.setAxis(foundAxis->second);
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
                            cerr << "Proj:" << *proj << endl;
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


}
