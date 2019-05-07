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

#include "fimex/coordSys/CoordinateSystem.h"
#include "CF1_xCoordSysBuilder.h"
#include "CoordSysBuilder.h"
#include "CoordSysImpl.h"
#include "WRFCoordSysBuilder.h"
#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/Logger.h"
#include "fimex/Units.h"
#include "fimex/Utils.h"
#include "fimex/coordSys/Projection.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"

#include <boost/make_shared.hpp>

#include <algorithm>
#include <boost/regex.hpp>
#include <functional>
#include <iterator>
#include <map>
#include <set>

namespace MetNoFimex
{
using namespace std;

namespace {

struct CompareCoordinateAxis {
    bool operator()(CoordinateAxis_cp a, CoordinateAxis_cp b) const { return *a < *b; }
};

struct TypeCheck : public std::unary_function<CoordinateAxis_cp, bool>
{
    CoordinateAxis::AxisType type_;
    TypeCheck(CoordinateAxis::AxisType type) : type_(type) {}
    bool operator()(const CoordinateAxis_cp& ca) const { return ca->isAxisType(type_); }
};

// search a string in a set
inline bool set_contains(const set<string>& s, const std::string & str)
{
    return s.find(str) != s.end();
}

// add/remove a string to a set
inline void set_insert_erase(set<string>& s, const std::string& str, bool set)
{
    if (set)
        s.insert(str);
    else
        s.erase(str);
}

bool hasTypeInAxes(CoordinateAxis::AxisType type, CoordinateAxis_cp_v& axes)
{
    CoordinateAxis_cp_v::const_iterator found = find_if(axes.begin(), axes.end(), TypeCheck(type));
    return found != axes.end();
}

CoordinateAxis_cp findTypeInAxes(CoordinateAxis::AxisType type, CoordinateAxis_cp_v& axes)
{
    CoordinateAxis_cp_v::iterator axis = find_if(axes.begin(), axes.end(), TypeCheck(type));
    if (axis != axes.end()) {
        return *axis;
    }
    // return 0/NULL Ptr
    return CoordinateAxis_cp();
}

void removeAxis(CoordinateAxis_cp_v& axes, const std::string& axisname)
{
    CoordinateAxis_cp_v::iterator found = axes.begin();
    while (true) {
        found = find_if(found, axes.end(), CDMNameEqualPtr(axisname));
        if (found == axes.end())
            break;
        found = axes.erase(found);
    }
}

typedef boost::shared_ptr<CoordSysBuilder> CoordSysBuilder_p;
typedef std::vector<CoordSysBuilder_p> CoordSysBuilder_pv;

CoordSysBuilder_pv createBuilders()
{
    CoordSysBuilder_pv builders;
    // supported conventions, 1st will be used as fallback
    builders.push_back(boost::make_shared<CF1_xCoordSysBuilder>());
    builders.push_back(boost::make_shared<WRFCoordSysBuilder>());
    return builders;
}

} // anonymous namespace

static Logger_p logger = getLogger("fimex.coordSys.CoordinateSystem");

CoordinateSystem::CoordinateSystem()
    : pimpl_(new CoordSysImpl())
{
}

CoordinateSystem::CoordinateSystem(const std::string& conventionName)
    : pimpl_(new CoordSysImpl())
{
    pimpl_->conventionName_ = conventionName;
}

CoordinateSystem::~CoordinateSystem() {}

std::ostream& operator<<(std::ostream& out, const CoordinateSystem& cs)
{
    out << cs.getConventionName() << "=";
    CoordinateAxis_cp_v axes = cs.getAxes();
    for (CoordinateAxis_cp_v::const_iterator axis = axes.begin(); axis != axes.end(); ++axis) {
        out << (*axis)->getName() << ":" << (*axis)->getAxisType() << "";
        if ((axis+1) != axes.end()) out << ",";
    }
    if (cs.hasProjection()) {
        out << ";" << *(cs.getProjection());
    }
    if (cs.isSimpleSpatialGridded()) {
        out << ";" << "simpleSpatialGrid";
    }
    if (cs.hasVerticalTransformation()) {
         out << ";" << *(cs.getVerticalTransformation());
     }
    return out;
}

std::string CoordinateSystem::id() const
{
    CoordinateAxis_cp_v axes = getAxes();
    sort(axes.begin(), axes.end(), CompareCoordinateAxis());
    return getConventionName() + ":" + joinPtr(axes.begin(), axes.end());
}

std::string CoordinateSystem::horizontalId() const
{
    CoordinateAxis_cp xAxis = getGeoXAxis();
    CoordinateAxis_cp yAxis = getGeoYAxis();
    if (xAxis.get() != 0 && yAxis.get() != 0) {
        return xAxis->getName() + "," + yAxis->getName();
    } else if (xAxis.get() != 0) {
        return xAxis->getName();
    } else if (yAxis.get() != 0) {
        return yAxis->getName();
    } else {
        return std::string();
    }
}

std::string CoordinateSystem::getConventionName() const
{
    return pimpl_->conventionName_;
}

void CoordinateSystem::setConventionName(const std::string& conventionName)
{
    pimpl_->conventionName_ = conventionName;
}

bool CoordinateSystem::isComplete(const std::string & varName) const
{
    return set_contains(pimpl_->isCompleteVars_, varName);
}

void CoordinateSystem::setComplete(const std::string & varName, bool set)
{
    set_insert_erase(pimpl_->isCompleteVars_, varName, set);
}

bool CoordinateSystem::isCSFor(const std::string & varName) const
{
    return set_contains(pimpl_->isCSForVars_, varName);
}

void CoordinateSystem::setCSFor(const std::string & varName, bool set)
{
    set_insert_erase(pimpl_->isCSForVars_, varName, set);
}

bool CoordinateSystem::isCSAndCompleteFor(const std::string& varName) const
{
    return isCSFor(varName) && isComplete(varName);
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
Projection_cp CoordinateSystem::getProjection() const
{
    return pimpl_->proj_;
}

void CoordinateSystem::setProjection(Projection_cp proj)
{
    pimpl_->proj_ = proj;
}

bool CoordinateSystem::hasVerticalTransformation() const
{
    return pimpl_->vtran_.get() != 0;
}

VerticalTransformation_cp CoordinateSystem::getVerticalTransformation() const
{
    return pimpl_->vtran_;
}

void CoordinateSystem::setVerticalTransformation(VerticalTransformation_cp vtran)
{
    pimpl_->vtran_ = vtran;
}

bool CoordinateSystem::hasAxisType(CoordinateAxis::AxisType type) const
{
    return hasTypeInAxes(type, pimpl_->axes_) || hasTypeInAxes(type, pimpl_->auxiliaryAxes_);
}

CoordinateAxis_cp CoordinateSystem::findAxisOfType(CoordinateAxis::AxisType type) const
{
    CoordinateAxis_cp axis = findTypeInAxes(type, pimpl_->axes_);
    if (axis.get() == 0) {
        axis = findTypeInAxes(type, pimpl_->auxiliaryAxes_);
    }
    return axis;
}

CoordinateAxis_cp CoordinateSystem::findAxisOfType(const vector<CoordinateAxis::AxisType>& types) const
{
    for (vector<CoordinateAxis::AxisType>::const_iterator typeIt = types.begin(); typeIt != types.end(); ++typeIt) {
        CoordinateAxis_cp axis = findAxisOfType(*typeIt);
        if (axis.get() != 0)
            return axis;
    }
    // return 0/NULL Ptr
    return CoordinateAxis_cp();
}

CoordinateAxis_cp CoordinateSystem::getGeoXAxis() const
{
    vector<CoordinateAxis::AxisType> types;
    types.push_back(CoordinateAxis::GeoX);
    types.push_back(CoordinateAxis::Lon);
    return findAxisOfType(types);
}

CoordinateAxis_cp CoordinateSystem::getGeoYAxis() const
{
    vector<CoordinateAxis::AxisType> types;
    types.push_back(CoordinateAxis::GeoY);
    types.push_back(CoordinateAxis::Lat);
    return findAxisOfType(types);
}

CoordinateAxis_cp CoordinateSystem::getGeoZAxis() const
{
    vector<CoordinateAxis::AxisType> types;
    types.push_back(CoordinateAxis::GeoZ);
    types.push_back(CoordinateAxis::Height);
    types.push_back(CoordinateAxis::Depth);
    types.push_back(CoordinateAxis::Pressure);
    return findAxisOfType(types);
}

CoordinateAxis_cp CoordinateSystem::getTimeAxis() const
{
    vector<CoordinateAxis::AxisType> types(1, CoordinateAxis::Time);
    return findAxisOfType(types);
}

CoordinateAxis_cp_v CoordinateSystem::getAxes() const
{
    CoordinateAxis_cp_v axes(pimpl_->axes_.begin(), pimpl_->axes_.end());
    axes.insert(axes.end(), pimpl_->auxiliaryAxes_.begin(), pimpl_->auxiliaryAxes_.end());
    return axes;
}

void CoordinateSystem::setAxis(CoordinateAxis_cp axis)
{
    assert(axis.get() != 0);
    CoordinateAxis_cp_v& v = pimpl_->axes_;
    // remove axis with same name
    removeAxis(v, axis->getName());
    // add new axis
    if (axis->getAxisType() != CoordinateAxis::Undefined &&
        hasTypeInAxes(axis->getAxisType(), v)) {
        CoordinateAxis_cp otherAxis = findTypeInAxes(axis->getAxisType(), v);
        throw CDMException("adding axis "+axis->getName()+" to CS: type already exists: " + CoordinateAxis::type2string(axis->getAxisType()) + " in " + otherAxis->getName());
    }
    v.push_back(axis);

    // remove auxiliary axes
    CoordinateAxis_cp_v& va = pimpl_->auxiliaryAxes_;
    // remove axis with same name
    removeAxis(va, axis->getName());
}

void CoordinateSystem::setAuxiliaryAxis(CoordinateAxis_cp axis)
{
    assert(axis.get() != 0);
    CoordinateAxis_cp_v& v = pimpl_->axes_;
    CoordinateAxis_cp_v::iterator foundInAxes = find_if(v.begin(), v.end(), CDMNameEqualPtr(axis->getName()));
    if (foundInAxes != v.end()) {
        return; // ignore, existing true axis with same name
    }

    if (axis->getAxisType() != CoordinateAxis::Undefined) {
        CoordinateAxis_cp otherAxis = findTypeInAxes(axis->getAxisType(), v);
        if (otherAxis.get() != 0) {
            return; // ignore, axisType already in use
        }
    }

    CoordinateAxis_cp_v& va = pimpl_->auxiliaryAxes_;
    // remove axis with same name
    removeAxis(va, axis->getName());
    va.push_back(axis);
}

std::set<std::string> CoordinateSystem::getDependencyVariables() const
{
    std::set<std::string> retVal = pimpl_->dependencyVars_;
    CoordinateAxis_cp_v axes = getAxes();
    for (CoordinateAxis_cp_v::iterator axIt = axes.begin(); axIt != axes.end(); ++axIt) {
        retVal.insert((*axIt)->getName());
    }
    return retVal;
}

void CoordinateSystem::addDependencyVariable(std::string varName)
{
    pimpl_->dependencyVars_.insert(varName);
}

int findBestHorizontalCoordinateSystems(bool withProjection, CDMReader_p reader, std::map<std::string, CoordinateSystem_cp>& systems,
                                        std::map<std::string, std::string>& variables, std::vector<std::string>& incompatibleVariables)
{
    typedef map<string, CoordinateSystem_cp> CoordSysMap;
    CoordSysMap coordSysMap;
    CoordinateSystem_cp_v coordSys = listCoordinateSystems(reader);
    const CDM& cdm = reader->getCDM();
    for (CoordinateSystem_cp_v::iterator cs = coordSys.begin(); cs != coordSys.end(); ++cs) {
        if (((!withProjection) || ((*cs)->isSimpleSpatialGridded() && (*cs)->hasProjection())) &&
              (withProjection || ((*cs)->hasAxisType(CoordinateAxis::Lat) && (*cs)->hasAxisType(CoordinateAxis::Lon)))) {
            coordSysMap[(*cs)->horizontalId()] = *cs;
        } else {
            LOG4FIMEX(logger, Logger::DEBUG, "CS dropped: simpleSpatialGrid="<<(*cs)->isSimpleSpatialGridded() << " projection=" << (*cs)->hasProjection() << " lon="<<(*cs)->hasAxisType(CoordinateAxis::Lon)<< " lat="<<(*cs)->hasAxisType(CoordinateAxis::Lat));
        }
    }
    if (coordSysMap.empty()) {
        return 0;
    }
    // mapping all variables with matching orgX/orgY dimensions
    // find all variables belonging to a cs containing the projection
    const CDM::VarVec& vars = cdm.getVariables();
    for (CDM::VarVec::const_iterator v = vars.begin(); v != vars.end(); ++v) {
        CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(coordSys, v->getName());
        if (cs.get()) {
            if (coordSysMap.find((cs)->horizontalId()) != coordSysMap.end()) {
                variables[v->getName()] = cs->horizontalId();
            }
        }
    }

    // remove all variables which are not part of any coordinate-system
    // but which share geographical-dimensions (x,y,lon,lat) with the system
    for (CoordSysMap::iterator csmi = coordSysMap.begin(); csmi != coordSysMap.end(); ++csmi){
        set<std::string> geoDimensions;
        vector<string>  xShape = cdm.getVariable(csmi->second->getGeoXAxis()->getName()).getShape();
        vector<string>  yShape = cdm.getVariable(csmi->second->getGeoYAxis()->getName()).getShape();
        geoDimensions.insert(xShape.begin(), xShape.end());
        geoDimensions.insert(yShape.begin(), yShape.end());
        CDM::VarVec vars = cdm.getVariables();
        CoordinateAxis_cp_v axes = csmi->second->getAxes();
        for (CDM::VarVec::const_iterator v = vars.begin(); v != vars.end(); ++v) {
            if ((axes.end() == find_if(axes.begin(), axes.end(), CDMNameEqualPtr(v->getName()))) &&
                (variables.end() == variables.find(v->getName()))) {
                // v is not an axis
                // v is not a projectionVariable
                vector<string> vShape = cdm.getVariable(v->getName()).getShape();
                sort(vShape.begin(), vShape.end());
                vector<string> intersect;
                set_intersection(geoDimensions.begin(), geoDimensions.end(),
                                 vShape.begin(), vShape.end(),
                                 back_inserter(intersect));
                if (intersect.size() > 0) {
                    incompatibleVariables.push_back(v->getName());
                }
            }
        }
    }

    // make the coordinateSystems minimal
    for (CoordSysMap::iterator csmi = coordSysMap.begin(); csmi != coordSysMap.end(); ++csmi) {
        CoordinateSystem_p cs = boost::make_shared<CoordinateSystem>();
        cs->setConventionName(csmi->second->getConventionName());
        cs->setAxis(csmi->second->getGeoXAxis());
        cs->setAxis(csmi->second->getGeoYAxis());
        // find lat and lon, eventually different from GeoX and GeoY

        if (csmi->second->hasProjection()) {
            cs->setProjection(csmi->second->getProjection());
        }
        if (csmi->second->hasAxisType(CoordinateAxis::Lon)) {
            cs->setAxis(csmi->second->findAxisOfType(CoordinateAxis::Lon));
        }
        if (csmi->second->hasAxisType(CoordinateAxis::Lat)) {
            cs->setAxis(csmi->second->findAxisOfType(CoordinateAxis::Lat));
        }

        // add all variables to the coordinate system
        for (map<string, string>::const_iterator v = variables.begin(); v != variables.end(); ++v) {
            if (cs->horizontalId() == v->second) {
                cs->setCSFor(v->first);
            }
        }

        LOG4FIMEX(logger, Logger::DEBUG, "interpolator of cs " << *cs);
        systems[csmi->first] = cs;
    }
    for (CoordSysMap::iterator csIt = systems.begin(); csIt != systems.end(); ++csIt) {
        assert(csIt->second.get() != 0);
    }
    return systems.size();
}

CoordinateSystem_cp_v listCoordinateSystems(const CDM& cdm)
{
    return listCoordinateSystems(const_cast<CDM&>(cdm));
}

CoordinateSystem_cp_v listCoordinateSystems(CDM& cdm)
{
    // the return value
    CoordinateSystem_cp_v coordSystems;

    const CoordSysBuilder_pv builders = createBuilders();

    for (size_t i = 0; i < builders.size(); ++i) {
        CoordSysBuilder_p builder = builders.at(i);
        if (builder->isMine(cdm)) {
            CoordinateSystem_cp_v myCoordSystems = builder->listCoordinateSystems(cdm);
            LOG4FIMEX(logger, Logger::DEBUG, "found convention: " << builder->getName() << ", amount: " << myCoordSystems.size());
            copy(myCoordSystems.begin(), myCoordSystems.end(), back_inserter(coordSystems));
        } else {
            LOG4FIMEX(logger, Logger::DEBUG, "no convention found for convention: " << builder->getName());
        }
    }
    if (coordSystems.empty() && !builders.empty()) {
        CoordSysBuilder_p b0 = builders.front();
        LOG4FIMEX(logger, Logger::DEBUG, "no regular conventions found, wild-guess checking " << b0->getName());
        coordSystems = b0->listCoordinateSystems(cdm);
    }

    LOG4FIMEX(logger, Logger::DEBUG, "total conventions found: " << coordSystems.size());
    return coordSystems;
}

CoordinateSystem_cp_v listCoordinateSystems(CDMReader_p reader)
{
    // the return value
    CoordinateSystem_cp_v coordSystems;

    const CoordSysBuilder_pv builders = createBuilders();

    for (size_t i = 0; i < builders.size(); ++i) {
        CoordSysBuilder_p builder = builders.at(i);
        if (builder->isMine(reader->getCDM())) {
            CoordinateSystem_cp_v myCoordSystems = builder->listCoordinateSystems(reader);
            LOG4FIMEX(logger, Logger::DEBUG, "found convention: " << builder->getName() << ", amount: " << myCoordSystems.size());
            copy(myCoordSystems.begin(), myCoordSystems.end(), back_inserter(coordSystems));
        } else {
            LOG4FIMEX(logger, Logger::DEBUG, "no convention found for convention: " << builder->getName());
        }
    }
    if (coordSystems.empty() && !builders.empty()) {
        CoordSysBuilder_p b0 = builders.front();
        LOG4FIMEX(logger, Logger::DEBUG, "no regular conventions found, wild-guess checking " << b0->getName());
        coordSystems = b0->listCoordinateSystems(reader);
    }

    LOG4FIMEX(logger, Logger::DEBUG, "total conventions found: " << coordSystems.size());
    return coordSystems;
}

void enhanceVectorProperties(CDMReader_p reader)
{
    const CoordSysBuilder_pv builders = createBuilders();

    bool found = false;
    for (size_t i = 0; i < builders.size(); ++i) {
        CoordSysBuilder_p builder = builders.at(i);
        if (builder->isMine(reader->getCDM())) {
            found = true;
            builder->enhanceVectorProperties(reader);
            LOG4FIMEX(logger, Logger::DEBUG, "found convention: " << builder->getName() << ", enhancing vectors");
        } else {
            LOG4FIMEX(logger, Logger::DEBUG, "no convention found for convention: " << builder->getName());
        }
    }
    if (!found && !builders.empty()) {
        CoordSysBuilder_p b0 = builders.front();
        LOG4FIMEX(logger, Logger::DEBUG, "no regular conventions found, wild-guess checking " << b0->getName());
        b0->enhanceVectorProperties(reader);
    }
}

CoordinateSystem_cp findCompleteCoordinateSystemFor(const CoordinateSystem_cp_v& coordSys, const std::string& varName)
{
    const CoordinateSystem_cp_v::const_iterator itCS = std::find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(varName));
    if (itCS != coordSys.end())
        return *itCS;
    else
        return CoordinateSystem_cp();
}

}
