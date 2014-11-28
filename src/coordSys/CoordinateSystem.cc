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
#include "fimex/CDMReader.h"
#include "fimex/Units.h"
#include "fimex/Utils.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/Logger.h"
#include "CoordSysImpl.h"
#include "CoordSysBuilder.h"
#include "CF1_xCoordSysBuilder.h"
#include "WRFCoordSysBuilder.h"

namespace MetNoFimex
{
using namespace std;

static const string logCat = "fimex.coordSys.CoordinateSystem";

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
    CoordinateSystem::ConstAxisList axes = getAxes();
    sort(axes.begin(), axes.end());
    return getConventionName() + ":" + joinPtr(axes.begin(), axes.end());

}

std::string CoordinateSystem::horizontalId() const
{
    ConstAxisPtr xAxis = getGeoXAxis();
    ConstAxisPtr yAxis = getGeoYAxis();
    string id;
    if (xAxis.get() != 0) {
        id = xAxis->getName() + ",";
    }
    if (yAxis.get() != 0) {
        id += yAxis->getName();
    }
    return id;
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

bool CoordinateSystem::hasVerticalTransformation() const
{
    return pimpl_->vtran_.get() != 0;
}
boost::shared_ptr<const VerticalTransformation> CoordinateSystem::getVerticalTransformation() const
{
    return pimpl_->vtran_;
}
void CoordinateSystem::setVerticalTransformation(boost::shared_ptr<const VerticalTransformation> vtran)
{
    pimpl_->vtran_ = vtran;
}


struct TypeCheck : public std::unary_function <CoordinateSystem::ConstAxisPtr, bool>{
    CoordinateAxis::AxisType type_;
    TypeCheck(CoordinateAxis::AxisType type) : type_(type) {}
    bool operator() (const CoordinateSystem::ConstAxisPtr& ca) const { return ca->isAxisType(type_); }
};

static bool hasTypeInAxes(CoordinateAxis::AxisType type, CoordinateSystem::ConstAxisList& axes)
{
    CoordinateSystem::ConstAxisList::const_iterator found = find_if(axes.begin(), axes.end(), TypeCheck(type));
    return found != axes.end();
}

bool CoordinateSystem::hasAxisType(CoordinateAxis::AxisType type) const
{
    return hasTypeInAxes(type, pimpl_->axes_) || hasTypeInAxes(type, pimpl_->auxiliaryAxes_);
}

static CoordinateSystem::ConstAxisPtr findTypeInAxes(CoordinateAxis::AxisType type, CoordinateSystem::ConstAxisList& axes)
{
    CoordinateSystem::ConstAxisList::iterator axis = find_if(axes.begin(), axes.end(), TypeCheck(type));
    if (axis != axes.end()) {
        return *axis;
    }
    // return 0/NULL Ptr
    return CoordinateSystem::ConstAxisPtr();
}

CoordinateSystem::ConstAxisPtr CoordinateSystem::findAxisOfType(CoordinateAxis::AxisType type) const
{
    CoordinateSystem::ConstAxisPtr axis = findTypeInAxes(type, pimpl_->axes_);
    if (axis.get() == 0) {
        axis = findTypeInAxes(type, pimpl_->auxiliaryAxes_);
    }
    return axis;
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
    ConstAxisList axes(pimpl_->axes_.begin(), pimpl_->axes_.end());
    axes.insert(axes.end(), pimpl_->auxiliaryAxes_.begin(), pimpl_->auxiliaryAxes_.end());
    return axes;
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
        hasTypeInAxes(axis->getAxisType(), v)) {
        ConstAxisPtr otherAxis = findTypeInAxes(axis->getAxisType(), v);
        throw CDMException("adding axis "+axis->getName()+" to CS: type already exists: " + CoordinateAxis::type2string(axis->getAxisType()) + " in " + otherAxis->getName());
    }
    v.push_back(axis);

    // remove auxiliary axes
    ConstAxisList& va = pimpl_->auxiliaryAxes_;
    // remove axis with same name
    ConstAxisList::iterator f = find_if(va.begin(), va.end(), CDMNameEqualPtr(axis->getName()));
    while (f != va.end()) {
        va.erase(f);
        f = find_if(va.begin(), va.end(), CDMNameEqualPtr(axis->getName()));
    }
}

void CoordinateSystem::setAuxiliaryAxis(ConstAxisPtr axis)
{
    assert(axis.get() != 0);
    CoordinateSystem::ConstAxisList& v = pimpl_->axes_;
    CoordinateSystem::ConstAxisList::iterator foundInAxes = find_if(v.begin(), v.end(), CDMNameEqualPtr(axis->getName()));
    if (foundInAxes != v.end()) {
        return; // ignore, existing true axis with same name
    }

    if (axis->getAxisType() != CoordinateAxis::Undefined) {
        ConstAxisPtr otherAxis = findTypeInAxes(axis->getAxisType(), v);
        if (otherAxis.get() != 0) {
            return; // ignore, axisType already in use
        }
    }

    ConstAxisList& va = pimpl_->auxiliaryAxes_;
    // remove axis with same name
    ConstAxisList::iterator f = find_if(va.begin(), va.end(), CDMNameEqualPtr(axis->getName()));
    while (f != va.end()) {
        va.erase(f);
        f = find_if(va.begin(), va.end(), CDMNameEqualPtr(axis->getName()));
    }
    va.push_back(axis);
}

std::set<std::string> CoordinateSystem::getDependencyVariables() const
{
    std::set<std::string> retVal = pimpl_->dependencyVars_;
    ConstAxisList axes = getAxes();
    for (ConstAxisList::iterator axIt = axes.begin(); axIt != axes.end(); ++axIt) {
        retVal.insert((*axIt)->getName());
    }
    return retVal;
}
void CoordinateSystem::addDependencyVariable(std::string varName)
{
    pimpl_->dependencyVars_.insert(varName);
}


int findBestHorizontalCoordinateSystems(bool withProjection, boost::shared_ptr<CDMReader> reader, std::map<std::string, boost::shared_ptr<const CoordinateSystem> >& systems, std::map<std::string, std::string>& variables, std::vector<std::string>& incompatibleVariables)
{
    typedef boost::shared_ptr<const CoordinateSystem> CoordSysPtr;
    typedef map<string, CoordSysPtr> CoordSysMap;
    typedef vector<CoordSysPtr> CoordSysVec;
    CoordSysMap coordSysMap;
    CoordSysVec coordSys = listCoordinateSystems(reader);
    const CDM& cdm = reader->getCDM();
    for (CoordSysVec::iterator cs = coordSys.begin(); cs != coordSys.end(); ++cs) {
        if (((!withProjection) || ((*cs)->isSimpleSpatialGridded() && (*cs)->hasProjection())) &&
              (withProjection || ((*cs)->hasAxisType(CoordinateAxis::Lat) && (*cs)->hasAxisType(CoordinateAxis::Lon)))) {
            coordSysMap[(*cs)->horizontalId()] = *cs;
        } else {
            LOG4FIMEX(getLogger("fimex.CoordinateSystem"), Logger::DEBUG, "CS dropped: simpleSpatialGrid="<<(*cs)->isSimpleSpatialGridded() << " projection=" << (*cs)->hasProjection() << " lon="<<(*cs)->hasAxisType(CoordinateAxis::Lon)<< " lat="<<(*cs)->hasAxisType(CoordinateAxis::Lat));
        }
    }
    if (coordSysMap.empty()) {
        return 0;
    }
    // mapping all variables with matching orgX/orgY dimensions
    // find all variables belonging to a cs containing the projection
    const CDM::VarVec& vars = cdm.getVariables();
    for (CDM::VarVec::const_iterator v = vars.begin(); v != vars.end(); ++v) {
        CoordSysVec::iterator vCs = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(v->getName()));
        if (coordSys.end() != vCs) {
            if (coordSysMap.find((*vCs)->horizontalId()) != coordSysMap.end()) {
                variables[v->getName()] = (*vCs)->horizontalId();
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
        CoordinateSystem::ConstAxisList axes = csmi->second->getAxes();
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
        boost::shared_ptr<CoordinateSystem> cs(new CoordinateSystem());
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

        LOG4FIMEX(getLogger("fimex.CoordinateSystem"),Logger::DEBUG, "interpolator of cs " << *cs);
        systems[csmi->first] = cs;
    }
    for (CoordSysMap::iterator csIt = systems.begin(); csIt != systems.end(); ++csIt) {
        assert(csIt->second.get() != 0);
    }
    return systems.size();
}


std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystems(const CDM& cdm) {
    return listCoordinateSystems(const_cast<CDM&>(cdm));
}

std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystems(CDM& cdm)
{
    // the return value
    vector<boost::shared_ptr<const CoordinateSystem> > coordSystems;

    vector<boost::shared_ptr<CoordSysBuilder> > builders;
    // supported conventions
    boost::shared_ptr<CoordSysBuilder> cfBuilder(boost::shared_ptr<CoordSysBuilder>(new CF1_xCoordSysBuilder()));
    builders.push_back(cfBuilder);
    builders.push_back(boost::shared_ptr<CoordSysBuilder>(new WRFCoordSysBuilder()));

    for (size_t i = 0; i < builders.size(); ++i) {
        boost::shared_ptr<CoordSysBuilder> builder = builders.at(i);
        if (builder->isMine(cdm)) {
            vector<boost::shared_ptr<const CoordinateSystem> > myCoordSystems = builder->listCoordinateSystems(cdm);
            LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "found convention: " << builder->getName() << ", amount: " << myCoordSystems.size());
            copy(myCoordSystems.begin(), myCoordSystems.end(), back_inserter(coordSystems));
        } else {
            LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "no convention found for convention: " << builder->getName());
        }
    }
    if (coordSystems.size() == 0) {
        LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "no regular conventions found, wild-guess checking CF");
        coordSystems = cfBuilder->listCoordinateSystems(cdm);
    }

    LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "total conventions found: " << coordSystems.size());
    return coordSystems;
}
std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystems(boost::shared_ptr<CDMReader> reader)
{
    // the return value
    vector<boost::shared_ptr<const CoordinateSystem> > coordSystems;

    vector<boost::shared_ptr<CoordSysBuilder> > builders;
    // supported conventions
    boost::shared_ptr<CoordSysBuilder> cfBuilder(boost::shared_ptr<CoordSysBuilder>(new CF1_xCoordSysBuilder()));
    builders.push_back(cfBuilder);
    builders.push_back(boost::shared_ptr<CoordSysBuilder>(new WRFCoordSysBuilder()));

    for (size_t i = 0; i < builders.size(); ++i) {
        boost::shared_ptr<CoordSysBuilder> builder = builders.at(i);
        if (builder->isMine(reader->getCDM())) {
            vector<boost::shared_ptr<const CoordinateSystem> > myCoordSystems = builder->listCoordinateSystems(reader);
            LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "found convention: " << builder->getName() << ", amount: " << myCoordSystems.size());
            copy(myCoordSystems.begin(), myCoordSystems.end(), back_inserter(coordSystems));
        } else {
            LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "no convention found for convention: " << builder->getName());
        }
    }
    if (coordSystems.size() == 0) {
        LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "no regular conventions found, wild-guess checking CF");
        coordSystems = cfBuilder->listCoordinateSystems(reader);
    }

    LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "total conventions found: " << coordSystems.size());
    return coordSystems;
}

void enhanceVectorProperties(boost::shared_ptr<CDMReader> reader)
{
    vector<boost::shared_ptr<CoordSysBuilder> > builders;
    // supported conventions
    boost::shared_ptr<CoordSysBuilder> cfBuilder(boost::shared_ptr<CoordSysBuilder>(new CF1_xCoordSysBuilder()));
    builders.push_back(cfBuilder);
    builders.push_back(boost::shared_ptr<CoordSysBuilder>(new WRFCoordSysBuilder()));

    bool found = false;
    for (size_t i = 0; i < builders.size(); ++i) {
        boost::shared_ptr<CoordSysBuilder> builder = builders.at(i);
        if (builder->isMine(reader->getCDM())) {
            found = true;
            builder->enhanceVectorProperties(reader);
            LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "found convention: " << builder->getName() << ", enhancing vectors");
        } else {
            LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "no convention found for convention: " << builder->getName());
        }
    }
    if (!found) {
        LOG4FIMEX(getLogger(logCat), Logger::DEBUG, "no regular conventions found, wild-guess checking CF");
        cfBuilder->enhanceVectorProperties(reader);
    }
}


}
