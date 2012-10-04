/*
 * Fimex, CDMMerger.cc
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
 *  Created on: Aug 28, 2012
 *      Author: Alexander Bürger
 */

#include "fimex/CDMMerger.h"
#include "fimex/CDMMerger_LinearSmoothing.h"

#define THROW(x) do { std::ostringstream t; t << x; throw CDMException(t.str()); } while(false)

#include "fimex/CDM.h"
#include "fimex/CDMconstants.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/coordSys/CoordinateAxis.h"
#include "fimex/DataIndex.h"
#include "fimex/Logger.h"

#include <boost/shared_ptr.hpp>

#include <sstream>

using namespace MetNoFimex;
using namespace std;

// ========================================================================

namespace MetNoFimex {

static LoggerPtr logger(getLogger("fimex.CDMMerger"));

typedef boost::shared_ptr<CDMInterpolator> CDMInterpolatorPtr;
typedef boost::shared_ptr<Data> DataPtr;
typedef boost::shared_ptr<CDMReader> CDMReaderPtr;
typedef boost::shared_ptr<const CoordinateSystem> CoordinateSystemPtr;
typedef boost::shared_ptr<const Projection> ProjectionPtr;

// ========================================================================

struct CDMMergerPrivate {
    CDMReaderPtr readerI;
    CDMReaderPtr readerO;

    int gridInterpolationI, gridInterpolationO;

    CDMMerger::SmoothingFactoryPtr smoothingFactory;

    CDMInterpolatorPtr interpolatedO;

    vector<CoordinateSystemPtr> allInnerCS, allOuterCS;

    struct MergedVariable {
        string nameI;
        string nameO;
        CDMReaderPtr interpolatedI;
        MergedVariable(string nI, string nO, CDMReaderPtr iI)
            : nameI(nI), nameO(nO), interpolatedI(iI) { }
    };
    
    vector<MergedVariable> mergedVariables;

    size_t beginIX, endIX, beginIY, endIY;
    string nameIX, nameIY, nameOX, nameOY;


    void checkAxesCompatibility(CoordinateSystemPtr csI, CoordinateSystemPtr csO);
    void checkSingleAxisCompatibility(CoordinateSystem::ConstAxisPtr axI, CoordinateSystem::ConstAxisPtr axO, const char* messageLabel);

    std::vector<double> mergeAxis(const CDM& cdmI, const CDM& cdmO, CoordinateSystem::ConstAxisPtr axisI, CoordinateSystem::ConstAxisPtr axisO,
                                  std::size_t& beginI, std::size_t& endI, bool& distinct, const char* unit);
};

// ========================================================================

namespace { // anonymous

inline bool equal(double a, double b)
{
    return fabs(a-b) < 1e-6;
}

struct FindOuterName {
    string nameO;
    FindOuterName(const string& nO) : nameO(nO) { }
    bool operator()(const CDMMergerPrivate::MergedVariable& mv) const
        { return nameO == mv.nameO; }
};

} // anonymous namespace

// ========================================================================

CDMMerger::CDMMerger(CDMReaderPtr inner, CDMReaderPtr outer)
    : p(new CDMMergerPrivate)
{
    p->readerI = inner;
    p->readerO = outer;

    p->gridInterpolationI = p->gridInterpolationO = MIFI_INTERPOL_BILINEAR;

    p->smoothingFactory = SmoothingFactoryPtr(new CDMMerger_LinearSmoothingFactory());

    p->allInnerCS = listCoordinateSystems(p->readerI);
    p->allOuterCS = listCoordinateSystems(p->readerO);

    p->interpolatedO = boost::shared_ptr<CDMInterpolator>(new CDMInterpolator(p->readerO));

    *cdm_ = p->interpolatedO->getCDM();
}

// ------------------------------------------------------------------------

void CDMMerger:: setSmoothing(SmoothingFactoryPtr smoothingFactory)
{
    // TODO throw if already running
    p->smoothingFactory = smoothingFactory;
}

// ------------------------------------------------------------------------

void CDMMerger::setGridInterpolationMethod(int methodI, int methodO)
{
    // TODO throw if already running
    p->gridInterpolationI = methodI;
    p->gridInterpolationO = methodO;
}

// ------------------------------------------------------------------------

void CDMMerger::addMergedVariable(const std::string& nameI, const std::string& nameO)
{
    LOG4FIMEX(logger, Logger::DEBUG, "adding merged variable '" << nameI << "' in inner and '" << nameO << "' in outer");
    const CDM &cdmI = p->readerI->getCDM(), &cdmO = p->readerO->getCDM();
    if( not cdmI.hasVariable(nameI) )
        THROW("inner does not have a variable named '" << nameI << "'");
    if( not cdmO.hasVariable(nameO) )
        THROW("outer does not have a variable named '" << nameO << "'");

    if( cdmI.hasDimension(nameI) )
        THROW("variable '" << nameI << "' is a dimension in inner, cannot merge");
    if( cdmO.hasDimension(nameO) )
        THROW("variable '" << nameO << "' is a dimension in outer, cannot merge");

    const CDMVariable& varI = cdmI.getVariable(nameI), varO = cdmO.getVariable(nameO);

    const vector<string>& shapeI = varI.getShape(), &shapeO = varO.getShape();
    if( shapeI.size() != shapeO.size() )
        THROW("variables '" << nameI << "' in inner and '" << nameO << "' in outer have different shape size, cannot merge");
    if( shapeI != shapeO ) {
        LOG4FIMEX(logger, Logger::WARN, "variables '" << nameI << "' in inner and '" << nameO
                  << "' in outer seem to have different shapes, merge outcome is unclear (crash possible)");
    }

#if 0
    const vector<CDMAttribute>& attributesI = cdmI.getAttributes(nameI);
    if( attributesI.size() != cdmO.getAttributes(nameO).size() )
        THROW("variable '" << nameI << "' in inner and '" << nameO << "' in outer have different attribute count, cannot merge");
    BOOST_FOREACH(const CDMAttribute& aI, attributesI) {
        try {
            const CDMAttribute& aO = cdmO.getAttribute(nameO, aI.getName());
            if( aI.getStringValue() != aO.getStringValue() )
                THROW("attibute '" << aI.getName() << "' has different values for variable '"
                      << nameI << "' in inner and '" << nameO << "' in outer, cannot merge");
        } catch(CDMException& e) {
            THROW("attribute '" << aI.getName() << "' of variable '" << nameI << "' in inner is not present for variable '"
                  << nameO << "' in outer, cannot merge");
        }
    }
#endif

    const vector<CoordinateSystemPtr>::iterator itI =
        find_if(p->allInnerCS.begin(), p->allInnerCS.end(), CompleteCoordinateSystemForComparator(nameI));
    const vector<CoordinateSystemPtr>::iterator itO =
        find_if(p->allOuterCS.begin(), p->allOuterCS.end(), CompleteCoordinateSystemForComparator(nameO));
    const bool hasInnerCS = (itI != p->allInnerCS.end()), hasOuterCS = (itO != p->allOuterCS.end());
    if( not hasInnerCS )
        THROW("no coordinate system for variable '" << nameI << "' in inner, cannot merge");
    if( not hasOuterCS )
        THROW("no coordinate system for variable '" << nameO << "' in outer, cannot merge");

    const CoordinateSystemPtr csI = *itI, csO = *itO;
    if( not (csI->isSimpleSpatialGridded() and csO->isSimpleSpatialGridded()) )
        THROW("coordinate systems for variable '" << nameO << "' in outer or '"
              << nameI << "' in inner is not a simple spatial grid, merge not implemented");
    p->checkAxesCompatibility(csI, csO);

    if( not (csI->hasProjection() and csO->hasProjection()) )
        THROW("coordinate systems for variable '" << nameO << "' in outer or '"
              << nameI << "' in inner have no projection, merge not implemented");
    ProjectionPtr projI = csI->getProjection(), projO = csO->getProjection();
    if( projI->isDegree() != projO->isDegree() )
        THROW("coordinate systems for variable '" << nameO << "' in outer or '"
              << nameI << "' are not both in degree/m, merge not implemented");

    if( p->mergedVariables.empty() ) {
        const char* unit = projO->isDegree() ? "degree" : "m";
        bool distinctX = true, distinctY = true;
        const vector<double> valuesX = p->mergeAxis(cdmI, cdmO, csI->getGeoXAxis(), csO->getGeoXAxis(), p->beginIX, p->endIX, distinctX, unit);
        const vector<double> valuesY = p->mergeAxis(cdmI, cdmO, csI->getGeoYAxis(), csO->getGeoYAxis(), p->beginIY, p->endIY, distinctY, unit);
        p->nameIX = csI->getGeoXAxis()->getName();
        p->nameIY = csI->getGeoYAxis()->getName();
        p->nameOX = csO->getGeoXAxis()->getName();
        p->nameOY = csO->getGeoYAxis()->getName();

        if( distinctX or distinctY ) {
            p->interpolatedO->changeProjection(p->gridInterpolationO, projO->getProj4String(),
                                               valuesX, valuesY, unit, unit, CDM_DOUBLE, CDM_DOUBLE);
            *cdm_ = p->interpolatedO->getCDM();
        }
    } else {
        // TODO if inner has different grid than first mapped variable, also interpolate inner for this variable
        if( p->nameIX != csI->getGeoXAxis()->getName() or p->nameIY != csI->getGeoYAxis()->getName() )
            THROW("variable '" << nameI << "' in inner has other horizontal axes than"
                  << " the first merged variable, merge not implemented");
    }
    p->mergedVariables.push_back(CDMMergerPrivate::MergedVariable(nameI, nameO, p->readerI));
}

// ------------------------------------------------------------------------

void CDMMergerPrivate::checkAxesCompatibility(CoordinateSystemPtr csI, CoordinateSystemPtr csO)
{
    const int N = 5;
    const CoordinateAxis::AxisType types[N] = {
        CoordinateAxis::GeoZ,
        CoordinateAxis::Time,
        CoordinateAxis::Pressure,
        CoordinateAxis::Height,
        CoordinateAxis::ReferenceTime
    };
    const char* labels[N] = {
        "z", "time", "pressure", "height", "reference time"
    };
    for(int i=0; i<N; ++i)
        checkSingleAxisCompatibility(csI->findAxisOfType(types[i]), csO->findAxisOfType(types[i]), labels[i]);
}

// ------------------------------------------------------------------------

void CDMMergerPrivate::checkSingleAxisCompatibility(CoordinateSystem::ConstAxisPtr axI, CoordinateSystem::ConstAxisPtr axO, const char* messageLabel)
{
    const bool hasAxisI = (axI.get() != 0), hasAxisO = (axO.get() != 0);
    if( hasAxisI != hasAxisO )
        THROW("only one of inner and outer has a '" << messageLabel << "' axis, cannot merge");
    if( !hasAxisI )
        return;
    const CDM& cdmI = readerI->getCDM(), &cdmO = readerO->getCDM();
    if( not (cdmI.hasDimension(axI->getName()) and cdmO.hasDimension(axO->getName())) )
        THROW(messageLabel << " axis in inner or outer is not a dimension, giving up");
    const CDMDimension& dimI = cdmI.getDimension(axI->getName()), &dimO = cdmO.getDimension(axO->getName());
    if( dimI.isUnlimited() != dimO.isUnlimited() )
        THROW(messageLabel << " axis is not unlimited in both inner and outer, cannot merge");
    DataPtr dataI = axI->getData(), dataO = axO->getData();
    if( (not dataI) != (not dataO) )
        THROW(messageLabel << " axis has data only for one of inner and outer, cannot merge");
    if( dataI.get() != 0 and dataI->size() != dataO->size() )
        THROW(messageLabel << " axis has different size in inner and outer, cannot merge");
}

// ------------------------------------------------------------------------

vector<double> CDMMergerPrivate::mergeAxis(const CDM& cdmI, const CDM& cdmO,
                                           CoordinateSystem::ConstAxisPtr axisI, CoordinateSystem::ConstAxisPtr axisO,
                                           size_t& beginI, size_t& endI, bool& distinct, const char* unit)
{
    const string &nameI = axisI->getName(), &nameO = axisO->getName();

    if( not (cdmI.hasDimension(nameI) and cdmO.hasDimension(nameO)) )
        THROW("found an axis that is not a dimension, cannot merge");

    const CDMDimension& dimI = cdmI.getDimension(nameI), &dimO = cdmO.getDimension(nameO);
    if( dimI.isUnlimited() != dimO.isUnlimited() )
        THROW("axis '" << nameI << "' / '" << nameO << "' is not unlimited in both inner and outer, cannot merge");

    DataPtr axisDataI = readerI->getScaledDataInUnit(nameI, unit), axisDataO = readerO->getScaledDataInUnit(nameO, unit);

    if( !axisDataI || axisDataI->size() < 2 )
        THROW("no data for axis '" << nameI << "' in inner");
    if( !axisDataO || axisDataO->size() < 2 )
        THROW("no data for axis '" << nameO << "' in outer");

    boost::shared_array<double> valuesI = axisDataI->asDouble(), valuesO = axisDataO->asDouble();
    const double stepI = valuesI[1] - valuesI[0], stepO = valuesO[1] - valuesO[0];

    for(size_t i=2; i<axisDataI->size(); ++i) {
        if( !equal(valuesI[i] - valuesI[i-1], stepI) )
            THROW("axis '" << nameI << "' in inner does not have constant step size, cannot merge");
    }
    for(size_t i=2; i<axisDataO->size(); ++i) {
        if( !equal(valuesO[i] - valuesO[i-1], stepO) )
            THROW("axis '" << nameO << "' in outer does not have constant step size, cannot merge");
    }

    const double minI = stepI > 0 ? valuesI[0] : valuesI[axisDataI->size()-1], minO = stepO > 0 ? valuesO[0] : valuesO[axisDataO->size()-1];
    const double maxI = stepI < 0 ? valuesI[0] : valuesI[axisDataI->size()-1], maxO = stepO < 0 ? valuesO[0] : valuesO[axisDataO->size()-1];
    if( minI < minO || maxI > maxO )
        THROW("top not inside  bottom");

    vector<double> reverse;
    for(double nO = valuesI[0] - stepI; nO >= minO && nO <= maxO; nO -= stepI)
        reverse.push_back(nO);
    vector<double> extended(reverse.rbegin(), reverse.rend());
    beginI = extended.size();
    extended.insert(extended.end(), &valuesI[0], &valuesI[axisDataI->size()]);
    endI = extended.size();
    for(double nO = valuesI[axisDataI->size()-1] + stepI; nO >= minO && nO <= maxO; nO += stepI)
        extended.push_back(nO);
    distinct = (extended[0] != valuesO[0] or extended[1] != valuesO[1] or extended.size() != axisDataO->size());
    return extended;
}

// ------------------------------------------------------------------------

DataPtr CDMMerger::getDataSlice(const std::string &varName, size_t unLimDimPos)
{
    vector<CDMMergerPrivate::MergedVariable>::const_iterator it
        = find_if(p->mergedVariables.begin(), p->mergedVariables.end(), FindOuterName(varName));
    if( it == p->mergedVariables.end() )
        return p->interpolatedO->getDataSlice(varName, unLimDimPos);

    const CDMMergerPrivate::MergedVariable& mv = *it;
    const CDM& cdmI = mv.interpolatedI->getCDM();

    const vector<string> &shapeO = cdm_->getVariable(varName).getShape(), &shapeI = cdmI.getVariable(mv.nameI).getShape();
    vector<size_t> dimSizesI, dimSizesO;
    int shapeIdxXI = -1, shapeIdxYI = -1, shapeIdxXO = -1, shapeIdxYO = -1;
    for(size_t i=0; i<shapeO.size(); ++i) {
        if( p->nameOX == shapeO[i] )
            shapeIdxXO = i;
        else if( p->nameOY == shapeO[i] )
            shapeIdxYO = i;

        if( p->nameIX == shapeI[i] )
            shapeIdxXI = i;
        else if( p->nameIY == shapeI[i] )
            shapeIdxYI = i;

        const CDMDimension& dimO = cdm_->getDimension(shapeO[i]);
        if( !dimO.isUnlimited() ) {
            dimSizesI.push_back(cdmI.getDimension(shapeI[i]).getLength());
            dimSizesO.push_back(dimO.getLength());
        }
    }

    DataPtr sliceO = p->interpolatedO->getDataSlice(varName, unLimDimPos);
    if( dimSizesI.empty() )
        return sliceO;

    SmoothingPtr smoothing = (*p->smoothingFactory)(varName);
    const double fillI = cdmI.getFillValue(mv.nameI), fillO = cdm_->getFillValue(varName);
    smoothing->setFillValues(cdmI.getFillValue(mv.nameI), cdm_->getFillValue(varName));
    smoothing->setHorizontalSizes(dimSizesI[shapeIdxXI], dimSizesI[shapeIdxYI]);

    DataPtr sliceI = p->readerI->getDataSlice(varName, unLimDimPos);

    const DataIndex idxI(dimSizesI), idxO(dimSizesO);
    
    vector<size_t> currentI(dimSizesI.size(), 0), currentO(dimSizesI.size(), 0);
    if( shapeIdxXO >= 0 )
        currentO[shapeIdxXO] = p->beginIX;
    if( shapeIdxYO >= 0 )
        currentO[shapeIdxYO] = p->beginIY;
    while( currentI.at(0) < dimSizesI.at(0) ) {
        const size_t posI = idxI.getPos(currentI);
        const size_t posO = idxO.getPos(currentO);
        const double valueI = sliceI->getDouble(posI);
        const double valueO = sliceO->getDouble(posO);
        double merged;
        if( valueI == fillI ) {
            merged = valueO;
        } else if( valueO == fillO or (not smoothing.get()) ) {
            merged = valueI;
        } else {
            merged = (*smoothing)(currentI[shapeIdxXI], currentI[shapeIdxYI], valueI, valueO);
        }
        sliceO->setValue(posO, merged);
        
        size_t incIdx = 0;
        while(incIdx < currentI.size()) {
            currentI[incIdx] += 1;
            currentO[incIdx] += 1;
            if( currentI[incIdx] >= dimSizesI[incIdx] ) {
                currentI[incIdx] = 0;
                // FIXME merging of different dimension names/ordering is not implemented, it will just be chaos
                if( static_cast<int>(incIdx) == shapeIdxXI )
                    currentO[incIdx] = p->beginIX;
                else if( static_cast<int>(incIdx) == shapeIdxYI )
                    currentO[incIdx] = p->beginIY;
                else
                    currentO[incIdx] = 0;
                incIdx += 1;
            } else {
                break;
            }
        }
        if( incIdx >= currentI.size() )
            break;
    }
    return sliceO;
}

} // namespace MetNoFimex
