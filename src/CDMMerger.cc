/*
 * Fimex, CDMMerger.cc
 *
 * (C) Copyright 2012-2013, met.no
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

#include "fimex/CDM.h"
#include "fimex/CDMBorderSmoothing_Linear.h"
#include "fimex/CDMException.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/CDMOverlay.h"
#include "fimex/CDMconstants.h"
#include "fimex/Data.h"
#include "fimex/DataIndex.h"
#include "fimex/Logger.h"
#include "fimex/SpatialAxisSpec.h"
#include "fimex/coordSys/CoordinateAxis.h"

#include "CDMMergeUtils.h"

// clang-format off
#define THROW(x) do { std::ostringstream t; t << x; throw CDMException(t.str()); } while(false)
// clang-format on

using namespace MetNoFimex;
using namespace std;

// ========================================================================

namespace MetNoFimex {

static Logger_p logger(getLogger("fimex.CDMMerger"));

// ========================================================================

struct CDMMergerPrivate {
    CDMReader_p readerI;
    CDMReader_p readerO;

    CDMInterpolator_p interpolatedOT;  //! outer interpolated to target grid
    CDMBorderSmoothing_p readerSmooth; //! smoothed (inner+outer) on inner grid
    CDMInterpolator_p interpolatedST;  //! smoothed (inner+outer), interpolated to target grid
    CDMOverlay_p readerOverlay;        //! final result, we forward getDataSlice there

    int gridInterpolationMethod;
    bool useOuterIfInnerUndefined;
    bool keepOuterVariables;
    CDMBorderSmoothing::SmoothingFactory_p smoothingFactory;

    CDM makeCDM(const string& tproj, const values_v& tx, const values_v& ty,
            const std::string& tx_unit, const std::string& ty_unit,
            const CDMDataType& tx_type, const CDMDataType& ty_type);
    values_v extendInnerAxis(CoordinateAxis_cp axisI, CoordinateAxis_cp axisO, const char* unit);
};

// ========================================================================

CDMMerger::CDMMerger(CDMReader_p inner, CDMReader_p outer)
    : p(new CDMMergerPrivate)
{
    p->readerI = inner;
    p->readerO = outer;
    p->gridInterpolationMethod = MIFI_INTERPOL_BILINEAR;
    p->smoothingFactory = CDMBorderSmoothing::SmoothingFactory_p(new CDMBorderSmoothing_LinearFactory());
    p->useOuterIfInnerUndefined = true;
    p->keepOuterVariables = false;
}

CDMMerger::~CDMMerger() {}

// ------------------------------------------------------------------------

void CDMMerger::setSmoothing(CDMBorderSmoothing::SmoothingFactory_p smoothingFactory)
{
    p->smoothingFactory = smoothingFactory;
    if (p->readerSmooth.get() != 0)
        p->readerSmooth->setSmoothing(smoothingFactory);
}

// ------------------------------------------------------------------------

void CDMMerger::setUseOuterIfInnerUndefined(bool useOuter)
{
    p->useOuterIfInnerUndefined = useOuter;
    if (p->readerSmooth.get() != 0)
        p->readerSmooth->setUseOuterIfInnerUndefined(useOuter);
}
// ------------------------------------------------------------------------

void CDMMerger::setKeepOuterVariables(bool keepOuterVariables)
{
    if (p->readerOverlay.get() != 0)
        LOG4FIMEX(logger, Logger::WARN, "setting keepOuterVariables after setting target grid has"
                " no effect on the already defined target grid");
    p->keepOuterVariables = keepOuterVariables;
}
// ------------------------------------------------------------------------

void CDMMerger::setGridInterpolationMethod(int method)
{
    if (p->readerOverlay.get() != 0)
        LOG4FIMEX(logger, Logger::WARN, "setting grid interpolation method after setting target grid has"
                " no effect on the already defined target grid");
    p->gridInterpolationMethod = method;
}

// ------------------------------------------------------------------------

void CDMMerger::setTargetGrid(const string& proj, const string& tx_axis, const string& ty_axis,
        const string& tx_unit, const string& ty_unit, const string& tx_type, const string& ty_type)
{
    SpatialAxisSpec xAxisSpec(tx_axis);
    SpatialAxisSpec yAxisSpec(ty_axis);
    if (xAxisSpec.requireStartEnd() or yAxisSpec.requireStartEnd())
        throw CDMException("setTargetGrid without axis start/end not implemented");

    const CDMDataType xType = string2datatype(tx_type),
            yType = string2datatype(ty_type);
    const vector<double> tx_values = xAxisSpec.getAxisSteps(),
            ty_values = yAxisSpec.getAxisSteps();
    setTargetGrid(proj, tx_values, ty_values, tx_unit, ty_unit, xType, yType);
}

// ------------------------------------------------------------------------

void CDMMerger::setTargetGrid(const string& proj, const values_v& tx, const values_v& ty,
        const std::string& tx_unit, const std::string& ty_unit,
        const CDMDataType& tx_type, const CDMDataType& ty_type)
{
    *cdm_ = p->makeCDM(proj, tx, ty, tx_unit, ty_unit, tx_type, ty_type);
}

// ------------------------------------------------------------------------

void CDMMerger::setTargetGridFromInner()
{
    const CDM &cdmI = p->readerI->getCDM(), &cdmO = p->readerO->getCDM();

    const CoordinateSystem_cp_v allCsI = listCoordinateSystems(p->readerI), allCsO = listCoordinateSystems(p->readerO);

    const CDM::VarVec& varsI = cdmI.getVariables();
    for (const CDMVariable& varI : varsI) {
        const string& varName = varI.getName();
        LOG4FIMEX(logger, Logger::DEBUG, "check variable '" << varName << "'");
        if (not cdmO.hasVariable(varName)) {
            LOG4FIMEX(logger, Logger::DEBUG, "variable '" << varName << "' not in outer");
            continue;
        }
        if (cdmI.hasDimension(varName) or cdmO.hasDimension(varName)) {
            LOG4FIMEX(logger, Logger::DEBUG, "variable '" << varName << "' has dimension (i.e. is an axis)");
            continue;
        }

        const CoordinateSystem_cp csI = findCompleteCoordinateSystemFor(allCsI, varName), csO = findCompleteCoordinateSystemFor(allCsO, varName);
        if (!csI.get() || !csO.get()) {
            LOG4FIMEX(logger, Logger::DEBUG, "no cs for '" << varName << "'");
            continue;
        }

        if (not (csI->isSimpleSpatialGridded() and csO->isSimpleSpatialGridded())) {
            LOG4FIMEX(logger, Logger::DEBUG, "variable '" << varName << "' cs is not simple spatial gridded");
            continue;
        }
        if (not (csI->hasProjection() and csO->hasProjection())) {
            LOG4FIMEX(logger, Logger::DEBUG, "no projection for '" << varName << "'");
            continue;
        }

        Projection_cp projI = csI->getProjection(), projO = csO->getProjection();
        if (projI->isDegree() != projO->isDegree()) {
            LOG4FIMEX(logger, Logger::DEBUG, "variable '" << varName << "' is degree in inner but not in outer, or vice-versa");
            continue;
        }

        CoordinateAxis_cp xAxisI = csI->getGeoXAxis(), yAxisI = csI->getGeoYAxis();

        const char* unit = projI->isDegree() ? "degree" : "m";
        const values_v vx = p->extendInnerAxis(xAxisI, csO->getGeoXAxis(), unit);
        const values_v vy = p->extendInnerAxis(yAxisI, csO->getGeoYAxis(), unit);

        LOG4FIMEX(logger, Logger::INFO, "extending grid for inner variable '" << varName << "'");
        setTargetGrid(projI->getProj4String(), vx, vy,
                cdmI.getUnits(xAxisI->getName()), cdmI.getUnits(yAxisI->getName()),
                xAxisI->getDataType(), yAxisI->getDataType());
        return;
    }

    LOG4FIMEX(logger, Logger::WARN, "extending grid failed, no inner variable with CS found");
}

// ------------------------------------------------------------------------

DataPtr CDMMerger::getDataSlice(const std::string &varName, size_t unLimDimPos)
{
    if (not p->readerOverlay)
        THROW("must call setTargetGrid or setTargetGridFromInner before getDataSlice");

    return p->readerOverlay->getDataSlice(varName, unLimDimPos);
}

// ========================================================================

CDM CDMMergerPrivate::makeCDM(const string& proj, const values_v& tx, const values_v& ty,
        const std::string& tx_unit, const std::string& ty_unit,
        const CDMDataType& tx_type, const CDMDataType& ty_type)
{
    readerSmooth = std::make_shared<CDMBorderSmoothing>(readerI, readerO);
    readerSmooth->setSmoothing(smoothingFactory);
    readerSmooth->setUseOuterIfInnerUndefined(useOuterIfInnerUndefined);

    interpolatedST = std::make_shared<CDMInterpolator>(readerSmooth);
    interpolatedST->changeProjection(gridInterpolationMethod, proj,
            tx, ty, tx_unit, ty_unit, tx_type, ty_type);

    readerOverlay = std::make_shared<CDMOverlay>(readerO, interpolatedST, gridInterpolationMethod, keepOuterVariables);

    return readerOverlay->getCDM();
}

// ------------------------------------------------------------------------

values_v CDMMergerPrivate::extendInnerAxis(CoordinateAxis_cp axisI, CoordinateAxis_cp axisO, const char* unit)
{
    const string &nameI = axisI->getName(), &nameO = axisO->getName();

    const CDM &cdmI = readerI->getCDM(), &cdmO = readerO->getCDM();
    if (not (cdmI.hasDimension(nameI) and cdmO.hasDimension(nameO)))
        THROW("found an axis that is not a dimension, cannot merge");

    const CDMDimension& dimI = cdmI.getDimension(nameI), &dimO = cdmO.getDimension(nameO);
    if( dimI.isUnlimited() != dimO.isUnlimited() )
        THROW("axis '" << nameI << "' / '" << nameO << "' is not unlimited in both inner and outer, cannot merge");

    DataPtr axisDataI = readerI->getScaledDataInUnit(nameI, unit), axisDataO = readerO->getScaledDataInUnit(nameO, unit);

    if( !axisDataI || axisDataI->size() < 2 )
        THROW("no data for axis '" << nameI << "' in inner");
    if( !axisDataO || axisDataO->size() < 2 )
        THROW("no data for axis '" << nameO << "' in outer");

    shared_array<double> valuesI = axisDataI->asDouble(), valuesO = axisDataO->asDouble();
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
    extended.insert(extended.end(), &valuesI[0], &valuesI[axisDataI->size()]);
    for(double nO = valuesI[axisDataI->size()-1] + stepI; nO >= minO && nO <= maxO; nO += stepI)
        extended.push_back(nO);
    return extended;
}

} // namespace MetNoFimex
