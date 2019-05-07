/*
 * Fimex, CoordinateSystemSliceBuilder.cc
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Jun 1, 2011
 *      Author: Heiko Klein
 */

#include "fimex/CoordinateSystemSliceBuilder.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/CDMException.h"
#include "fimex/Logger.h"
#include "fimex/CDM.h"

#include <algorithm>

namespace MetNoFimex
{

static Logger_p logger = getLogger("fimex.CoordinateSystemSliceBuilder");

using namespace std;

const std::string& getCSVarFromCDM_(const CDM& cdm, CoordinateSystem_cp cs)
{
    const CDM::VarVec& vars = cdm.getVariables();
    for (CDM::VarVec::const_iterator v = vars.begin(); v != vars.end(); ++v) {
        if (cs->isCSAndCompleteFor(v->getName())) {
            // return first variable
            return v->getName();
        }
    }
    throw CDMException("no variable belonging to CoordinateSystem found in CoordinateSystem for CoordinateSystemSliceBuilder");
}

CoordinateSystemSliceBuilder::CoordinateSystemSliceBuilder(const CDM& cdm, CoordinateSystem_cp cs)
    : SliceBuilder(cdm, getCSVarFromCDM_(cdm, cs))
    , cs_(cs)
{
    setReferenceTimePos(0); // referenceTime is allways set
    if (cs_->hasAxisType(CoordinateAxis::Time)) {
        tShape_ = cs_->getTimeAxis()->getShape();
    }
}

void CoordinateSystemSliceBuilder::setReferenceTimePos(size_t refTimePos)
{
    if (cs_->hasAxisType(CoordinateAxis::ReferenceTime)) {
        CoordinateAxis_cp refAxis = cs_->findAxisOfType(CoordinateAxis::ReferenceTime);
        if (refAxis->isExplicit()) {
            setStartAndSize(refAxis, refTimePos, 1);
        } else {
            LOG4FIMEX(logger, Logger::DEBUG, "referenceTime " << refAxis->getName() << " not explicit, ignoring position");
        }
    }
}
void CoordinateSystemSliceBuilder::setTimeStartAndSize(size_t start, size_t size)
{
    if (cs_->hasAxisType(CoordinateAxis::Time)) {
        CoordinateAxis_cp tAxis = cs_->getTimeAxis();
        if (tAxis->isExplicit()) {
            setStartAndSize(tAxis, start, size);
        } else {
            set<string> tShape(tShape_.begin(), tShape_.end());
            // try to reduce the time-variables dimensions by extracting the refTime
            if (cs_->hasAxisType(CoordinateAxis::ReferenceTime)) {
                CoordinateAxis_cp refAxis = cs_->findAxisOfType(CoordinateAxis::ReferenceTime);
                tShape.erase(refAxis->getName());
            }
            if (tShape.size() == 1) {
                setStartAndSize(*(tShape.begin()), start, size);
            } else {
                throw CDMException("unable to setTimeStartAndSize");
            }
        }
    } else {
        LOG4FIMEX(logger, Logger::DEBUG, "CoordinateSystemSliceBuilder without time, ignoring setTimeStartAndSize()");
    }
}
SliceBuilder CoordinateSystemSliceBuilder::getTimeVariableSliceBuilder()
{
    vector<string> names = tShape_;
    vector<size_t> dimSize(tShape_.size());
    const vector<size_t>& iMaxDimensions = getMaxDimensionSizes();
    for (size_t pos = 0; pos < names.size(); ++pos) {
        size_t iPos = getDimPos(names.at(pos));
        dimSize.at(pos) = iMaxDimensions.at(iPos);
    }
    SliceBuilder sb(names, dimSize);
    // now reduce the slicebuilder to the current start and size
    const vector<size_t>& iStart = getDimensionStartPositions();
    const vector<size_t>& iSize = getDimensionSizes();
    for (size_t pos = 0; pos < names.size(); ++pos) {
        size_t iPos = getDimPos(names.at(pos));
        sb.setStartAndSize(names.at(pos), iStart.at(iPos), iSize.at(iPos));
    }
    return sb;
}

void CoordinateSystemSliceBuilder::setAxisStartAndSize(CoordinateAxis::AxisType axis, size_t start, size_t size)
{
    switch (axis) {
        case CoordinateAxis::ReferenceTime : setReferenceTimePos(start); break;
        case CoordinateAxis::Time : setTimeStartAndSize(start, size); break;
        default: {
            if (cs_->hasAxisType(axis)) {
                CoordinateAxis_cp_v axes = cs_->getAxes();
                for (CoordinateAxis_cp_v::const_iterator axIt = axes.begin(); axIt != axes.end(); ++axIt) {
                    if ((*axIt)->getAxisType() == axis) {
                        if ((*axIt)->isExplicit()) {
                            setStartAndSize(*axIt, start, size);
                        }
                    }
                }
            }
            break;
        }
    }
}

vector<CoordinateAxis::AxisType> CoordinateSystemSliceBuilder::getAxisTypes() const
{
    vector<string> dimNames = getDimensionNames();
    vector<CoordinateAxis::AxisType> retVal;
    CoordinateAxis_cp_v axes = cs_->getAxes();
    for (vector<string>::iterator it = dimNames.begin(); it != dimNames.end(); ++it) {
        CoordinateAxis_cp_v::const_iterator axIt = std::find_if(axes.begin(), axes.end(), CDMNameEqualPtr(*it));
        if (axIt != axes.end()) {
            LOG4FIMEX(logger, Logger::DEBUG, (*axIt)->getAxisType() << CoordinateAxis::type2int((*axIt)->getAxisType()));
            retVal.push_back((*axIt)->getAxisType());
        } else {
            // this should not happen
            throw CDMException("cannot find axis-type for dimension: " + *it);
        }
    }
    return retVal;
}

}
