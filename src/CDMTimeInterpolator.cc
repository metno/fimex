/*
 * Fimex, CDMTimeInterpolator.cpp
 *
 * (C) Copyright 2008, met.no
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
 *  Created on: Dec 3, 2008
 *      Author: heikok
 */

#include "fimex/CDMTimeInterpolator.h"
#include <set>
#include <functional>
#include <algorithm>
#include <utility>
#include <fimex/CDM.h>
#include "fimex/TimeSpec.h"
#include "fimex/Data.h"
#include "fimex/interpolation.h"
#include "fimex/Logger.h"
#include "MutexLock.h"

namespace MetNoFimex
{

using namespace std;

static LoggerPtr logger = getLogger("fimex.CDMTimeInterpolator");

typedef std::vector<boost::shared_ptr<const CoordinateSystem> > CoordSysList;
// TODO: this function is mostly copied from CDM::getTimeAxis
//       CDM.h should provide a thread-safe version of getTimeAxis
static std::string getTimeAxis(const CoordSysList& cs, const CDM& cdm, const std::string& varName)
{
    // check if variable is its own axis (coord-axis don't have coordinate system)
    for (CoordSysList::const_iterator csIt = cs.begin(); csIt != cs.end(); ++csIt) {
        CoordinateSystem::ConstAxisPtr timeAxis = (*csIt)->getTimeAxis();
        if (timeAxis.get() != 0 && timeAxis->getName() == varName) {
            return varName;
        }
    }

    // search for coordinate system for varName
    CoordSysList::const_iterator varSysIt = find_if(cs.begin(), cs.end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt == cs.end()) {
        return "";
    }
    CoordinateSystem::ConstAxisPtr axis = (*varSysIt)->getTimeAxis();
    return (axis.get() == 0) ? "" : axis->getName();
}

CDMTimeInterpolator::CDMTimeInterpolator(boost::shared_ptr<CDMReader> dataReader)
   : dataReader_(dataReader)
{
    coordSystems_ = listCoordinateSystems(dataReader_);
    *cdm_ = dataReader_->getCDM();
    // removing all time-dependant data in cdm
    // just to be sure it's read from the dataReader_ or assigned in #changeTimeAxis
    const CDM::VarVec& variables = cdm_->getVariables();
    for (CDM::VarVec::const_iterator it = variables.begin(); it != variables.end(); ++it) {
        std::string timeDimName = getTimeAxis(coordSystems_, getCDM(), it->getName());
        if (timeDimName != "") {
            cdm_->getVariable(it->getName()).setData(boost::shared_ptr<Data>());
        }
    }
}

CDMTimeInterpolator::~CDMTimeInterpolator()
{
}

MutexType staticMutex;
boost::shared_ptr<Data> CDMTimeInterpolator::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    std::string timeAxis = getTimeAxis(coordSystems_, getCDM(), varName);
    LOG4FIMEX(logger, Logger::DEBUG, "getting time-interpolated data-slice for " << varName << " with time-axis: " << timeAxis);
    if (timeAxis == "" || (dataReaderTimesInNewUnits_.find(timeAxis)->second.size() == 0)) {
        // not time-axis or "changeTimeAxis" never called
        // no changes, simply forward
        return dataReader_->getDataSlice(varName, unLimDimPos);
    }

    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        return getDataSliceFromMemory(variable, unLimDimPos);
    }

    // interpolate the data
    boost::shared_ptr<Data> data;

    // if unlimdim = time-axis, fetch all needed original slices
    CDMDimension timeDim = cdm_->getDimension(timeAxis);
    if (timeDim.isUnlimited()) {
        double currentTime = getDataSliceFromMemory(cdm_->getVariable(timeAxis), unLimDimPos)->asDouble()[0];
        // interpolate and return the time-slices
        pair<size_t, size_t> orgTimes = timeChangeMap_.find(timeAxis)->second.at(unLimDimPos);
        double d1Time = dataReaderTimesInNewUnits_.find(timeDim.getName())->second.at(orgTimes.first);
        double d2Time = dataReaderTimesInNewUnits_.find(timeDim.getName())->second.at(orgTimes.second);
        boost::shared_ptr<Data> d1 = dataReader_->getDataSlice(varName, orgTimes.first);
        boost::shared_ptr<Data> d2 = dataReader_->getDataSlice(varName, orgTimes.second);
        LOG4FIMEX(logger, Logger::DEBUG, "interpolation between " << d1Time << " and " << d2Time << " at " << currentTime);
        // convert if both slices are defined, otherwise, simply use the defined one or return undefined
        if (d1->size() == 0) {
            data = d2;
        } else if (d2->size() == 0) {
            data = d1;
        } else if (d1->size() == d2->size()) {
            boost::shared_array<float> out(new float[d1->size()]);
            mifi_get_values_linear_f(d1->asFloat().get(), d2->asFloat().get(), out.get(), d1->size(), d1Time, d2Time, currentTime);
            data = createData(d1->size(), out);
        } else {
            throw CDMException("getDataSlice for " + varName + ": got slices with different size");
        }
    } else {
        // TODO
        // else get original slice, subslice the needed time-slices
        // interpolate and return the time-slices
        throw CDMException("TimeDimension != unlimited dimension not implemented yet in CDMTimeInterpolator");
    }
    return data;
}

void CDMTimeInterpolator::changeTimeAxis(std::string timeSpec)
{
    // changing time-axes
    const CDM& orgCDM = dataReader_->getCDM();
    const CDM::VarVec& vars = orgCDM.getVariables();
    set<string> changedTimes;
    for (CDM::VarVec::const_iterator varIt = vars.begin(); varIt != vars.end(); ++varIt) {
        // change all different time-axes
        std::string timeDimName = orgCDM.getTimeAxis(varIt->getName());
        if (timeDimName != "" && changedTimes.find(timeDimName) == changedTimes.end()) {
            changedTimes.insert(timeDimName); // avoid double changes
            boost::shared_ptr<Data> times = dataReader_->getScaledData(timeDimName);
            string unit = cdm_->getAttribute(timeDimName, "units").getStringValue();
            TimeUnit tu(unit);
            vector<FimexTime> oldTimes;
            boost::shared_array<double> oldTimesPtr = times->asDouble();
            size_t nEl = times->size();
            transform(oldTimesPtr.get(),
                      oldTimesPtr.get()+nEl,
                      back_inserter(oldTimes),
                      bind1st(mem_fun_ref(&TimeUnit::unitTime2fimexTime),tu));
            TimeSpec ts(timeSpec, oldTimes[0], oldTimes[nEl-1]);

            // create mapping of new time value positions to old time values (per time-axis)
            const vector<FimexTime>& newTimes = ts.getTimeSteps();
            size_t newTimePos = 0;
            size_t lastPos = 0;
            vector<pair<size_t, size_t> > timeMapping(newTimes.size());
            for (vector<FimexTime>::const_iterator it = newTimes.begin(); it != newTimes.end(); ++it, ++newTimePos) {
                vector<FimexTime>::iterator olb = lower_bound(oldTimes.begin() + lastPos, oldTimes.end(), *it);
                size_t pos = distance(oldTimes.begin(), olb);
                size_t t1, t2;
                if (pos == oldTimes.size()) {
                    // extrapolation at the end, starting with last and previous element
                    pos--;
                }
                t2 = pos;
                if (pos != 0) {
                    t1 = pos-1;
                } else { // extrapolation at beginning
                    t1 = pos;
                    if ((pos+1) != oldTimes.size()) {
                        t2 = pos+1;
                    } else {// only one element, t1 = t2, implicit
                        assert(oldTimes.size() == 1);
                    }
                }
                lastPos = pos; // make search faster, we know that the next lower_bound_pos will be >= pos
                timeMapping[newTimePos] = make_pair<size_t, size_t>(t1,t2);
            }
            timeChangeMap_[timeDimName] = timeMapping;


            // change cdm timeAxis values
            cdm_->addOrReplaceAttribute(timeDimName, CDMAttribute("units", ts.getUnitString()));
            boost::shared_array<double> timeData(new double[newTimes.size()]);
            TimeUnit newTU(ts.getUnitString());
            transform(newTimes.begin(), newTimes.end(), timeData.get(),
                      bind1st(mem_fun_ref(&TimeUnit::fimexTime2unitTimeX),newTU));
            cdm_->getVariable(timeDimName).setData(createData(newTimes.size(), timeData));
            cdm_->getDimension(timeDimName).setLength(newTimes.size());

            // store old times with new unit as oldTimesNewUnits-vector
            Units u;
            double slope, offset;
            u.convert(unit, ts.getUnitString(), slope, offset);
            dataReaderTimesInNewUnits_[timeDimName].clear();
            transform(oldTimesPtr.get(),
                      oldTimesPtr.get() + nEl,
                      back_inserter(dataReaderTimesInNewUnits_[timeDimName]),
                      ScaleValue<double,double>(MIFI_UNDEFINED_D, 1., 0., cdm_->getFillValue(timeDimName), slope, offset));
        }
    }
}

} /* MetNoFimex */
