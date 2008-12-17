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
#include "fimex/TimeSpec.h"
#include "fimex/DataImpl.h"

namespace MetNoFimex
{

using namespace std;

CDMTimeInterpolator::CDMTimeInterpolator(boost::shared_ptr<CDMReader> dataReader)
   : dataReader(dataReader)
{
	cdm = dataReader->getCDM();
}

CDMTimeInterpolator::~CDMTimeInterpolator()
{
}

const boost::shared_ptr<Data> CDMTimeInterpolator::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
	// TODO

	// if unlimdim = time-axis, fetch all needed original slices
	// else get original slice, subslice the needed time-slices
	//
	// interpolate and return the time-slices
}

/**
 *  find the lower bound of value val in vector ft starting from startPos. This is similar to
 *  std::lower_bound, except that it returns an array-position rather than an iterator
 *
 *  @return position of lower_bound of val, might be == ft.size() if all values are smaller.
 */
size_t lower_bound_pos(const vector<FimexTime>& ft, size_t startPos, const FimexTime& val)
{
	if (startPos >= ft.size()) {
		return ft.size();
	}
	for (size_t i = startPos; i < ft.size(); ++i) {
		if (val <= ft[i]) {
			return i;
		}
	}
	return ft.size();
}

void CDMTimeInterpolator::changeTimeAxis(std::string timeSpec) throw(CDMException)
{
	// TODO
	const CDM& orgCDM = dataReader->getCDM();
	const CDM::VarVec& vars = orgCDM.getVariables();
	set<string> changedTimes;
	for (CDM::VarVec::const_iterator varIt = vars.begin(); varIt != vars.end(); ++varIt) {
		// change time-axis of each variable, possibly different time-axis
		std::string timeDimName = orgCDM.getTimeAxis(varIt->getName());
		if (timeDimName != "" && changedTimes.find(timeDimName) == changedTimes.end()) {
			changedTimes.insert(timeDimName); // avoid double changes
			boost::shared_ptr<Data> times = dataReader->getData(timeDimName);
			string unit = cdm.getAttribute(timeDimName, "units").getStringValue();
			TimeUnit tu(unit);
			vector<FimexTime> oldTimes;
			boost::shared_array<double> oldTimesPtr = times->asConstDouble();
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
			vector<pair<size_t, size_t> > timeMapping;
			for (vector<FimexTime>::const_iterator it = newTimes.begin(); it != newTimes.end(); ++it, ++newTimePos) {
				size_t pos = lower_bound_pos(oldTimes, lastPos, *it);
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
			timeChangeMap[timeDimName] = timeMapping;


			// change cdm timeAxis values
			cdm.addOrReplaceAttribute(timeDimName, CDMAttribute("units", ts.getUnitString()));
			CDMVariable timeVar = cdm.getVariable(timeDimName);
			boost::shared_array<double> timeData(new double[newTimes.size()]);
			TimeUnit newTU(ts.getUnitString());
			typedef const FimexTime& cftref;
			transform(newTimes.begin(), newTimes.end(), timeData.get(),
					  bind1st(mem_fun_ref(&TimeUnit::fimexTime2unitTime),newTU));
			timeVar.setData(boost::shared_ptr<Data>(new DataImpl<double>(timeData, newTimes.size())));
			cdm.removeVariable(timeDimName);
			cdm.addVariable(timeVar);

		}
	}
}

} /* MetNoFimex */
