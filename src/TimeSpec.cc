/*
 * Fimex, TimeSpec.cc
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
 *  Created on: Dec 11, 2008
 *      Author: Heiko Klein
 */

#include "fimex/TimeSpec.h"
#include "fimex/Utils.h"
#include <vector>
#include <algorithm>
#include <iterator>
#include "fimex/Logger.h"
#include "fimex/TimeUnit.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/regex.hpp>

namespace MetNoFimex
{

using namespace std;

LoggerPtr logger = getLogger("fimex.TimeSpec");

double translateRelativeTime(string value, double startOffset, double finalValue)
{
	double retVal;
	boost::smatch what;
	boost::regex finalVals("x\\s*([+-])?\\s*(\\d\\.?\\d*)?\\s*");
	if (boost::regex_search(value, what, finalVals)) {
		if (what.size() >= 3) {
			if (what[1] == '+') {
				retVal = finalValue + string2type<double>(what[2]);
			} else {
				retVal = finalValue - string2type<double>(what[2]);
			}
		} else {
			retVal = finalValue;
		}
	} else {
		retVal = startOffset + string2type<double>(value);
	}
	LOG4FIMEX(logger, Logger::DEBUG, "translateRelativeTime(" << value << "," << startOffset << ","<<finalValue << ")="<<retVal)
	return retVal;
}

TimeSpec::TimeSpec(const string& timeSpec, const FimexTime& startTime, const FimexTime& endTime) throw(CDMException)
	: outputUnit("seconds since 1970-01-01 00:00:00")
{
	LoggerPtr logger = getLogger("fimex.TimeSpec");
	LOG4FIMEX(logger, Logger::DEBUG, "getting timespec of " << timeSpec);
	vector<string> toks = tokenize(timeSpec, ";");
	std::string timeStepStr;
	std::string relativeUnit;
	for(vector<string>::iterator it = toks.begin(); it != toks.end(); ++it) {
		vector<string> extraParam = tokenize(*it, "=");
		if (extraParam.size() == 1) {
			if (timeStepStr == "") {
				timeStepStr = extraParam[0];
			} else {
				throw CDMException("time-steps redefined from " + timeStepStr + " to " + extraParam[0]);
			}
		} else if (extraParam.size() == 2) {
			if (extraParam[0] == "unit") {
				outputUnit = extraParam[1];
			} else if (extraParam[0] == "relativeUnit") {
				relativeUnit = extraParam[1];
			} else {
				throw CDMException("unknown timeSpec parameter: '" + extraParam[0] + "'");
			}
		} else {
			throw CDMException("unknown timeSpec section: '" + *it + "'");
		}
	}

	if (relativeUnit == "") {
		// absolute time in spec
		vector<string> times = tokenize(timeStepStr, ",");
		vector<string>::iterator dotsIt = find(times.begin(), times.end(), "...");
		if (dotsIt != times.end()) {
			if ((dotsIt != times.begin()) &&
				(dotsIt-1 != times.begin()) && // two predecessors
				(dotsIt+1 != times.end())) { // one successor
				transform(times.begin(), dotsIt, back_inserter(timeSteps), string2FimexTime);
				boost::posix_time::time_duration delta = timeSteps[timeSteps.size()-1] - timeSteps[timeSteps.size()-2];
				FimexTime last = string2FimexTime(*(dotsIt+1));
				FimexTime next = timeSteps[timeSteps.size()-1] + delta;
				while (next < last) {
					timeSteps.push_back(next);
					next = next + delta;
				}
				dotsIt++; // forward from "..."
				transform(dotsIt, times.end(), back_inserter(timeSteps), string2FimexTime);
			} else {
				throw CDMException("... in TimeSpec("+timeStepStr+") needs two predecessors and one successor");
			}
		} else {
			transform(times.begin(), times.end(), back_inserter(timeSteps), string2FimexTime);
		}
	} else {
		// relative times
		vector<string> times = tokenize(timeStepStr, ",");
		if (times.size() < 2) {
			throw CDMException("TimeSpec requires at least 2 times for relative time definition, got: " + timeStepStr);
		}
		double delta = string2type<double>(times[1]) - string2type<double>(times[0]);
		TimeUnit tu(relativeUnit);
		// find the position 0 in the relative times
		double startTimeU = tu.fimexTime2unitTime(startTime);
		double startOffset = static_cast<int>(startTimeU / delta) * delta;
		if (startTimeU < startOffset) {
			// cast to largest value below startTimeU
			// but I want smallest value above startTimeU
			startOffset += delta;
		}
		// find position x in the relative times
		double endTimeU = tu.fimexTime2unitTime(endTime);
		double finalTimeU = static_cast<int>(endTimeU / delta) * delta;
		vector<double> timeDoubles;
		// expand ... list and translate all times to doubles in timeUnits
		for (size_t i = 0; i < times.size(); i++) {
			string current = times[i];
			if (current == "...") {
				if (i < times.size()-1) {
					double last = translateRelativeTime(times[i+1], startOffset, finalTimeU);
					double curDelta = timeDoubles[i-1] - timeDoubles[i-2];
					double next = timeDoubles[i-1] + curDelta;
					while (next < last) {
						timeDoubles.push_back(next);
						next += curDelta;
					}
				} else {
					throw CDMException("TimeSpec cannot find continuation after ... in " + timeStepStr);
				}
			} else {
				timeDoubles.push_back(translateRelativeTime(times[i], startOffset, finalTimeU));
			}
		}
		// add the times as fimexTimes to timeSteps
		transform(timeDoubles.begin(), timeDoubles.end(), back_inserter(timeSteps), bind1st(mem_fun_ref(&TimeUnit::unitTime2fimexTime),tu));
	}

	LOG4FIMEX(logger, Logger::DEBUG, "got parameters unit:" << outputUnit << ";relativeUnit:" << relativeUnit << ";steps:"<<timeStepStr);
}


} /* MetNoFimex */
