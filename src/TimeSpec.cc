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

#include "fimex/Logger.h"
#include "fimex/TimeUnit.h"
#include "fimex/Utils.h"

#include <algorithm>
#include <iterator>
#include <regex>
#include <vector>

namespace MetNoFimex
{

using namespace std;

static Logger_p logger = getLogger("fimex.TimeSpec");

/**
 * Translate the relative time-strings, e.g. 0,3,x,x+1 to startOffset,startOffset+3,finalTime,finalTime+1
 * @param value
 * @param startOffset
 * @param finalValue
 * @return
 */
static double translateRelativeTime(string value, double startOffset, double finalValue)
{
    double retVal = 0.;
    std::smatch what;
    std::regex finalVals("x\\s*([+-])?\\s*(\\d\\.?\\d*)?\\s*");
    if (std::regex_search(value, what, finalVals)) {
        assert(what.size() == 3); // defined by regex
        if (what[2] == "") {
            // only x is set
            retVal = finalValue;
        } else {
            if (what[1] == '+') {
                retVal = finalValue + string2type<double>(what[2]);
            } else if (what[1] != "-") {
                retVal = finalValue - string2type<double>(what[2]);
            } else {
                LOG4FIMEX(logger, Logger::WARN,
                          "translateRelativeTime(" << value << "," << startOffset << "," << finalValue << ")=" << retVal << " ignoring value after x");
            }
        }
    } else {
        retVal = startOffset + string2type<double>(value);
    }
    LOG4FIMEX(logger, Logger::DEBUG, "translateRelativeTime(" << value << "," << startOffset << "," << finalValue << ")=" << retVal);
    return retVal;
}

/**
 * convenience function to convert a comma-separated dotted string of iso-times
 * to a dotted string of doubles, representing time in TimeUnit
 * @param timeString string like YYYY-MM-DD HH:MM:SS,...,YYYY-MM-DD HH:MM:SS
 * @param tu unit to represent the time as doubles
 * @return string of doubles or ..., comma-separated
 */
string timeString2timeUnitString(string timeString, const TimeUnit& tu) {
    vector<string> times = tokenize(timeString, ",");
    for (size_t i = 0; i < times.size(); ++i) {
        if (times[i] != "...") {
            // convert iso-time to double time of time-unit
            LOG4FIMEX(logger, Logger::DEBUG, "converting times["<<i<<"]: " << times[i]);
            times[i] = type2string(tu.fimexTime2unitTime(string2FimexTime(times[i])));
            LOG4FIMEX(logger, Logger::DEBUG, "converted times["<<i<<"] to " << times[i] << " in timeUnit");
        }
    }
    return join(times.begin(), times.end(), ",");
}

TimeSpec::TimeSpec(const string& timeSpec, const FimexTime& startTime, const FimexTime& endTime)
    : outputUnit("seconds since 1970-01-01 00:00:00")
{
    LOG4FIMEX(logger, Logger::DEBUG, "getting timespec of " << timeSpec);
    vector<string> toks = tokenize(timeSpec, ";");
    std::string timeStepStr;
    std::string relativeUnit;
    for (vector<string>::iterator it = toks.begin(); it != toks.end(); ++it) {
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
        // convert all times to double using tu and using tokenizeDotted
        const TimeUnit tu(outputUnit); // all times should be representable in outputUnit
        string timeDoubles = timeString2timeUnitString(timeStepStr, tu);
        vector<double> timeDoubleVec = tokenizeDotted<double>(timeDoubles, ",");
        // add the times as fimexTimes to timeSteps
        transform(timeDoubleVec.begin(), timeDoubleVec.end(), back_inserter(timeSteps), std::bind1st(std::mem_fun_ref(&TimeUnit::unitTime2fimexTime), tu));
    } else {
        // including x
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

        // convert list with x and ... to timeUnitString
        for (size_t i = 0; i < times.size(); i++) {
            if (times[i] != "...") {
                times[i] = type2string(translateRelativeTime(times[i], startOffset, finalTimeU));
            }
        }
        string timeDoubles = join(times.begin(), times.end(), ",");
        vector<double> timeDoubleVec = tokenizeDotted<double>(timeDoubles, ",");
        // add the times as fimexTimes to timeSteps
        transform(timeDoubleVec.begin(), timeDoubleVec.end(), back_inserter(timeSteps),
                std::bind1st(std::mem_fun_ref(&TimeUnit::unitTime2fimexTime), tu));
    }

    LOG4FIMEX(logger, Logger::DEBUG, "got parameters unit:" << outputUnit << ";relativeUnit:" << relativeUnit << ";steps:" << timeStepStr);
}

} // namespace MetNoFimex
