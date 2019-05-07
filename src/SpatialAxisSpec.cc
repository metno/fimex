/*
 * Fimex, TimeSpec.cc
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
 *  Created on: Mar 18, 2009
 *      Author: Heiko Klein
 */

#include "fimex/SpatialAxisSpec.h"
#include "fimex/Logger.h"
#include "fimex/Units.h"
#include "fimex/Utils.h"

#include <algorithm>
#include <iterator>
#include <regex>
#include <vector>

namespace MetNoFimex
{

using namespace std;

static Logger_p logger = getLogger("fimex.SpatialAxisSpec");

class TranslateRelativePlace
{
    double startOffset;
    double finalValue;
public:
    TranslateRelativePlace(double startOffset, double finalValue) :
        startOffset(startOffset), finalValue(finalValue)
    {
    }

    std::string operator()(string value)
    {
        std::string retVal;
        if (value != "...") {
            std::smatch what;
            std::regex finalVals("x\\s*([+-])?\\s*(\\d\\.?\\d*)?\\s*");
            double retDouble;
            if (std::regex_search(value, what, finalVals)) {
                if (what.size() >= 3) {
                    if (what[1] == '+') {
                        retDouble = finalValue + string2type<double> (what[2]);
                    } else if (what[1] == '-'){
                        retDouble = finalValue - string2type<double> (what[2]);
                    } else {
                        retDouble = finalValue;
                    }
                } else {
                    retDouble = finalValue;
                }
            } else {
                retDouble = startOffset + string2type<double> (value);
            }
            retVal = type2string(retDouble);
        } else {
            retVal = "...";
        }
        LOG4FIMEX(logger, Logger::DEBUG, "translateRelativeAxis(" << value << "," << startOffset << ","<<finalValue << ")="<<retVal)
        return retVal;
    }
};

void SpatialAxisSpec::init()
{
    LOG4FIMEX(logger, Logger::DEBUG, "getting axisspec of " << axisSpec);
    if (requireStartEnd()) throw CDMException("require start and end for axisSpec: " + axisSpec);
    vector<string> toks = tokenize(axisSpec, ";");
    std::string axisStepStr;
    std::string relativeStart;
    std::string inputUnit = "m";
    for (vector<string>::iterator it = toks.begin(); it != toks.end(); ++it) {
        vector<string> extraParam = tokenize(*it, "=");
        if (extraParam.size() == 1) {
            if (axisStepStr == "") {
                axisStepStr = extraParam[0];
            } else {
                throw CDMException("axis-steps redefined from " + axisStepStr
                        + " to " + extraParam[0]);
            }
        } else if (extraParam.size() == 2) {
            if (extraParam[0] == "unit") {
                // TODO: accept compatible units
                throw CDMException("unit not supported yet in SpatialAxisSpec, please enter values in m or degree");
                inputUnit = extraParam[1];
            } else if (extraParam[0] == "relativeStart") {
                relativeStart = extraParam[1];
            } else {
                throw CDMException("unknown axisSpec parameter: '"
                        + extraParam[0] + "'");
            }
        } else {
            throw CDMException("unknown axisSpec section: '" + *it + "'");
        }
    }

    if (relativeStart == "") {
        // absolute time in spec
        axisSteps = tokenizeDotted<double> (axisStepStr, ",");
        // TODO: convert unit to m/degree
    } else {
        // relative axis
        vector<string> places = tokenize(axisStepStr, ",");
        if (places.size() < 2) {
            throw CDMException(
                    "SpatialAxisSpec requires at least 2 values with relative start definition, got: "
                            + axisStepStr);
        }
        double delta = string2type<double> (places[1]) - string2type<double>(places[0]);
        Units units;
        // find the position 0 in the relative times
        double startU = start; // TODO check unit
        double startOffset = static_cast<int> (startU / delta) * delta;
        if (startU < startOffset) {
            // cast to largest value below startU
            // but I want smallest value above startU
            startOffset += delta;
        }
        // find position x in the relative times
        double endU = end; // TODO check unit
        double finalU = static_cast<int> (endU / delta) * delta;
        transform(places.begin(), places.end(), places.begin(),
                TranslateRelativePlace(startOffset, finalU));
        string absolutPlaces = join(places.begin(), places.end(), ",");
        axisSteps = tokenizeDotted<double>(absolutPlaces, ",");
        // TODO: check unit
    }
    axisInitialized = true;
    LOG4FIMEX(logger, Logger::DEBUG, "got parameters unit:" << inputUnit << ";relativeStart:" << relativeStart << ";steps:"<<axisStepStr);
}

bool SpatialAxisSpec::requireStartEnd()
{
    if (startEndInitialized) return false;
    if (axisSpec.find("relativeStart") == string::npos) return false;
    return true;
}

} /* MetNoFimex */
