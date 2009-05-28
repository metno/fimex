/*
 * Fimex, CDMQualityExtractor.cc
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
 *  Created on: May 25, 2009
 *      Author: Heiko Klein
 */

#include "fimex/CDMQualityExtractor.h"
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/Data.h"
#include "fimex/XMLDoc.h"
#include "fimex/interpolation.h" // for constants
#include <boost/regex.hpp>
#include <algorithm>

using namespace std;

namespace MetNoFimex
{

static LoggerPtr logger = getLogger("fimex.CDMQualityExtractor");

/**
 * find the variable which is quality-assured by the status-variable
 * @param cdm
 * @param statusVarName the status variable
 * @return the quality assured variables
 */
static vector<string> findCorrespondingVariables(const CDM& cdm, const string& statusVarName) {
    vector<string> correspondingVars;
    // the standard name of a status variable is: "corresponding_standard_name status_flag" as of CF-1.3
    CDMAttribute attr;
    if (cdm.getAttribute(statusVarName, "standard_name", attr)) {
        std::string standardName = attr.getData()->asString();
        vector<string> parts = tokenize(standardName, " ");
        if (parts.size() >=2) {
            string foreignStandardName = parts[0];
            vector<string> correspondingVars = cdm.findVariables("standard_name", foreignStandardName);
        } else {
            LOG4FIMEX(logger, Logger::WARN, "standard_name "+ standardName + "not suitabale for a status_flag in statusVariable "+ statusVarName);
        }
    } else {
        LOG4FIMEX(logger, Logger::INFO, "unable to find standard_name for status variable "+statusVarName + ": autoconfig not possible");
    }
    return correspondingVars;
}

static vector<double> getValidValues(const CDM& cdm, const string& statusVarName, const string& autoConfString, const string& statusAttrName) {
    vector<double> validVals;
    boost::smatch matches;
    boost::regex rgx("values=(.*)");
    if (boost::regex_match(autoConfString, matches, rgx)) {
        validVals = tokenizeDotted<double>(matches[1], ",");
        if (validVals.size() == 0) {
            LOG4FIMEX(logger, Logger::WARN, "could not find values autoConfString: "+autoConfString);
        }
    } else {
        const CDMAttribute& attr = cdm.getAttribute(statusVarName, statusAttrName);
        boost::shared_array<double> validData = attr.getData()->asDouble();
        size_t validSize = attr.getData()->size();
        if (validSize > 0) {
            if (autoConfString == "all") {
                copy(&validData[0], &validData[validSize], back_inserter(validVals));
            } else if (autoConfString == "highest") {
                validVals[0] = validData[validSize-1];
            } else if (autoConfString == "lowest") {
                validVals[0] = validData[0];
            } else {
                LOG4FIMEX(logger, Logger::WARN, "unable to interpret autoConfString: "+autoConfString);
            }
        } else {
            LOG4FIMEX(logger, Logger::WARN, "could not find flag_values in status-variable: "+statusVarName);
        }
    }
    return validVals;
}

CDMQualityExtractor::CDMQualityExtractor(boost::shared_ptr<CDMReader> dataReader, std::string autoConfString, std::string configFile) throw(CDMException)
: dataReader(dataReader)
{
    cdm = dataReader->getCDM();
    const CDM& cdm = dataReader->getCDM();
    if (autoConfString != "") {
        vector<string> vars = cdm.findVariables("flag_values",".*");
        for (vector<string>::iterator varIt = vars.begin(); varIt != vars.end(); ++varIt) {
            vector<string> correspondingVars = findCorrespondingVariables(cdm, *varIt);
            for (vector<string>::iterator cVarIt = correspondingVars.begin(); cVarIt != correspondingVars.end(); ++cVarIt) {
                statusVariable[*cVarIt] = *varIt;
                variableValues[*cVarIt] = getValidValues(cdm, *varIt, autoConfString, "flag_values");
            }
        }
        vars = cdm.findVariables("flag_masks", ".*");
        if (vars.size() > 0) {
            // TODO: implement
            LOG4FIMEX(logger, Logger::INFO, "flag_masks not implemented yet");
        }
    }
    if (configFile != "") {
        XMLDoc doc(configFile);
        XPathObjPtr xpathObj = doc.getXPathObject("/cdmQualityConfig");
        size_t size = xpathObj->nodesetval ? xpathObj->nodesetval->nodeNr : 0;
        if (size != 1) {
            throw CDMException("config-file "+configFile+" does not contain cdmQualityConfig root element");
        }
        // loop over variables
        xpathObj = doc.getXPathObject("/cdmQualityConfig/variable");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        size = nodes ? nodes->nodeNr : 0;
        for (size_t i = 0; i < size; i++) {
            xmlNodePtr node = nodes->nodeTab[i];
            string varName = getXmlProp(node, "name");
            XPathObjPtr statusVarXPath = doc.getXPathObject("status_flag_variable", node);
            int statusVarNr = statusVarXPath->nodesetval ? statusVarXPath->nodesetval->nodeNr : 0;
            string statusVarName;
            string statusVarUse;
            string statusVarValues;
            if (statusVarNr == 1) {
                statusVarName = getXmlProp(statusVarXPath->nodesetval->nodeTab[0], "name");
                XPathObjPtr allowedXPath = doc.getXPathObject("allowed_values", statusVarXPath->nodesetval->nodeTab[0]);
                int allowedSize = allowedXPath->nodesetval ? allowedXPath->nodesetval->nodeNr : 0;
                if (allowedSize == 1) {
                    statusVarUse = getXmlProp(allowedXPath->nodesetval->nodeTab[0], "use");
                    xmlNodePtr valNode = allowedXPath->nodesetval->nodeTab[0]->children;
                    if (valNode && (valNode->type == XML_TEXT_NODE)) {
                        statusVarValues = reinterpret_cast<char *>(valNode->content);
                    }
                }
            }
            if (statusVarName == "") throw CDMException("could not find status_flag_variable for var: " + varName);
            LOG4FIMEX(logger,Logger::DEBUG, "adding (variable,statusVar,use,vals): ("<<varName<<","<<statusVarName<<","<<statusVarUse<<","<<statusVarValues<<")");
            vector<double> statusVarVals = tokenizeDotted<double>(statusVarValues);
            if (statusVarVals.size() > 0) {
                statusVariable[varName] = statusVarName;
                variableValues[varName] = statusVarVals;
            } else if (statusVarUse != "") {
                statusVariable[varName] = statusVarName;
                variableFlags[varName] = statusVarUse;
            } else {
                throw CDMException("unable to quality-assure variable " + varName + ": no use or values given");
            }
        }

    }

}

static double findDefinedExtreme(double* begin, double* end, const double& (*minmax)(const double& a, const double& b)) {
    double extreme = MIFI_UNDEFINED_D;
    double* pos = begin;
    // forward to first defined element
    while ((pos != end) && (!isnan(*pos))) {
        ++pos;
    }
    if (pos == end) {
        extreme = MIFI_UNDEFINED_D;
    } else {
        extreme = *pos;
        while (pos != end) {
            if (!isnan(*pos)) {
                extreme = minmax(extreme, *pos);
            }
        }
    }
    return extreme;
}

const boost::shared_ptr<Data> CDMQualityExtractor::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
    // no change in cdm-data in CDMQualityExtractor, so no need to check for in-memory data

    boost::shared_ptr<Data> data = dataReader->getDataSlice(varName, unLimDimPos);
    // test if variable has quality assignment
    if (statusVariable.find(varName) != statusVariable.end()) {
        string statusVar = statusVariable[varName];
        boost::shared_ptr<Data> statusData = getDataSlice(statusVar, unLimDimPos); // reading statusData after applying QualityExtractor
        if (statusData->size() == 0) {
            statusData = getDataSlice(statusVar, 0); // get the default slice
        }

        if (data->size() == statusData->size()) {
            size_t size = data->size();
            boost::shared_array<double> sd = statusData->asDouble();
            vector<double> useVals;
            if (variableValues.find(varName) != variableValues.end()) {
                useVals = variableValues[varName];
                sort(useVals.begin(), useVals.end());
            } else if (variableFlags.find(varName) != variableFlags.end()) {
                string flag = variableFlags[varName];
                double statusFill = cdm.getFillValue(statusVar);
                double minFlag = MIFI_UNDEFINED_D;
                double maxFlag = MIFI_UNDEFINED_D;
                CDMAttribute attr;
                if (cdm.getAttribute(statusVar, "valid_min", attr)) {
                    minFlag = attr.getData()->asDouble()[0];
                }
                if (cdm.getAttribute(statusVar, "valid_max", attr)) {
                    maxFlag = attr.getData()->asDouble()[0];
                }
                if (cdm.getAttribute(statusVar, "valid_range", attr)) {
                    minFlag = attr.getData()->asDouble()[0];
                    maxFlag = attr.getData()->asDouble()[1];
                }
                if (!isnan(minFlag)) {
                    double* sdIt = &sd[0];
                    while (sdIt != &sd[size]) {
                        if (*sdIt < minFlag) {
                            *sdIt = MIFI_UNDEFINED_D;
                        }
                        sdIt++;
                    }
                }
                if (!isnan(maxFlag)) {
                    double* sdIt = &sd[0];
                    while (sdIt != &sd[size]) {
                        if (*sdIt > maxFlag) {
                            *sdIt = MIFI_UNDEFINED_D;
                        }
                        sdIt++;
                    }
                }
                if (!isnan(statusFill)) {
                    double* sdIt = &sd[0];
                    while (sdIt != &sd[size]) {
                        if (*sdIt == statusFill) {
                            *sdIt = MIFI_UNDEFINED_D;
                        }
                        sdIt++;
                    }
                }
                boost::smatch match;
                if (flag == "all") {
                    // no more to do
                } else if (boost::regex_match(flag, match, boost::regex("max:(.+)"))) {
                    double max = string2type<double>(match[1]);
                    LOG4FIMEX(logger, Logger::DEBUG, "using max="<<max<<" for statusVar "<<statusVar<< " on var "<< varName);
                    double* sdIt = &sd[0];
                    while (sdIt != &sd[size]) {
                        if (*sdIt > max) {
                            *sdIt = MIFI_UNDEFINED_D;
                        }
                        sdIt++;
                    }
                } else if (boost::regex_match(flag, match, boost::regex("min:(.+)"))) {
                    double min = string2type<double>(match[1]);
                    double* sdIt = &sd[0];
                    size_t count = 0;
                    while (sdIt != &sd[size]) {
                        if (*sdIt < min) {
                            *sdIt = MIFI_UNDEFINED_D;
                            ++count;
                        }
                        sdIt++;
                    }
                    LOG4FIMEX(logger, Logger::DEBUG, "using min="<<min<<" for statusVar "<<statusVar<< " on var "<< varName << ": removed points: " << count);
                } else if (flag == "highest") {
                    double testVal = findDefinedExtreme(&sd[0], &sd[size], &max<double>);
                    if (!isnan(testVal)) useVals.push_back(testVal);
                } else if (flag == "lowest") {
                    double testVal = findDefinedExtreme(&sd[0], &sd[size], &min<double>);
                    if (!isnan(testVal)) useVals.push_back(testVal);
                } else {
                    throw CDMException("undefined quality-flag: "+flag+" for variable: "+varName);
                }
            }
            if (useVals.size() > 0) {
                // useVals are externally given flag-values or internally derived min/max
                double* sdIt = &sd[0];
                while (sdIt != &sd[size]) {
                    if (!binary_search(useVals.begin(), useVals.end(), *sdIt)) {
                        *sdIt = MIFI_UNDEFINED_D;
                    }
                    sdIt++;
                }
            }
            double fillValue = cdm.getFillValue(varName);
            double *sdIt = &sd[0];
            for (size_t i = 0; i < size; ++i) {
                if (isnan(*sdIt)) {
                    data->setValue(i, fillValue);
                }
                ++sdIt;
            }
        } else {
            LOG4FIMEX(logger, Logger::WARN, "different size in data of variable and statusVariable at slice "<< unLimDimPos << ": "<<varName << ","<<statusVar<<": "<< data->size() << "<>" << statusData->size());
        }
    }
    return data;
}

}
