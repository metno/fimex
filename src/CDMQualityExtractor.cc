/*
 * Fimex, CDMQualityExtractor.cc
 *
 * (C) Copyright 2009-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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
#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/String2Type.h"
#include "fimex/TokenizeDotted.h"
#include "fimex/XMLDoc.h"
#include "fimex/XMLUtils.h"
#include "fimex/mifi_constants.h"

#include <algorithm>
#include <map>
#include <regex>
#include <set>
#include <tuple>

using namespace std;

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.CDMQualityExtractor");

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

static inline std::set<double> into_set(std::vector<double> vec)
{
    return std::set<double>(vec.begin(), vec.end());
}

static std::set<double> getValidValues(const CDM& cdm, const string& statusVarName, const string& autoConfString, const string& statusAttrName)
{
    std::set<double> validVals;
    std::smatch matches;
    std::regex rgx("values=(.*)");
    if (std::regex_match(autoConfString, matches, rgx)) {
        validVals = into_set(tokenizeDotted<double>(matches[1], ","));
        if (validVals.size() == 0) {
            LOG4FIMEX(logger, Logger::WARN, "could not find values autoConfString: "+autoConfString);
        }
    } else {
        const CDMAttribute& attr = cdm.getAttribute(statusVarName, statusAttrName);
        auto validData = attr.getData()->asDouble();
        size_t validSize = attr.getData()->size();
        if (validSize > 0) {
            if (autoConfString == "all") {
                validVals.insert(&validData[0], &validData[validSize]);
            } else if (autoConfString == "highest") {
                validVals.insert(validData[validSize - 1]);
            } else if (autoConfString == "lowest") {
                validVals.insert(validData[0]);
            } else {
                LOG4FIMEX(logger, Logger::WARN, "unable to interpret autoConfString: "+autoConfString);
            }
        } else {
            LOG4FIMEX(logger, Logger::WARN, "could not find flag_values in status-variable: "+statusVarName);
        }
    }
    return validVals;
}

static const std::string ALLOWED_ALL = "all";
static const std::string ALLOWED_HIGHEST = "highest";
static const std::string ALLOWED_LOWEST = "lowest";

using QualityMap = std::map<std::string, std::shared_ptr<const CDMQualityExtractor::QualityAssignment>>;

static void addAutoConfigQualities(const CDM& cdm, const string& autoConfString, QualityMap& qualities)
{
    // precheck allowed autoConfStrings, in case no variable is found, but string
    // is wrong
    if (autoConfString != ALLOWED_ALL && autoConfString != ALLOWED_HIGHEST && autoConfString != ALLOWED_LOWEST) {
        throw CDMException("auto-configure-string '" + autoConfString + "' unknown");
    }

    const auto vars = cdm.findVariables("flag_values", ".*");
    for (const auto& var : vars) {
        const auto correspondingVars = findCorrespondingVariables(cdm, var);
        if (correspondingVars.empty()) {
            continue;
        }
        auto assignment = std::make_shared<CDMQualityExtractor::QualityAssignment>();
        assignment->statusVariable = var;
        assignment->values = getValidValues(cdm, var, autoConfString, "flag_values");
        for (const auto& cvar : correspondingVars) {
            qualities[cvar] = assignment;
        }
    }

    const auto flag_mask_vars = cdm.findVariables("flag_masks", ".*");
    if (!flag_mask_vars.empty()) {
        // TODO: implement
        LOG4FIMEX(logger, Logger::INFO, "flag_masks not implemented yet");
    }
}

/**
 * Find all variables whose name fully matches one of the regular expression in namePattern.
 */
static std::set<string> findNamedVariables(const CDM& cdm, const string& namePattern)
{
    std::regex rgx;
    try {
        rgx = std::regex(namePattern);
    } catch (const std::regex_error& e) {
        throw CDMException("invalid variable regular expression '" + namePattern + "': " + e.what());
    }

    std::set<string> found;
    for (const auto& var : cdm.getVariables()) {
        if (std::regex_match(var.getName(), rgx)) {
            found.insert(var.getName());
        }
    }

    return found;
}

/**
 * Check that the shape of the status variable is compatible with the shape of the variable.
 * @return true if compatible, otherwise false and the reason in reason
 */
static bool checkShapeCompatibility(const CDM& cdm, const string& varName, const CDMReader_p& statusReader,
                                    const string& statusVarName, string& reason)
{
    // TODO improve shape compatibility check
    LOG4FIMEX(logger,Logger::DEBUG, "getting shape for variable '" << varName << "'");
    const vector<string> shapeVar = cdm.getVariable(varName).getShape();
    LOG4FIMEX(logger,Logger::DEBUG, "getting shape for status variable '" << statusVarName << "'");
    vector<string> shapeStatus;
    if (statusReader->getCDM().hasVariable(statusVarName)) {
        shapeStatus = statusReader->getCDM().getVariable(statusVarName).getShape();
    }
    if (shapeVar == shapeStatus) {
        return true;
    }
    if (shapeVar.size() < shapeStatus.size()) {
        reason = "external status variable '" + statusVarName + "' has more dimensions than variable '" + varName + "'";
        return false;
    }
    for (size_t i=0; i<shapeStatus.size(); ++i) {
        if (cdm.getDimension(shapeVar[i]).getLength() != statusReader->getCDM().getDimension(shapeStatus[i]).getLength()) {
            reason = "external status variable '" + statusVarName + "' and variable '" + varName
                   + "' have different shapes, quality extract not implemented";
            return false;
        }
    }
    LOG4FIMEX(logger,Logger::INFO, "shapes of variable '" << varName << "' and statusVar '" << statusVarName
              << "' are different but hopefully compatible");
    return true;
}

static void addConfigFileQualities(CDMReader_p dataReader, const string& configFile, QualityMap& qualities)
{
    const CDM& cdm = dataReader->getCDM();

    // one CDMReader per (file, type, config) combination
    std::map<std::tuple<string, string, string>, CDMReader_p> statusReaderCache;

    XMLDoc_p doc = std::make_shared<XMLDoc>(configFile);
    XPathNodeSet nodesQC(doc, "/cdmQualityConfig");
    if (nodesQC.size() != 1) {
        throw CDMException("config-file " + configFile + " does not contain cdmQualityConfig root element");
    }

    // loop over variables
    XPathNodeSet variables(doc, "/cdmQualityConfig/variable");
    for (auto node : variables) {
        string varName = getXmlProp(node, "name");
        string fillValStr = getXmlProp(node, "fillValue");
        XPathNodeSet statusVar(doc, "status_flag_variable", node);
        string statusVarName;
        string statusVarUse;
        string statusVarValues;
        string statusVarFile, statusVarType, statusVarConfig;
        if (statusVar.size() == 1) {
            auto statusNode = statusVar[0];
            statusVarName = getXmlProp(statusNode, "name");
            XPathNodeSet allowed(doc, "allowed_values", statusNode);
            if (allowed.size() == 1) {
                statusVarUse = getXmlProp(allowed[0], "use");
                auto valNode = allowed[0]->children;
                if (valNode && (valNode->type == XML_TEXT_NODE)) {
                    statusVarValues = reinterpret_cast<char*>(valNode->content);
                }
            }
            statusVarFile = getXmlProp(statusNode, "file");
            statusVarType = getXmlProp(statusNode, "type");
            statusVarConfig = getXmlProp(statusNode, "config");
        }
        if (statusVarName.empty()) {
            throw CDMException("could not find status_flag_variable for var: " + varName);
        }
        if (statusVarFile.empty() && (!statusVarConfig.empty() || !statusVarType.empty())) {
            throw CDMException("could not find status_flag_variable has type/config but no filename: " + varName);
        }
        LOG4FIMEX(logger, Logger::DEBUG,
                  "adding (variable,statusVar,use,vals): (" << varName << "," << statusVarName << "," << statusVarUse << "," << statusVarValues << ")");
        CDMReader_p statusReader = dataReader;
        CDMReader_p externalReader;
        if (!statusVarFile.empty()) {
            // status var from a different reader
            const auto key = std::make_tuple(statusVarFile, statusVarType, statusVarConfig);
            auto cached = statusReaderCache.find(key);
            if (cached == statusReaderCache.end()) {
                externalReader = CDMFileReaderFactory::create(statusVarType, statusVarFile, statusVarConfig);
                statusReaderCache[key] = externalReader;
            } else {
                externalReader = cached->second;
            }
            statusReader = externalReader;
        }

        std::optional<double> fillValue;
        if (!fillValStr.empty()) {
            fillValue = string2type<double>(fillValStr);
        }
        std::optional<std::set<double>> values;
        string flags;
        const auto statusVarVals = into_set(tokenizeDotted<double>(statusVarValues));
        if (!statusVarVals.empty()) {
            values = statusVarVals;
        } else if (!statusVarUse.empty()) {
            flags = statusVarUse;
        } else {
            throw CDMException("unable to quality-assure variable " + varName + ": no use or values given");
        }

        const auto matches = findNamedVariables(cdm, varName);
        if (matches.empty()) {
            LOG4FIMEX(logger, Logger::WARN, "no variable matches '" << varName << "' in quality config, skipping");
            continue;
        }
        // assignment shared by all matches without an existing entry
        auto shared = std::make_shared<CDMQualityExtractor::QualityAssignment>();
        shared->statusVariable = statusVarName;
        shared->statusReader = externalReader;
        shared->fillValue = fillValue;
        if (values) {
            shared->values = values;
        } else {
            shared->flags = flags;
        }

        for (const auto& name : matches) {
            string reason;
            if (!checkShapeCompatibility(cdm, name, statusReader, statusVarName, reason)) {
                if (matches.size() == 1) {
                    throw CDMException(reason);
                }
                // coordinate variables (1-dim, named after their dimension) are expected to be skipped silently
                const auto shape = cdm.getVariable(name).getShape();
                const bool isCoordinate = shape.size() == 1 && shape[0] == name;
                if (!shape.empty() && !isCoordinate) {
                    LOG4FIMEX(logger, Logger::WARN, "skipping variable '" << name << "' for '" << varName << "': " << reason);
                }
                continue;
            }

            if (const auto existing = qualities.find(name); existing != qualities.end()) {
                // copy-on-write: merge with the existing entry, leaving the shared object untouched
                auto q = std::make_shared<CDMQualityExtractor::QualityAssignment>(*existing->second);
                q->statusVariable = shared->statusVariable;
                if (shared->statusReader) {
                    q->statusReader = shared->statusReader;
                }
                q->fillValue = shared->fillValue;
                if (shared->values) {
                    q->values = shared->values;
                } else {
                    q->flags = shared->flags;
                }
                qualities[name] = q;
            } else {
                qualities[name] = shared;
            }
        }
    }
}

CDMQualityExtractor::CDMQualityExtractor(CDMReader_p dataReader, std::string autoConfString, std::string configFile)
    : dataReader(dataReader)
{
    *cdm_.get() = dataReader->getCDM();

    if (!autoConfString.empty()) {
        addAutoConfigQualities(dataReader->getCDM(), autoConfString, qualities);
    }
    if (!configFile.empty()) {
        addConfigFileQualities(dataReader, configFile, qualities);
    }
}

std::shared_ptr<const CDMQualityExtractor::QualityAssignment> CDMQualityExtractor::getQualityAssignment(const std::string& varName)
{
    if (const auto it = qualities.find(varName); it != qualities.end()) {
        return it->second;
    } else {
        return {};
    }
}

static double findDefinedExtreme(double* begin, double* end, const double& (*minmax)(const double& a, const double& b))
{
    double extreme = MIFI_UNDEFINED_D;
    double* pos = begin;
    // forward to first defined element
    while ((pos != end) && (!std::isnan(*pos))) {
        ++pos;
    }
    if (pos == end) {
        extreme = MIFI_UNDEFINED_D;
    } else {
        extreme = *pos;
        while (pos != end) {
            if (!std::isnan(*pos)) {
                extreme = minmax(extreme, *pos);
            }
        }
    }
    return extreme;
}

DataPtr CDMQualityExtractor::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    // no change in cdm-data in CDMQualityExtractor, so no need to check for in-memory data

    DataPtr data = dataReader->getDataSlice(varName, unLimDimPos);
    // test if variable has quality assignment
    const auto q_it = qualities.find(varName);
    if (q_it != qualities.end()) {
        const auto& q = *q_it->second;
        const auto& statusVar = q.statusVariable;

        DataPtr statusData;
        CDMReader_p statusReader = q.statusReader ? q.statusReader : dataReader;

        if (statusVar == varName && statusReader == dataReader) {
            // reuse data in case of own status data
            statusData = data;
        } else {
            statusData = statusReader->getDataSlice(statusVar, unLimDimPos);
            if (statusData->size() == 0) {
                statusData = statusReader->getDataSlice(statusVar, 0); // get the default slice
            }
        }

        const auto& cdmS = statusReader->getCDM();

        const double fillValue = q.fillValue ? *q.fillValue : cdm_->getFillValue(varName);
        const size_t sizeD = data->size(), sizeS = statusData->size();
        if (sizeD == 0 && sizeS == 0) {
            // special case: only undefined data
            const auto& var = cdm_->getVariable(varName);
            const auto& shape = var.getShape();
            size_t length = 1;
            size_t sliceDims = cdm_->hasUnlimitedDim(var) ? shape.size()-1 : shape.size();
            for (size_t i = 0; i < sliceDims; ++i) {
                length *= cdm_->getDimension(shape.at(i)).getLength();
            }

            // return undefined data with new fill-value
            return createData(var.getDataType(), length, fillValue);
        }

        const double sizeRatio = double(sizeD)/sizeS;
        if (sizeRatio == int(sizeRatio) && sizeRatio >= 1) {
            auto sd = statusData->asDouble();
            std::set<double> useVals;
            if (q.values) {
                useVals = *q.values;
            } else if (!q.flags.empty()) {
                const auto& flag = q.flags;
                const double statusFill = cdmS.getFillValue(statusVar);
                double minFlag = MIFI_UNDEFINED_D;
                double maxFlag = MIFI_UNDEFINED_D;
                CDMAttribute attr;
                if (cdmS.getAttribute(statusVar, "valid_min", attr)) {
                    minFlag = attr.getData()->asDouble()[0];
                }
                if (cdmS.getAttribute(statusVar, "valid_max", attr)) {
                    maxFlag = attr.getData()->asDouble()[0];
                }
                if (cdmS.getAttribute(statusVar, "valid_range", attr)) {
                    minFlag = attr.getData()->asDouble()[0];
                    maxFlag = attr.getData()->asDouble()[1];
                }
                if (!std::isnan(minFlag)) {
                    double* sdIt = &sd[0];
                    while (sdIt != &sd[sizeS]) {
                        if (*sdIt < minFlag) {
                            *sdIt = MIFI_UNDEFINED_D;
                        }
                        sdIt++;
                    }
                }
                if (!std::isnan(maxFlag)) {
                    double* sdIt = &sd[0];
                    while (sdIt != &sd[sizeS]) {
                        if (*sdIt > maxFlag) {
                            *sdIt = MIFI_UNDEFINED_D;
                        }
                        sdIt++;
                    }
                }
                if (!std::isnan(statusFill)) {
                    double* sdIt = &sd[0];
                    while (sdIt != &sd[sizeS]) {
                        if (*sdIt == statusFill) {
                            *sdIt = MIFI_UNDEFINED_D;
                        }
                        sdIt++;
                    }
                }
                std::smatch match;
                if (flag == ALLOWED_ALL) {
                    // no more to do
                } else if (std::regex_match(flag, match, std::regex("max:(.+)"))) {
                    double max = string2type<double>(match[1]);
                    LOG4FIMEX(logger, Logger::DEBUG, "using max="<<max<<" for statusVar "<<statusVar<< " on var "<< varName);
                    double* sdIt = &sd[0];
                    while (sdIt != &sd[sizeS]) {
                        if (*sdIt > max) {
                            *sdIt = MIFI_UNDEFINED_D;
                        }
                        sdIt++;
                    }
                } else if (std::regex_match(flag, match, std::regex("min:(.+)"))) {
                    double min = string2type<double>(match[1]);
                    double* sdIt = &sd[0];
                    size_t count = 0;
                    while (sdIt != &sd[sizeS]) {
                        if (*sdIt < min) {
                            *sdIt = MIFI_UNDEFINED_D;
                            ++count;
                        }
                        sdIt++;
                    }
                    LOG4FIMEX(logger, Logger::DEBUG, "using min="<<min<<" for statusVar "<<statusVar<< " on var "<< varName << ": removed points: " << count);
                } else if (flag == ALLOWED_HIGHEST) {
                    double testVal = findDefinedExtreme(&sd[0], &sd[sizeS], &max<double>);
                    if (!std::isnan(testVal))
                        useVals.insert(testVal);
                } else if (flag == ALLOWED_LOWEST) {
                    double testVal = findDefinedExtreme(&sd[0], &sd[sizeS], &min<double>);
                    if (!std::isnan(testVal))
                        useVals.insert(testVal);
                } else {
                    throw CDMException("undefined quality-flag: "+flag+" for variable: "+varName);
                }
            }
            if (!useVals.empty()) {
                // useVals are externally given flag-values or internally derived min/max
                double* sdIt = &sd[0];
                while (sdIt != &sd[sizeS]) {
                    if (useVals.find(*sdIt) == useVals.end()) {
                        *sdIt = MIFI_UNDEFINED_D;
                    }
                    sdIt++;
                }
            }
            for(size_t iD = 0; iD < sizeD; ) {
                double *sdIt = &sd[0];
                for (size_t iS = 0; iS < sizeS; ++iS, ++iD) {
                    if (std::isnan(*sdIt)) {
                        data->setValue(iD, fillValue);
                    }
                    ++sdIt;
                }
            }
        } else {
            LOG4FIMEX(logger, Logger::WARN, "incompatible size in data of variable and statusVariable at slice "<< unLimDimPos << ": "<<varName << ","<<statusVar<<": "<< data->size() << "<>" << statusData->size());
        }
    }
    return data;
}

}
