/*
 * Fimex, GribCDMIndexer.cc
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
 *  Created on: Sep 9, 2009
 *      Author: Heiko Klein
 */

#include "GribCDMIndexer.h"
#include "GribFileIndex.h"
#include "GribReaderConfig.h"

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/CDMUtils.h"
#include "fimex/CDM_XMLConfigHelper.h"
#include "fimex/Data.h"
#include "fimex/GridDefinition.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/ReplaceStringObject.h"
#include "fimex/ReplaceStringTemplateObject.h"
#include "fimex/ReplaceStringTimeObject.h"
#include "fimex/String2Type.h"
#include "fimex/StringUtils.h"
#include "fimex/TimeUnit.h"
#include "fimex/TimeUtils.h"
#include "fimex/Type2String.h"
#include "fimex/coordSys/Projection.h"

#include "LogTimer.h"

#include "fimex_grib_config.h"

#include <cassert>
#include <functional>
#include <limits>
#include <map>
#include <numeric>
#include <set>
#include <stdexcept>

namespace MetNoFimex {

namespace { // anonymous

Logger_p logger = getLogger("fimex.GribCDMIndexer");

struct ProjectionInfo
{
    std::string xDim;
    std::string yDim;
    std::string coordinates;
    std::string gridMapping;
};

GribCDMIndexer::grib_index intoIndex(StringListBuilder& grib_files, const GribFileMessage& gfm)
{
    return {grib_files.add(gfm.getFileURL()), static_cast<size_t>(gfm.getFilePosition()), gfm.getMsgSize()};
}

typedef std::pair<long, long> leveltype_t;

typedef std::vector<long> levelvalues_t;

struct levelvals_t
{
    std::string dimname;
    std::vector<double> levelPvData;
    levelvalues_t values;
};

// it is somewhat unfortunate that file_index == 0 means "invalid" as this is fragile
const GribCDMIndexer::grib_index invalid_msg{0, 0, 0};
} // namespace

struct GribCDMIndexer::Init
{
    ChunkReaderFactory_p ca;

    GribReader::GribConfig_cp config;

    int lastXYId;
    std::map<GridDefinition, ProjectionInfo> gridProjection;

    size_t maxEnsembles;

    std::string timeDimName;
    std::string ensembleDimName;

    StringListBuilder grib_files;

    // varName -> has Ensemble
    struct var_has
    {
        bool has_ensemble;
        bool has_time;
        GridDefinition gridDef;
        CDMDataType datatype;
        std::string vectorDirection;
        std::string vectorCounterpart;
        std::vector<CDMAttribute> attributes;
        GribReader::Precision precision;

        // edition_levelType
        leveltype_t level_type;
        // position in levelValsOfType
        size_t level_pos;

        std::set<long> level_numbers;
        std::vector<double> levelPvData; // pv from first GRIB message for this variable
    };
    std::map<std::string, var_has> varHas;

    // list of different vectors per edition _ typeOfLevel
    std::map<leveltype_t, std::vector<levelvals_t>> levelValsOfType;

    /**
     * config attributes may contain template parameters marked with %PARAM%
     * which should be replaced by dynamic values from the grib-file and stored
     * temporary in this map
     *
     * Currently implemented parameters are: %MIN_DATETIME%, %MAX_DATETIME%: earliest and latest time in felt-file as ISO string
     */
    std::map<std::string, std::shared_ptr<ReplaceStringObject>> templateReplacementAttributes;

    // union of all times seen in grib messages
    std::set<uint64_t> times;

    FimexTime referenceTime;

    CDM cdm;

    // varName -> time (encoded val) -> level (val) -> ensemble (pert.num. val) -> GFI (filename / start / size)
    std::map<std::string, std::map<uint64_t, std::map<long, std::map<size_t, grib_index>>>> varTimeLevelEnsembleGFIBox;

    Init();
};

// static
std::vector<std::pair<std::string, std::regex>> GribCDMIndexer::makeMembersRegex(const std::vector<std::pair<std::string, std::string>>& memberStrings)
{
    std::vector<std::pair<std::string, std::regex>> members;
    for (const auto& m : memberStrings) {
        members.push_back(std::make_pair(m.first, std::regex(m.second)));
    }
    return members;
}

GribCDMIndexer::GribCDMIndexer(const XMLInput& configXML, const std::vector<std::pair<std::string, std::string>>& members, ChunkReaderFactory_p ca)
    : GribCDMIndexer(configXML, makeMembersRegex(members), ca)
{
}

GribCDMIndexer::GribCDMIndexer(const XMLInput& configXML, const std::vector<std::pair<std::string, std::regex>>& members, ChunkReaderFactory_p ca)
    : init_(std::make_shared<Init>())
{
    init_->config = GribReader::configFromXML(configXML, members);

    init_->ca = ca;
}

GribCDMIndexer::Init::Init()
    : grib_files(true)
    , maxEnsembles(0)
    , lastXYId(-1)
{
}

void GribCDMIndexer::load(const std::vector<std::string>& fileNames)
{
    std::map<std::string, std::string> options;
    if (!init_->config->replaceEarthString.empty()) {
        options["earthfigure"] = init_->config->replaceEarthString;
    }
    options["extraKeys"] = init_->config->extraKeys;

    for (const std::string& fn : fileNames) {
        const auto messages = GribFileIndex(init_->ca, fn, init_->config->ensembleMemberIds, options).listMessages();
        for (const auto& gfm : messages) {
            loadOneMessage(gfm);
        }
    }
}

void GribCDMIndexer::load(const std::string& grbmlFileName)
{
    LOGTIMER_MSG("grbml");
    const auto messages = GribFileIndex(init_->ca, grbmlFileName).listMessages();
    for (const auto& gfm : messages) {
        loadOneMessage(gfm);
    }
}

void GribCDMIndexer::build(std::shared_ptr<CDM> cdm, std::shared_ptr<grib_indexed> gi)
{
    LOGTIMER;

    *cdm = init_->cdm; // some projection variables are added on load

    buildLevels();

    // time-dimension needs to be added before global attributes due to replacements
    initAddTimeDimension(cdm, gi);
    // fill templateReplacementAttributes: MIN_DATETIME, MAX_DATETIME
    if (!init_->times.empty()) {
        const auto t_max = fimexTime2epochTime(FimexTime::fromEncoded(*init_->times.begin()));
        const auto t_min = fimexTime2epochTime(FimexTime::fromEncoded(*init_->times.rbegin()));

        init_->templateReplacementAttributes["MIN_DATETIME"] = std::make_shared<ReplaceStringTimeObject>(t_max);
        init_->templateReplacementAttributes["MAX_DATETIME"] = std::make_shared<ReplaceStringTimeObject>(t_min);
    }
    initUpdateTemplatedAttributeValues();

    initAddGlobalAttributes(cdm);
    initAddEnsembles(cdm, gi);
    initLevels(cdm);
    initAddVariables(cdm, gi);

    initCreateVarMessages(cdm, gi);
}

void GribCDMIndexer::initUpdateTemplatedAttributeValues()
{
    for (const auto& vc : init_->config->variable_configs) {
        for (auto& att : vc->attributes) {
            if (att.getDataType() == CDM_STRING) {
                const auto val_old = att.getStringValue();
                const auto val_new = replaceTemplateAttribute(val_old, init_->templateReplacementAttributes);
                if (val_old != val_new) {
                    att.setData(createData(val_new));
                }
            }
        }
    }
}

// #define LOG_IFV 1

void GribCDMIndexer::initAddGlobalAttributes(std::shared_ptr<CDM> cdm)
{
    const std::string xpathString("/gr:cdmGribReaderConfig/gr:global_attributes");
    XPathNodeSet nodes(init_->config->doc, xpathString);
    if (nodes.size() != 1) {
        throw CDMException("unable to find " + xpathString + " in config: " + init_->config->configId);
    }
    for (auto node : nodes) {
        assert(node->type == XML_ELEMENT_NODE);
        std::vector<CDMAttribute> globAttributes;
        fillAttributeListFromXMLNode(globAttributes, node->children, init_->templateReplacementAttributes);
        addAttributes(*cdm, cdm->globalAttributeNS(), globAttributes);
    }
    if (!cdm->checkVariableAttribute(cdm->globalAttributeNS(), "history", std::regex(".*"))) {
        const std::string now = make_time_string_extended(make_time_utc_now());
        cdm->addAttribute(cdm->globalAttributeNS(), CDMAttribute("history", now + " creation by fimex"));
    }
}

/// add the levelOfType for grib-edition edition to the levelDimsOfType, and the levels to the CDM
void GribCDMIndexer::initLevels(std::shared_ptr<CDM> cdm)
{
    for (auto& lit : init_->levelValsOfType) {
        const long edition = lit.first.first;
        const long typeId = lit.first.second;
        const std::string xpathLevelString("/gr:cdmGribReaderConfig/gr:axes/gr:vertical_axis[@grib" + type2string(edition) + "_id='" + type2string(typeId) +
                                           "']");
        XPathNodeSet nodes(init_->config->doc, xpathLevelString);
        if (nodes.size() > 1) {
            throw CDMException("more than one 'vertical'-axis " + type2string(typeId) + " in config: " + init_->config->configId +
                               " and xpath: " + xpathLevelString);
        }

        std::string levelId;
        CDMDataType levelDataType;
        xmlNodePtr node = 0;
        if (nodes.size() == 0) {
            LOG4FIMEX(logger, Logger::INFO, "no definition for vertical axis " + type2string(typeId) + " found, using default");
            levelId = "grib" + type2string(edition) + "_vLevel" + type2string(typeId);
            levelDataType = CDM_FLOAT;
        } else {
            node = nodes[0];
            assert(node->type == XML_ELEMENT_NODE);
            levelId = getXmlProp(node, "id");
            levelDataType = string2datatype(getXmlProp(node, "type"));
        }

        auto& levelvals = lit.second;

        int counter = -1;
        for (auto& lvs : levelvals) {
            counter += 1;
            std::string extension = (levelvals.size() > 1 ? type2string(counter) : "");
            const auto levelName = findUniqueDimName(*cdm, levelId + extension);
            extension = levelName.substr(levelId.size());

            lvs.dimname = levelName;

            LOG4FIMEX(logger, Logger::DEBUG, "declaring level: " << levelName);
            CDMDimension levelDim(levelName, lvs.values.size());
            cdm->addDimension(levelDim);

            // create level variable
            CDMVariable levelVar(levelName, levelDataType, {levelName});
            cdm->addVariable(levelVar);

            // add level variable data
            DataPtr levelData = createData(levelDataType, lvs.values.begin(), lvs.values.end());
            // add special data for hybrid levels
            if (node != 0) {
                initSpecialLevels_(cdm, node, extension, lit.first, counter, levelVar.getShape(), levelData);
            }
            cdm->getVariable(levelName).setData(levelData);

            // add attributes
            std::vector<CDMAttribute> levelAttributes;
            if (node != 0) {
                std::map<std::string, std::shared_ptr<ReplaceStringObject>> replacements;
                replacements["EXT"] = std::make_shared<ReplaceStringTemplateObject<std::string>>(extension);
                fillAttributeListFromXMLNode(levelAttributes, nodes[0]->children, replacements);

                // add special attributes for grib1 / grib2
                {
                    const std::string xpathGribLevelString("gr:grib" + type2string(edition));
                    XPathNodeSet gribNodes(init_->config->doc, xpathGribLevelString, nodes[0]);
                    if (gribNodes.size() == 1) {
                        fillAttributeListFromXMLNode(levelAttributes, gribNodes[0]->children, init_->templateReplacementAttributes);
                    }
                }
            } else {
                levelAttributes.push_back(CDMAttribute("units", "1"));
                levelAttributes.push_back(CDMAttribute("long_name", "grib" + type2string(edition) + "-level " + type2string(typeId)));
            }

            // add the attributes to the CDM
            addAttributes(*cdm, levelName, levelAttributes);
        }
    }
}

std::vector<double> GribCDMIndexer::readValuesFromXPath_(std::shared_ptr<CDM> cdm, xmlNodePtr node, DataPtr levelData, const std::vector<double>& pv,
                                                         const std::string& extension)
{
    const XPathNodeSet nodes(init_->config->doc, "./gr:values", node);
    if (nodes.size() == 0) {
        auto d = levelData->asDouble();
        return std::vector<double>(&d[0], &d[0] + levelData->size());
    }

    std::vector<double> retValues;
    for (auto node : nodes) {
        if (node->type != XML_ELEMENT_NODE) {
            continue;
        }

        const auto mode = getXmlProp(node, "mode");
        if (mode.empty() || mode == "inline") {
            // add all space delimited values to the retVal vector
            const auto values = XmlCharPtr(xmlNodeGetContent(node)).to_string();
            const auto tokens = tokenize(values, " ");
            retValues.reserve(tokens.size());
            transform(tokens.begin(), tokens.end(), back_inserter(retValues), string2type<double>);
        } else if (mode == "extraLevel1" || mode == "extraLevel2") {
            size_t offset = (mode == "extraLevel1") ? 0 : pv.size() / 2;
            auto lv = levelData->asUInt();
            for (size_t i = 0; i < levelData->size(); i++) {
                size_t pos = offset + lv[i];
                if (pos < pv.size()) {
                    // using pos-1 due to fortran numbering in grib
                    double value = (pos == 0) ? pv.at(0) : pv[pos - 1];
                    retValues.push_back(value);
                } else {
                    throw CDMException("levelData (pv) has not enough elements, need " + type2string(pos + 1));
                }
            }
        } else if (mode == "extraHalvLevel1" || mode == "extraHalvLevel2") {
            auto lv = levelData->asUInt();
            if (pv.size() / 2 < levelData->size()) {
                throw CDMException("probably an asimof header, not supported");
            }
            // NOTE: In the asimof header, the values are a1,b1,a2,b2...an,bn
            size_t offset;
            offset = (mode == "extraHalvLevel1") ? 0 : pv.size() / 2;
            for (size_t i = 0; i < levelData->size(); i++) {
                size_t pos = offset + lv[i] - 1;
#if 0
                LOG4FIMEX(logger, Logger::DEBUG, "pos: " << pos << " lv[" << i << "]=" << lv[i]);
#endif
                if (pos < pv.size()) {
                    double value;
                    if (pos + 1 < pv.size())
                        value = (pv[pos] + pv[pos + 1]) / 2;
                    else
                        value = pv.back();
                    retValues.push_back(value);
                } else {
                    throw CDMException("levelData (pv) has not enough elements, need " + type2string(levelData->size()));
                }
            }
        } else if (mode == "hybridSigmaCalc(ap,b,p0)") {
            const CDMVariable& p0 = cdm->getVariable("p0" + extension);
            const CDMVariable& ap = cdm->getVariable("ap" + extension);
            const CDMVariable& b = cdm->getVariable("b" + extension);
            auto p0Data = p0.getData()->asDouble();
            auto apData = ap.getData()->asDouble();
            auto bData = b.getData()->asDouble();
            retValues.reserve(ap.getData()->size());
            for (size_t i = 0; i < ap.getData()->size(); ++i) {
                retValues.push_back(apData[i] / p0Data[0] + bData[i]);
            }
        } else {
            throw CDMException("unkown mode '" + mode + "' to extract level-data");
        }

        const auto sscale = getXmlProp(node, "scale_factor");
        if (!sscale.empty()) {
            const double scale = string2type<double>(sscale);
            std::transform(retValues.begin(), retValues.end(), retValues.begin(), std::bind(std::multiplies<double>(), std::placeholders::_1, scale));
        }
    }

    return retValues;
}

void GribCDMIndexer::initSpecialLevels_(std::shared_ptr<CDM> cdm, xmlNodePtr node, const std::string& extension, const leveltype_t& levelType, size_t levelPos,
                                        const std::vector<std::string>& levelShape, DataPtr& levelData)
{
    const std::string xpathAdditional = "gr:additional_axis_variable";
    XPathNodeSet addNodes(init_->config->doc, xpathAdditional, node);
    if (addNodes.size() > 0) {
        const auto& levelPvData = init_->levelValsOfType[levelType][levelPos].levelPvData;
        for (auto inode : addNodes) {
            const auto name = getXmlProp(inode, "name") + extension;
            const auto type = getXmlProp(inode, "type");
            const auto axis = getXmlProp(inode, "axis");
            CDMDataType dataType = string2datatype(type);
            std::vector<std::string> shape;
            if (!axis.empty() && axis != "1") {
                shape = levelShape;
            }
            try {
                CDMVariable var(name, dataType, shape);
                // get data:
                const auto lv = readValuesFromXPath_(cdm, inode, levelData, levelPvData, extension);
                DataPtr lvData = createData(dataType, lv.begin(), lv.end());
                var.setData(lvData);
                cdm->addVariable(var);
            } catch (std::runtime_error& re) {
                LOG4FIMEX(logger, Logger::WARN, "problem adding auxiliary level " << name << ": " << re.what());
            }

            // add attributes
            std::vector<CDMAttribute> levelAttributes;
            fillAttributeListFromXMLNode(levelAttributes, inode->children, init_->templateReplacementAttributes);
            // add the attributes to the CDM
            addAttributes(*cdm, name, levelAttributes);
        }
        // levelData might change, too. Needs to be done after reading all auxiliary variables
        try {
            const auto lv = readValuesFromXPath_(cdm, node, levelData, levelPvData, extension);
            levelData = createData(CDM_DOUBLE, lv.begin(), lv.end());
        } catch (std::runtime_error& re) {
            LOG4FIMEX(logger, Logger::WARN, "problem adding level-data: " << re.what());
        }
    }
}

void GribCDMIndexer::loadOneMessage(const GribFileMessage& gfm)
{
    const auto vc = init_->config->findVariableConfig(gfm);
    if (init_->config->select_only_defined && !vc) {
        // skip this message
        return;
    }

    // use first valid reference time
    // TODO check if reference time changes, assuming they are all alike
    if (init_->referenceTime.invalid()) {
        const auto msgRefTime = gfm.getReferenceTime();
        if (!is_invalid_time_point(msgRefTime)) {
            init_->referenceTime = msgRefTime;
        }
    }

    const auto varName = init_->config->getVariableName(gfm, vc);
    LOG4FIMEX(logger, Logger::DEBUG, "scan varName = '" << varName << "'");

    const auto vhit = init_->varHas.insert(std::make_pair(varName, Init::var_has{}));
    const bool var_was_unknown = vhit.second;
    auto& varHas = vhit.first->second;

    // ----------------------------------------
    // build a set of valid times appearing in the grib messages
    const auto valTime = init_->config->getVariableValidTime(gfm, vc);
    const bool hasTime = !is_invalid_time_point(valTime);
    if (!var_was_unknown && hasTime != varHas.has_time) {
        throw CDMException("grib-variable " + varName + " has messages with time and without: fimex can't proceed");
    } else if (hasTime) {
        init_->times.insert(valTime.toEncoded());
        varHas.has_time = true;
    }

    // ----------------------------------------
    // find maximum ensemble number
    const size_t total_ensembles = gfm.getTotalNumberOfEnsembles();
    const bool hasEnsemble = (total_ensembles > 1);
    size_t perturbation_number = gfm.getPerturbationNumber();
    if (!hasEnsemble && perturbation_number != 0) {
        if (perturbation_number != 255) { // no warning for 255 == MISSING
            LOG4FIMEX(logger, Logger::WARN, "Perturbation number " << perturbation_number << " is illegal without ensembles and will be replaced with 0.");
        }
        perturbation_number = 0;
    }

    if (hasEnsemble) {
        if (init_->maxEnsembles == 0) { // first member
            init_->maxEnsembles = total_ensembles;
        } else if (init_->maxEnsembles < total_ensembles) {
            LOG4FIMEX(logger, Logger::WARN, "increased total number of ensembles from " << init_->maxEnsembles << " to " << total_ensembles);
            init_->maxEnsembles = total_ensembles;
        }
        if (init_->maxEnsembles <= perturbation_number) {
            LOG4FIMEX(logger, Logger::WARN,
                      "increasing ensemble count from " << init_->maxEnsembles << " to " << perturbation_number + 1
                                                        << " -- total number of ensembles might be wrong");
            init_->maxEnsembles = perturbation_number + 1;
        }
    }

    if (var_was_unknown) {
        varHas.has_ensemble = hasEnsemble;
    } else if (varHas.has_ensemble != hasEnsemble) {
        throw CDMException("grib-variable " + varName + " has messages within ensembles, and without: fimex can't proceed");
    }

    // ----------------------------------------
    // remember level and levelType
    const leveltype_t levelType = {gfm.getEdition(), gfm.getLevelType()};
    if (var_was_unknown) {
        varHas.level_type = levelType;
    } else if (varHas.level_type != levelType) {
        const auto& olt = varHas.level_type;
        std::ostringstream msg;
        msg << "typeOfLevel change for variable " << varName << " from " << olt.first << '_' << olt.second << " to " << levelType.first << '_'
            << levelType.second;
        throw CDMException(msg.str());
    }
    varHas.level_numbers.insert(gfm.getLevelNumber());

    // ----------------------------------------
    // grid definitions
    const auto& gridDef = gfm.getGridDefinition();
    if (var_was_unknown) {
        varHas.gridDef = gridDef;
        loadAddProjection(gridDef, gfm.getTypeOfGrid());
    } else if (gridDef != varHas.gridDef) {
        throw CDMException("grib-variable " + varName + " has messages with different grids: fimex can't proceed");
    }

    if (var_was_unknown) {
        if (vc) {
            varHas.datatype = vc->type;
            varHas.vectorDirection = vc->vectorDirection;
            varHas.vectorCounterpart = vc->vectorCounterpart;
            varHas.attributes = vc->attributes;
            varHas.precision = vc->precision;
        } else {
            varHas.datatype = CDM_DOUBLE;
        }

        auto cr = init_->ca->readerFor(gfm.getFileURL());
        gfm.readLevelData(cr, varHas.levelPvData, MIFI_FILL_DOUBLE, false);
    }

    // encoded time; 0 if var has no time (GribMessage returns FimexTime() which is min_time which encodes as 0)
    const auto val_t = valTime.toEncoded();

    const long val_l = gfm.getLevelNumber();
    const size_t val_e = perturbation_number;

    init_->varTimeLevelEnsembleGFIBox[varName][val_t][val_l][val_e] = intoIndex(init_->grib_files, gfm);
}

void GribCDMIndexer::buildLevels()
{
    // map from variableName to levelType and position in levelValsOfType
    for (auto& it_vl : init_->varHas) {
        const auto& varName = it_vl.first;
        const auto& levelType = it_vl.second.level_type;
        const auto& levels = it_vl.second.level_numbers;

        size_t pos;
        const auto it_lvot = init_->levelValsOfType.find(levelType);
        if (it_lvot != init_->levelValsOfType.end()) {
            // level type is known; search for identical set of level numbers
            auto& lvs = it_lvot->second;
            const auto posIt = find_if(lvs.begin(), lvs.end(), [&](const levelvals_t& lv) {
                return lv.values.size() == levels.size() && std::equal(lv.values.begin(), lv.values.end(), levels.begin());
            });
            if (posIt != lvs.end()) {
                // identical set of level numbers found
                pos = distance(lvs.begin(), posIt);
            } else {
                // set of level numbers not known, add it
                pos = lvs.size();
                lvs.push_back({"", it_vl.second.levelPvData, std::vector<long>(levels.begin(), levels.end())});
            }
        } else {
            // level type is not known yet
            pos = 0;
            init_->levelValsOfType[levelType] = {{"", it_vl.second.levelPvData, std::vector<long>(levels.begin(), levels.end())}};
        }

        it_vl.second.level_pos = pos;
    }
}

void GribCDMIndexer::initAddTimeDimension(std::shared_ptr<CDM> cdm, std::shared_ptr<grib_indexed> gi)
{
    XPathNodeSet nodes(init_->config->doc, "/gr:cdmGribReaderConfig/gr:axes/gr:time");
    if (nodes.size() != 1) {
        throw CDMException("unable to find exactly 1 'time'-axis in config: " + init_->config->configId);
    }

    auto node = nodes[0];
    assert(node->type == XML_ELEMENT_NODE);

    init_->timeDimName = getXmlProp(node, "name");
    const auto timeDataType = string2datatype(getXmlProp(node, "type"));

    CDMDimension timeDim(init_->timeDimName, init_->times.size());
    timeDim.setUnlimited(true);
    cdm->addDimension(timeDim);

    CDMVariable timeVar(init_->timeDimName, timeDataType, {init_->timeDimName});
    cdm->addVariable(timeVar);

    std::vector<CDMAttribute> timeAttributes;
    fillAttributeListFromXMLNode(timeAttributes, nodes[0]->children, init_->templateReplacementAttributes);
    addAttributes(*cdm, init_->timeDimName, timeAttributes);
    CDMAttribute tunit;
    if (!cdm->getAttribute(init_->timeDimName, "units", tunit)) {
        // use seconds since 1970 as default
        tunit = CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00");
        cdm->addAttribute(init_->timeDimName, tunit);
    }

    const TimeUnit tu(tunit.getStringValue());
    std::vector<double> timeVecLong;
    timeVecLong.reserve(init_->times.size());
    std::transform(init_->times.begin(), init_->times.end(), std::back_inserter(timeVecLong),
                   [tu](const uint64_t& enct) { return tu.fimexTime2unitTime(FimexTime::fromEncoded(enct)); });
    DataPtr timeData = createData(timeDataType, timeVecLong.begin(), timeVecLong.end());
    cdm->getVariable(init_->timeDimName).setData(timeData);

    // also add reference time
    if (!is_invalid_time_point(init_->referenceTime)) {
        // TODO: move reference time name to config
        const std::string referenceTime = "forecast_reference_time";
        CDMVariable refTimeVar(referenceTime, timeDataType, {});
        DataPtr refTimeData = createData(timeDataType, 1);
        // TODO: this forces times to be seconds since 1970-01-01, maybe I should interpret the config-file unit first
        refTimeData->setValue(0, fimexTime2epochTime(init_->referenceTime));
        refTimeVar.setData(refTimeData);
        cdm->addVariable(refTimeVar);
        cdm->addAttribute(referenceTime, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
        cdm->addAttribute(referenceTime, CDMAttribute("standard_name", "forecast_reference_time"));
    }
}

namespace {
template <typename C>
size_t find_index(const C& container, const typename C::value_type& value)
{
    const auto it = std::find(container.begin(), container.end(), value);
    if (it == container.end())
        throw CDMException("value not found in container");
    return std::distance(container.begin(), it);
}
} // namespace

void GribCDMIndexer::initCreateVarMessages(std::shared_ptr<CDM> cdm, std::shared_ptr<grib_indexed> gi)
{
    for (const auto& gm : init_->varTimeLevelEnsembleGFIBox) {
        const auto& varName = gm.first;
        const auto& var = cdm->getVariable(varName);
        const auto& shape = var.getShape();

        // level data
        const auto& level_type = init_->varHas.at(varName).level_type;
        const auto& level_pos = init_->varHas.at(varName).level_pos;
        const auto& levels = init_->levelValsOfType.at(level_type).at(level_pos).values;

        size_t shapeidx = 0;
        // all variables have x and y dimensions
        const size_t size_x = cdm->getDimension(shape[shapeidx++]).getLength();
        const size_t size_y = cdm->getDimension(shape[shapeidx++]).getLength();
        // variables may have ensemble dimension
        size_t dim_e_len = 1;
        if (shape[shapeidx] == init_->ensembleDimName) {
            dim_e_len = cdm->getDimension(shape[shapeidx++]).getLength();
        }
        // all variables have vertical dimension
        const size_t dim_l_len = cdm->getDimension(shape[shapeidx++]).getLength();
        // variables may have time dimension
        size_t dim_t_len = 1;
        if (shapeidx < shape.size() && shape[shapeidx] == init_->timeDimName) {
            dim_t_len = cdm->getDimension(shape[shapeidx++]).getLength();
        }

        const size_t size_msgs = dim_e_len * dim_l_len * dim_t_len;

        auto& msgs = gi->grib_vars[varName].messages;
        msgs = grib_index_v(size_msgs, invalid_msg);
        const auto& box_time_lev_ens = gm.second;
        for (const auto& items_time_lev_ens : box_time_lev_ens) {
            const size_t idx_t = find_index(init_->times, items_time_lev_ens.first);

            const auto& box_lev_ens = items_time_lev_ens.second;
            for (const auto& items_lev_ens : box_lev_ens) {
                const size_t idx_l = find_index(levels, items_lev_ens.first);

                const auto& items_ens = items_lev_ens.second;
                for (const auto& val_ens : items_ens) {
                    const size_t idx_e = val_ens.first; // perturbation number is used directly as index

                    const auto idx = (idx_t * dim_l_len + idx_l) * dim_e_len + idx_e;
                    if (idx >= size_msgs) {
                        throw CDMException("idx >= size_msgs");
                    }
                    msgs.at(idx) = val_ens.second;
                }
            }
        }
    }

    gi->grib_files = init_->grib_files.strings();
}

void GribCDMIndexer::initAddEnsembles(std::shared_ptr<CDM> cdm, std::shared_ptr<grib_indexed> gi)
{
    if (init_->maxEnsembles <= 1)
        return;
    // TODO: read names from config?

    init_->ensembleDimName = "ensemble_member";
    cdm->addDimension(CDMDimension(init_->ensembleDimName, init_->maxEnsembles));

    CDMVariable ensembleVar(init_->ensembleDimName, CDM_SHORT, {init_->ensembleDimName});
    auto eMembers = make_shared_array<short>(init_->maxEnsembles);
    std::iota(eMembers.get(), eMembers.get() + init_->maxEnsembles, 0);
    DataPtr eData = createData(init_->maxEnsembles, eMembers);
    ensembleVar.setData(eData);
    cdm->addVariable(ensembleVar);
    cdm->addAttribute(ensembleVar.getName(), CDMAttribute("long_name", "ensemble run number"));
    cdm->addAttribute(ensembleVar.getName(), CDMAttribute("standard_name", "realization"));

    if (init_->config->ensembleMemberIds.size() == init_->maxEnsembles) {
        // add a character dimension, naming all ensemble members
        size_t maxLen = 0;
        for (const auto& emi : init_->config->ensembleMemberIds) {
            maxLen = std::max(maxLen, emi.first.size());
        }
        const auto charDim = init_->ensembleDimName + "_strlen";
        cdm->addDimension(CDMDimension(charDim, maxLen));
        CDMVariable names(init_->ensembleDimName + "_names", CDM_STRING, {charDim, init_->ensembleDimName});

        std::string names_values(maxLen * init_->maxEnsembles, 0);
        std::ostringstream members;
        size_t mi = 0;
        for (const auto& emi : init_->config->ensembleMemberIds) {
            const auto& id = emi.first;
            names_values.replace(mi * maxLen, id.size(), id);
            if (mi > 0)
                members << ' ';
            members << id;
            mi += 1;
        }
        names.setData(createData(names_values));
        cdm->addVariable(names);
        cdm->addAttribute(names.getName(), CDMAttribute("long_name", "names of ensemble members"));

        // add the names as flags (needed by ADAGUC)
        std::vector<int> memberFlags(init_->maxEnsembles);
        std::iota(memberFlags.begin(), memberFlags.end(), 0);
        cdm->addAttribute(init_->ensembleDimName, CDMAttribute("flag_values", createData(CDM_INT, memberFlags.begin(), memberFlags.end())));
        cdm->addAttribute(init_->ensembleDimName, CDMAttribute("flag_meanings", members.str()));
    } else if (!init_->config->ensembleMemberIds.empty()) {
        LOG4FIMEX(logger, Logger::ERROR,
                  "have " << init_->config->ensembleMemberIds.size() << " ensemble member names for " << init_->maxEnsembles << " ensemble members");
    }
}

void GribCDMIndexer::loadAddProjection(const GridDefinition& gridDef, const std::string& gridType)
{
    if (init_->gridProjection.find(gridDef) != init_->gridProjection.end()) {
        return;
    }

    LOG4FIMEX(logger, Logger::DEBUG, "new grid id='" << gridDef.id() << "' type='" << gridType << "'");

    init_->lastXYId += 1;
    const std::string projStr = replaceProj4Earthfigure(gridDef.getProjDefinition(), init_->config->replaceEarthString);
    ProjectionInfo pi;

    std::string appendix;
    if (init_->lastXYId > 0) {
        appendix = type2string(init_->lastXYId);
    }

    pi.gridMapping = std::string("projection_" + gridType + appendix);
    // projection-variable without datatype and dimension
    CDMVariable projVar(pi.gridMapping, CDM_NAT, std::vector<std::string>());
    init_->cdm.addVariable(projVar);
    const auto projAttr = Projection::createByProj4(projStr)->getParameters(); // create a copy
    addAttributes(init_->cdm, pi.gridMapping, projAttr);

    {
        // create the x dimension variables and dimensions
        const std::string xpathStringX("/gr:cdmGribReaderConfig/gr:axes/gr:spatial_axis[@typeOfGrid='" + gridType + "' and @id='x']");
        std::vector<CDMAttribute> xVarAttributes;
        std::map<std::string, std::string> xXmlAttributes;
        int found = readXPathNodeWithCDMAttributes(*(init_->config->doc), xpathStringX, xXmlAttributes, xVarAttributes, init_->templateReplacementAttributes);
        if (found != 1) {
            throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringX);
        }
        pi.xDim = xXmlAttributes["name"] + appendix;
        CDMDimension xDim(pi.xDim, gridDef.getXSize());
        CDMDataType xDataType = string2datatype(xXmlAttributes["type"]);
        CDMVariable xVar(pi.xDim, xDataType, {pi.xDim});
        std::vector<double> xData;
        xData.reserve(gridDef.getXSize());
        double xStart = gridDef.getXStart();
        if (gridDef.isDegree()) {
            // normalize only start, coordinate-axes should be monotonous: http://www.unidata.ucar.edu/netcdf/docs/netcdf.html#Variables
            xStart = normalizeLongitude180(gridDef.getXStart());
        }
        for (size_t i = 0; i < gridDef.getXSize(); i++) {
            xData.push_back(xStart + i * gridDef.getXIncrement());
        }
        xVar.setData(createData(xDataType, xData.begin(), xData.end()));
        init_->cdm.addDimension(xDim);
        init_->cdm.addVariable(xVar);
        addAttributes(init_->cdm, pi.xDim, xVarAttributes);
    }
    {
        // create the y dimension variables and dimensions
        std::string xpathStringY("/gr:cdmGribReaderConfig/gr:axes/gr:spatial_axis[@typeOfGrid='" + gridType + "' and @id='y']");
        std::vector<CDMAttribute> yVarAttributes;
        std::map<std::string, std::string> yXmlAttributes;
        int found = readXPathNodeWithCDMAttributes(*(init_->config->doc), xpathStringY, yXmlAttributes, yVarAttributes, init_->templateReplacementAttributes);
        if (found != 1) {
            throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringY);
        }
        pi.yDim = yXmlAttributes["name"] + appendix;
        CDMDimension yDim(pi.yDim, gridDef.getYSize());
        CDMDataType yDataType = string2datatype(yXmlAttributes["type"]);
        CDMVariable yVar(pi.yDim, yDataType, {pi.yDim});
        std::vector<double> yData;
        yData.reserve(gridDef.getYSize());
        for (size_t i = 0; i < gridDef.getYSize(); i++) {
            yData.push_back(gridDef.getYStart() + i * gridDef.getYIncrement());
        }
        // no normalization required for latitudes
        yVar.setData(createData(yDataType, yData.begin(), yData.end()));
        init_->cdm.addDimension(yDim);
        init_->cdm.addVariable(yVar);
        addAttributes(init_->cdm, pi.yDim, yVarAttributes);
    }

    std::string longName, latName;
    {
        // read longitude and latitude names for projection axes
        const std::string xpathStringLong("/gr:cdmGribReaderConfig/gr:axes/gr:spatial_axis[@id='longitude']");
        std::vector<CDMAttribute> lonlatVarAttributes;
        std::map<std::string, std::string> lonlatXmlAttributes;
        int found = readXPathNodeWithCDMAttributes(*(init_->config->doc), xpathStringLong, lonlatXmlAttributes, lonlatVarAttributes,
                                                   init_->templateReplacementAttributes);
        if (found != 1) {
            throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringLong);
        }
        longName = lonlatXmlAttributes["name"] + appendix;
        const std::string xpathStringLat("/gr:cdmGribReaderConfig/gr:axes/gr:spatial_axis[@id='latitude']");
        found = readXPathNodeWithCDMAttributes(*(init_->config->doc), xpathStringLat, lonlatXmlAttributes, lonlatVarAttributes,
                                               init_->templateReplacementAttributes);
        if (found != 1) {
            throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringLat);
        }
        latName = lonlatXmlAttributes["name"] + appendix;
    }

    // add projection axes 'coordinates = "lon lat";
    if (pi.xDim != longName && pi.yDim != latName) {
        pi.coordinates = longName + " " + latName;
        init_->cdm.generateProjectionCoordinates(pi.gridMapping, pi.xDim, pi.yDim, longName, latName);
    }
    init_->gridProjection[gridDef] = pi;
}

void GribCDMIndexer::initAddVariables(std::shared_ptr<CDM> cdm, std::shared_ptr<grib_indexed> gi)
{
    LOG4FIMEX(logger, Logger::DEBUG, "adding variables " << init_->varHas.size());
    for (const auto& vh : init_->varHas) {
        const std::string& varName = vh.first;
        LOG4FIMEX(logger, Logger::DEBUG, "adding variable '" << varName << "'");

        // map shape, generate variable, set attributes/variable to CDM (fastest moving index (x) first, slowest (unlimited, time) last
        const auto& pi = init_->gridProjection.at(vh.second.gridDef);

        std::vector<std::string> shape = {pi.xDim, pi.yDim};
        const auto vhit = init_->varHas.find(varName);
        if (vhit != init_->varHas.end() && vhit->second.has_ensemble) {
            shape.push_back(init_->ensembleDimName);
        }

        gi->grib_vars[varName].scale_factor = vh.second.precision.scale_factor;
        gi->grib_vars[varName].add_offset = vh.second.precision.add_offset;

        const auto vltit = init_->varHas.find(varName);
        assert(vltit != init_->varHas.end());
        const auto& level_type = vltit->second.level_type;
        const auto& level_pos = vltit->second.level_pos;
        const auto& levelDimName = init_->levelValsOfType[level_type].at(level_pos).dimname;
        shape.push_back(levelDimName);

        if (vhit != init_->varHas.end() && vhit->second.has_time) {
            shape.push_back(init_->timeDimName);
        }

        CDMVariable var(varName, vh.second.datatype, shape);
        if (vh.second.vectorCounterpart.empty()) {
            var.setAsSpatialVector(vh.second.vectorCounterpart, CDMVariable::vectorDirectionFromString(vh.second.vectorDirection));
        }
        cdm->addVariable(var);

        // add attributes
        addAttributes(*cdm, varName, vh.second.attributes);
        // add projection attributes
        cdm->addAttribute(varName, CDMAttribute("grid_mapping", pi.gridMapping));
        if (!pi.coordinates.empty()) {
            cdm->addAttribute(varName, CDMAttribute("coordinates", pi.coordinates));
        }
    }
}

GribCDMIndexer::~GribCDMIndexer() {}

} // namespace MetNoFimex
