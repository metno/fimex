/*
 * Fimex, GribCDMReader.cc
 *
 * (C) Copyright 2009-2019, met.no
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
 *  Created on: Sep 9, 2009
 *      Author: Heiko Klein
 */

#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/GribCDMReader.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/Data.h"
#include "fimex/GribFileIndex.h"
#include "fimex/GridDefinition.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/ReplaceStringTemplateObject.h"
#include "fimex/ReplaceStringTimeObject.h"
#include "fimex/SliceBuilder.h"
#include "fimex/String2Type.h"
#include "fimex/StringUtils.h"
#include "fimex/TimeUnit.h"
#include "fimex/TimeUtils.h"
#include "fimex/TokenizeDotted.h"
#include "fimex/Type2String.h"
#include "fimex/coordSys/Projection.h"

#include "CDM_XMLConfigHelper.h"
#include "MutexLock.h"
#include "RecursiveSliceCopy.h"

#include "fimex_config.h"

#include <algorithm>
#include <cassert>
#include <limits>
#include <map>
#include <numeric>
#include <regex>
#include <set>
#include <stdexcept>

namespace MetNoFimex {

using namespace std;

static Logger_p logger = getLogger("fimex.GribCDMReader");

struct ProjectionInfo {
    string xDim;
    string yDim;
    string coordinates;
    string gridMapping;
};

struct GribCDMReader::Impl
{
    string configId;
    vector<GribFileMessage> indices;
    XMLDoc_p doc;
    map<int, vector<xmlNodePtr> > nodeIdx1;
    map<int, vector<xmlNodePtr> > nodeIdx2;
    OmpMutex mutex;
    map<GridDefinition, ProjectionInfo> gridProjection;
    string timeDimName;
    string ensembleDimName;
    vector<pair<string, std::regex>> ensembleMemberIds;
    size_t maxEnsembles;
    // store ptimes of all times
    vector<FimexTime> times;

    map<string, std::pair<double, double> > varPrecision;
    // varName -> time (unlimDimPos) -> level (val) -> ensemble (val) -> GFI (index)
    map<string, map<size_t, map<long, map<size_t, size_t> > > > varTimeLevelEnsembleGFIBox;
    // varName -> has Ensemble
    map<string, bool> varHasEnsemble;

    // list of different vectors per edition _ typeOfLevel
    map<string, vector<vector<long> > > levelValsOfType;
    // edition_typeOfLevel -> [ dimensionName ]
    map<string, vector<string> > levelDimNames;
    // above levelDimNames as set over the dimensions in the vector<string>
    set<string> levelDimSet;
    // varName -> (edition_levelType, position n levelValsOfType
    map<string, pair<string, size_t> > varLevelTypePos;

    /**
     * config attributes may contain template parameters marked with %PARAM%
     * which should be replaced by dynamic values from the grib-file and stored
     * temporary in this map
     *
     * Currently implemented parameters are: %MIN_DATETIME%, %MAX_DATETIME%: earliest and latest time in felt-file as ISO string
     */
    map<string, std::shared_ptr<ReplaceStringObject>> templateReplacementAttributes;
};

/**
 * generating a unique index for a multidimensional grib-variable
 * which is fast to copy and to find
 */
class GribVarIdx
{
public:
    GribVarIdx() : edition_(0), params_(vector<long>(3,0)) {}
    GribVarIdx(long edition, vector<long> params) : edition_(edition), params_(params) {}
    bool operator<(GribVarIdx rhs) const {
        // order: edition_, param[0], param[...]
        if (edition_ < rhs.edition_) return true;
        else if (edition_ > rhs.edition_) return false;

        // editions equals, test the param-vectors
        return lexicographical_compare(params_.begin(), params_.end(),
                                            rhs.params_.begin(), rhs.params_.end());

    }
    bool operator==(GribVarIdx rhs) const {
        if (edition_ == rhs.edition_) {
            if ((params_.size() == rhs.params_.size()) &&
                equal(params_.begin(), params_.end(), rhs.params_.begin())) {
                return true;
            }
        }
        return false;
    }
private:
    const long edition_;
    const vector<long> params_;
};
// based upon ==
bool operator!=(const GribVarIdx& lhs, const GribVarIdx& rhs) {return !(lhs == rhs); }
// based upon <
bool operator>=(const GribVarIdx& lhs, const GribVarIdx& rhs) {return !(lhs < rhs);}
bool operator>(const GribVarIdx& lhs, const GribVarIdx& rhs) {return (rhs < lhs);}
bool operator<=(const GribVarIdx& lhs, const GribVarIdx& rhs) {return !(rhs < lhs);}

GribCDMReader::GribCDMReader(const vector<string>& fileNames, const XMLInput& configXML, const std::vector<std::pair<std::string, std::string>>& members)
    : p_(new Impl())
{
    initXMLAndMembers(configXML, members);
    std::map<std::string, std::string> options;
    if (getConfigEarthFigure(p_->doc) != "") {
        options["earthfigure"] = getConfigEarthFigure(p_->doc);
    }
    options["extraKeys"] = getConfigExtraKeys(p_->doc);

    for (const std::string& file : fileNames) {
        vector<GribFileMessage> messages = GribFileIndex(file, p_->ensembleMemberIds, options).listMessages();
        p_->indices.insert(p_->indices.end(), messages.begin(), messages.end());
    }
    initPostIndices();
}

GribCDMReader::GribCDMReader(const string& grbmlFileName, const XMLInput& configXML, const std::vector<std::pair<std::string, std::string>>& members)
    : p_(new Impl())
{
    initXMLAndMembers(configXML, members);

    p_->indices = GribFileIndex(grbmlFileName).listMessages();

    initPostIndices();
}

XMLDoc_p GribCDMReader::initXMLConfig(const XMLInput& configXML)
{
    XMLDoc_p doc(configXML.getXMLDoc());
    doc->registerNamespace("gr", "http://www.met.no/schema/fimex/cdmGribReaderConfig");
    {
        // check config for root element
        xmlXPathObject_p xpathObj = doc->getXPathObject("/gr:cdmGribReaderConfig");
        size_t rootElements = (xpathObj->nodesetval == 0) ? 0 : xpathObj->nodesetval->nodeNr;
        if (rootElements != 1)
            throw CDMException("error with rootElement in cdmGribReaderConfig at: " + configXML.id());
    }
    return doc;
}

std::string GribCDMReader::getConfigEarthFigure(XMLDoc_p doc)
{
    // get the overruled earthform
    xmlXPathObject_p xpathObj = doc->getXPathObject("/gr:cdmGribReaderConfig/gr:overrule/gr:earthFigure");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    string replaceEarthString;
    if (size == 1) {
        replaceEarthString = getXmlProp(nodes->nodeTab[0], "proj4");
    }
    return replaceEarthString;
}

std::string GribCDMReader::getConfigExtraKeys(XMLDoc_p doc)
{
    set<string> extraKeys;
    // get the additinal keys from the xml-document
    xmlXPathObject_p xpathObj = doc->getXPathObject("//gr:extraKey");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        extraKeys.insert(getXmlProp(nodes->nodeTab[0], "name"));
    }
    return join(extraKeys.begin(), extraKeys.end(), ",");
}

void GribCDMReader::initXMLAndMembers(const XMLInput& configXML, const std::vector<std::pair<std::string, std::string> >& members)
{
    for (vector<pair<string, string> >::const_iterator memIt = members.begin(); memIt != members.end(); ++memIt) {
        p_->ensembleMemberIds.push_back(make_pair(memIt->first, std::regex(memIt->second)));
    }

    p_->configId = configXML.id();
    p_->doc = initXMLConfig(configXML);
    initXMLNodeIdx();
}

void GribCDMReader::initPostIndices()
{
    // select wanted indices from doc, default to all
    {
        xmlXPathObject_p xpathObj = p_->doc->getXPathObject("/gr:cdmGribReaderConfig/gr:processOptions/gr:option[@name='selectParameters']");
        size_t size = (xpathObj->nodesetval == 0) ? 0 : xpathObj->nodesetval->nodeNr;
        if (size > 0) {
            string select = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            LOG4FIMEX(logger, Logger::DEBUG, "selecting parameters: " << select);
            initSelectParameters(select);
        }
    }

    if (!p_->indices.empty()) {
        // time-dimension needs to be added before global attributes due to replacements
        initAddTimeDimension();
        // fill templateReplacementAttributes: MIN_DATETIME, MAX_DATETIME
        if (!p_->times.empty()) {
            p_->templateReplacementAttributes["MIN_DATETIME"] = std::make_shared<ReplaceStringTimeObject>(fimexTime2epochTime(p_->times.at(0)));
            p_->templateReplacementAttributes["MAX_DATETIME"] =
                std::make_shared<ReplaceStringTimeObject>(fimexTime2epochTime(p_->times.at(p_->times.size() - 1)));
        }

        initAddGlobalAttributes();
        initCreateGFIBoxes();
        initAddEnsembles();
        initLevels();
        initAddProjection();
        initAddVariables();
    }
}

void GribCDMReader::initXMLNodeIdx() {
    string xpathString = "/gr:cdmGribReaderConfig/gr:variables/gr:parameter";
    xmlXPathObject_p xpathObj = p_->doc->getXPathObject(xpathString);
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    size_t size = (nodes) ? nodes->nodeNr : 0;
    //cerr << "found " << size << " parameters " << endl;
    for (size_t i = 0; i < size; ++i) {
        xmlNodePtr node = nodes->nodeTab[i];
        string grib1Id = "gr:grib1";
        xmlXPathObject_p xpathObj1 = p_->doc->getXPathObject(grib1Id, node);
        xmlNodeSetPtr nodes1 = xpathObj1->nodesetval;
        size_t size1 = (nodes1) ? nodes1->nodeNr : 0;
        for (size_t i = 0; i < size1; ++i) {
            xmlNodePtr node1 = nodes1->nodeTab[i];
            string idVal = getXmlProp(node1, GK_indicatorOfParameter);
            if (idVal.empty())
                continue;
            long id = string2type<long>(idVal);
            p_->nodeIdx1[id].push_back(node1);
        }
        // and the same for grib2
        string grib2Id = "gr:grib2";
        xmlXPathObject_p xpathObj2 = p_->doc->getXPathObject(grib2Id, node);
        xmlNodeSetPtr nodes2 = xpathObj2->nodesetval;
        size_t size2 = (nodes2) ? nodes2->nodeNr : 0;
        for (size_t i = 0; i < size2; ++i) {
            xmlNodePtr node2 = nodes2->nodeTab[i];
            string idVal = getXmlProp(node2, "parameterNumber");
            if (idVal.empty())
                continue;
            long id = string2type<long>(idVal);
            p_->nodeIdx2[id].push_back(node2);
        }
    }
    //cerr << "nodeIdx1: " << p_->nodeIdx1.size() << endl;
}

xmlNodePtr GribCDMReader::findVariableXMLNode(const GribFileMessage& msg) const
{
    const vector<long>& pars = msg.getParameterIds();
    if (pars.size() < 3)
        return 0;
    map<string, long> optionals;
    optionals["typeOfLevel"] = msg.getLevelType();
    optionals["levelNo"] = msg.getLevelNumber();
    optionals[GK_timeRangeIndicator] = msg.getTimeRangeIndicator();
    optionals[GK_typeOfStatisticalProcessing] = msg.getTypeOfStatisticalProcessing();
    map<string, string> optionals_string;
    optionals_string[GK_stepType] = msg.getStepType();

    vector<xmlNodePtr> nodes;
    const long param = pars.front();
    if (msg.getEdition() == 1) {
        nodes = p_->nodeIdx1[param];
        optionals[GK_gribTablesVersionNo] = pars[1];
        optionals[GK_identificationOfOriginatingGeneratingCentre] = pars[2];
    } else {
        nodes = p_->nodeIdx2[param];
        optionals[GK_parameterCategory] = pars[1];
        optionals[GK_discipline] = pars[2];
    }

    LOG4FIMEX(logger, Logger::DEBUG,
              "searching GRIB message with "
                  << " edition=" << msg.getEdition() << " indOfPar/parNum=" << pars.at(0) << " gTblVerNo/parCat=" << pars[1]
                  << " idOfOrigGenCtr/disc=" << pars[2] << " typeOfLevel=" << msg.getLevelType() << " levelNo=" << msg.getLevelNumber()
                  << " timeRangeIndicator=" << msg.getTimeRangeIndicator() << " " << GK_typeOfStatisticalProcessing << "="
                  << msg.getTypeOfStatisticalProcessing() << " " << GK_stepType << "='" << msg.getStepType() << "'");

    if (!nodes.empty()) {
        LOG4FIMEX(logger, Logger::DEBUG, "found parameter for edition " << msg.getEdition() << " and id " << pars.at(0));
        vector<xmlNodePtr> matchingNodes;
        for (xmlNodePtr node : nodes) {
            LOG4FIMEX(logger, Logger::DEBUG, "start checking node");
            bool allOptionalsMatch = true;

            for (const auto& opt : optionals) {
                const string optVal = getXmlProp(node, opt.first);
                if (!optVal.empty()) {
                    LOG4FIMEX(logger, Logger::DEBUG, "node numeric prop '" << opt.first << "' is '" << optVal << "'");
                    const vector<long> optVals = tokenizeDotted<long>(optVal, ",");
                    if (find(optVals.begin(), optVals.end(), opt.second) == optVals.end()) {
                        // opt->second not found
                        // optional set and not the same as this message value, don't use
                        allOptionalsMatch = false;
                        break;
                    }
                }
            }
            if (!allOptionalsMatch)
                continue;

            for (const auto& opt : optionals_string) {
                const string optVal = getXmlProp(node, opt.first);
                if (!optVal.empty()) {
                    LOG4FIMEX(logger, Logger::DEBUG, "node string prop '" << opt.first << "' is '" << optVal << "'");
                    const vector<string> optVals = split_any(optVal, ",");
                    if (find(optVals.begin(), optVals.end(), opt.second) == optVals.end()) {
                        // opt->second not found
                        // optional set and not the same as this message value, don't use
                        allOptionalsMatch = false;
                        break;
                    }
                }
            }
            if (!allOptionalsMatch)
                continue;

            // check the options from msg.getOtherKeys()
            for (const auto& opt : msg.getOtherKeys()) {
                string xpathStr = "gr:extraKey[@name='" + opt.first + "']";
                xmlXPathObject_p xpathObj = p_->doc->getXPathObject(xpathStr, node);
                xmlNodeSetPtr nodes = xpathObj->nodesetval;
                size_t size = (nodes) ? nodes->nodeNr : 0;
                if (size != 0) {
                    long val = string2type<long>(getXmlProp(nodes->nodeTab[0], "value"));
                    if (val != opt.second) {
                        allOptionalsMatch = false;
                        break;
                    }
                }
            }

            if (allOptionalsMatch) {
                LOG4FIMEX(logger, Logger::DEBUG, "node match");
                matchingNodes.push_back(node->parent); // return the parent, since xpath looks for grib1/2 node
            }
        }

        if (!matchingNodes.empty()) {
            const Logger::LogLevel level = Logger::INFO;
            if (matchingNodes.size() > 1 && logger->isEnabledFor(level)) {
                stringstream opt_ss;
                for (const auto& opt : optionals)
                    opt_ss << opt.first << "=" << opt.second << ", ";
                for (const auto& opt : optionals_string)
                    opt_ss << opt.first << "='" << opt.second << "', ";
                LOG4FIMEX(logger, level,
                          "using first of several parameters for edition '" << msg.getEdition() << "', id '" << pars.at(0) << "' and " << opt_ss.str());
            }
            return matchingNodes.front();
        }
    }
    LOG4FIMEX(logger, Logger::DEBUG, "no parameter found in config for edition '" << msg.getEdition() << "', id '" << pars.at(0) << "'");
    return 0;
}

void GribCDMReader::initSelectParameters(const string& select)
{
    if (select == "all") {
        // nothing to do
    } else if (select == "definedOnly") {
        vector<GribFileMessage> newIndices;
        for (const GribFileMessage& gfm : p_->indices) {
            if (findVariableXMLNode(gfm)) {
                // parameter found
                newIndices.push_back(gfm);
            }
        }
        p_->indices = newIndices;
    } else {
        throw runtime_error("unknown select-parameter: " + select);
    }
}


void GribCDMReader::initAddGlobalAttributes()
{
    string xpathString("/gr:cdmGribReaderConfig/gr:global_attributes");
    xmlXPathObject_p xpathObj = p_->doc->getXPathObject(xpathString);
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size != 1) {
        throw CDMException("unable to find " + xpathString + " in config: " + p_->configId);
    }
    for (int i = 0; i < size; ++i) {
        assert(nodes->nodeTab[i]->type == XML_ELEMENT_NODE);
        vector<CDMAttribute> globAttributes;
        fillAttributeListFromXMLNode(globAttributes, nodes->nodeTab[0]->children, p_->templateReplacementAttributes);
        for (vector<CDMAttribute>::iterator it = globAttributes.begin(); it != globAttributes.end(); ++it) {
            cdm_->addAttribute(cdm_->globalAttributeNS(), *it);
        }
    }
    if (!cdm_->checkVariableAttribute(cdm_->globalAttributeNS(), "history", std::regex(".*"))) {
        const std::string now = make_time_string_extended(make_time_utc_now());
        cdm_->addAttribute(cdm_->globalAttributeNS(), CDMAttribute("history", now + " creation by fimex"));
    }
}

/// add the levelOfType for grib-edition edition to the levelDimsOfType, and the levels to the CDM
void GribCDMReader::initLevels()
{
    for (const auto& lit : p_->levelValsOfType) {
        const vector<string> editionType = tokenize(lit.first, "_");
        long edition = string2type<long>(editionType.at(0));
        long typeId = string2type<long>(editionType.at(1));
        string xpathLevelString("/gr:cdmGribReaderConfig/gr:axes/gr:vertical_axis[@grib"+type2string(edition)+"_id='"+type2string(typeId)+"']");
        xmlXPathObject_p xpathObj = p_->doc->getXPathObject(xpathLevelString);
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        if (size > 1) {
            throw CDMException("more than one 'vertical'-axis "+type2string(typeId)+" in config: " + p_->configId +" and xpath: " + xpathLevelString);
        }
        string levelName, levelId;
        CDMDataType levelDataType;
        xmlNodePtr node = 0;
        if (size == 0) {
            LOG4FIMEX(logger, Logger::INFO, "no definition for vertical axis " + type2string(typeId) + " found, using default");
            levelName = "grib"+type2string(edition) +"_vLevel" + type2string(typeId);
            levelId = levelName;
            levelDataType = CDM_FLOAT;
        } else {
            node = nodes->nodeTab[0];
            assert(node->type == XML_ELEMENT_NODE);
            levelName = getXmlProp(node, "name");
            levelId = getXmlProp(node, "id");
            levelDataType = string2datatype(getXmlProp(node, "type"));
        }
        vector<string> dimNames;
        dimNames.reserve(lit.second.size());
        for (size_t i = 0; i < lit.second.size(); ++i) {
            const string myExtension = (lit.second.size() > 1 ? type2string(i) : "");
            const string myLevelId = findUniqueDimName(*cdm_, levelId + myExtension);
            dimNames.push_back(myLevelId);
            LOG4FIMEX(logger, Logger::DEBUG, "declaring level: " << myLevelId);
            CDMDimension levelDim(myLevelId, lit.second.at(i).size());
            cdm_->addDimension(levelDim);

            // create level variable
            const vector<string> levelShape(1, levelDim.getName());
            CDMVariable levelVar(levelDim.getName(), levelDataType, levelShape);
            cdm_->addVariable(levelVar);

            // add level variable data
            DataPtr levelData = createData(levelDataType, lit.second.at(i).begin(), lit.second.at(i).end());
            // add special data for hybrid levels
            if (node != 0) {
                initSpecialLevels_(node, myExtension, lit.first, i, levelShape, levelData);
            }
            cdm_->getVariable(levelVar.getName()).setData(levelData);

            // add attributes
            vector<CDMAttribute> levelAttributes;
            if (node != 0) {
                map<string, std::shared_ptr<ReplaceStringObject>> replacements;
                replacements["EXT"] = std::make_shared<ReplaceStringTemplateObject<string>>(myExtension);
                fillAttributeListFromXMLNode(levelAttributes, nodes->nodeTab[0]->children, replacements);

                // add special attributes for grib1 / grib2
                {
                    string xpathGribLevelString("gr:grib"+type2string(edition));
                    xmlXPathObject_p xpathObj = p_->doc->getXPathObject(xpathGribLevelString, nodes->nodeTab[0]);
                    xmlNodeSetPtr gribNodes = xpathObj->nodesetval;
                    int size = (gribNodes) ? gribNodes->nodeNr : 0;
                    if (size == 1) {
                        fillAttributeListFromXMLNode(levelAttributes, gribNodes->nodeTab[0]->children, p_->templateReplacementAttributes);
                    }
                }
            } else {
                levelAttributes.push_back(CDMAttribute("units", "1"));
                levelAttributes.push_back(CDMAttribute("long_name", "grib"+type2string(edition)+ "-level " + type2string(typeId)));
            }

            // add the attributes to the CDM
            for (const CDMAttribute& attr : levelAttributes) {
                cdm_->addAttribute(levelVar.getName(), attr);
            }
        }
        p_->levelDimNames[lit.first] = dimNames;
    }

    // create a fast lookup set for all level-names
    for (map<string, vector<string> >::const_iterator levDimsIt = p_->levelDimNames.begin(); levDimsIt !=  p_->levelDimNames.end(); ++levDimsIt) {
        for (vector<string>::const_iterator levDimsIt2 = levDimsIt->second.begin(); levDimsIt2 != levDimsIt->second.end(); ++levDimsIt2) {
            p_->levelDimSet.insert(*levDimsIt2);
        }
    }
}

vector<double> GribCDMReader::readVarPv_(string exampleVar, bool asimofHeader)
{
    vector<double> pv;
    // example gribFileMessage
    size_t gfiPos = p_->varTimeLevelEnsembleGFIBox[exampleVar].begin()->second.begin()->second.begin()->second;
    // Read asimof header if true
    size_t count = p_->indices.at(gfiPos).readLevelData(pv, MIFI_FILL_DOUBLE, asimofHeader);
    if (count <= 0) {
        LOG4FIMEX(logger, Logger::WARN, "could not find extra level data (PV)");
    }
    return pv;
}

vector<double> GribCDMReader::readValuesFromXPath_(xmlNodePtr node, DataPtr levelData, string exampleVar, string extension)
{
    vector<double> retValues;
    string valuesXPath("./gr:values");
    xmlXPathObject_p xpathObj = p_->doc->getXPathObject(valuesXPath, node);
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        xmlNodePtr node = nodes->nodeTab[i];
        if (node->type == XML_ELEMENT_NODE) {
            string mode = getXmlProp(node, "mode");
            if (mode.empty() || mode == "inline") {
                // add all space delimited values to the retVal vector
                xmlChar *valuePtr = xmlNodeGetContent(node);
                string values(reinterpret_cast<const char *>(valuePtr));
                xmlFree(valuePtr);
                vector<string> tokens = tokenize(values, " ");
                transform(tokens.begin(), tokens.end(),
                        back_inserter(retValues), string2type<double>);
            } else if (mode == "extraLevel1" || mode == "extraLevel2") {
                vector<double> pv = readVarPv_(exampleVar);
                size_t offset = (mode == "extraLevel1") ? 0 : pv.size()/2;
                shared_array<unsigned int> lv = levelData->asUInt();
                for (size_t i = 0; i < levelData->size(); i++) {
                    size_t pos = offset + lv[i];
                    if (pos < pv.size()) {
                        // using pos-1 due to fortran numbering in grib
                        double value = (pos == 0) ? pv.at(0) : pv[pos-1];
                        retValues.push_back(value);
                    } else {
                        throw CDMException("levelData (pv) of " + exampleVar + " has not enough elements, need " + type2string(pos+1));
                    }
                }
            } else if (mode == "extraHalvLevel1" || mode == "extraHalvLevel2") {
                bool asimofHeader = false;
                vector<double> pv = readVarPv_(exampleVar,asimofHeader);
                shared_array<unsigned int> lv = levelData->asUInt();
                if (pv.size()/2 < levelData->size()) {
                    // reread pv, try asimof header
                    asimofHeader=true;
                    pv = readVarPv_(exampleVar, asimofHeader);
                    // TODO: Check and read all messages if pv.size()/2 != levelData->size()
                    //if (pv.size()/2 != levelData->size())
                    //{
                    //}
                }
                // NOTE: In the asimof header, the values are a1,b1,a2,b2...an,bn
                size_t offset;
                if (!asimofHeader) {
                    offset = (mode == "extraHalvLevel1") ? 0 : pv.size()/2;
                } else {
                    offset = (mode == "extraHalvLevel1") ? 0 : 1;
                }
                for (size_t i = 0; i < levelData->size(); i++) {
                    size_t pos;
                    if (!asimofHeader) {
                        pos = offset + lv[i] - 1;
                    } else {
                        pos = offset + 2*i;
                    }
                    LOG4FIMEX(logger, Logger::DEBUG, "pos: " << pos << " lv[" << i << "]=" << lv[i]);
                    if (pos < pv.size()) {
                        double value;
                        if (!asimofHeader) {
                            if (pos+1 < pv.size())
                                value = (pv[pos] + pv[pos+1])/2;
                            else
                                value = pv.back();
                        } else {
                            if (pos < 2)
                                value = pv[pos];
                            else
                                value = (pv[pos] + pv[pos-2])/2;
                        }
                        retValues.push_back(value);
                    } else {
                        throw CDMException("levelData (pv) of " + exampleVar + " has not enough elements, need " + type2string(levelData->size()));
                    }
                }
            } else if (mode == "hybridSigmaCalc(ap,b,p0)") {
                // fetch ap, b, and calc
                const CDMVariable& p0 = getCDM().getVariable("p0"+ extension);
                const CDMVariable& ap = getCDM().getVariable("ap" + extension);
                const CDMVariable& b = getCDM().getVariable("b" + extension);
                shared_array<double> p0Data = p0.getData()->asDouble();
                shared_array<double> apData = ap.getData()->asDouble();
                shared_array<double> bData = b.getData()->asDouble();
                for (size_t i = 0; i < ap.getData()->size(); ++i) {
                    retValues.push_back(apData[i]/p0Data[0] + bData[i]);
                }
            } else {
                LOG4FIMEX(logger, Logger::WARN, "unkown mode '"+ mode +"' to extract level-data for variable '" + exampleVar + "' using some dummy values");
                shared_array<double> d = levelData->asDouble();
                retValues = vector<double>(&d[0], &d[0]+levelData->size());
            }
            string sscale = getXmlProp(node, "scale_factor");
            if (!sscale.empty()) {
                double scale = string2type<double>(sscale);
                transform(retValues.begin(), retValues.end(),
                        retValues.begin(), bind1st(multiplies<double>(), scale));
            }
        }
    }
    if (size == 0) {
        shared_array<double> d = levelData->asDouble();
        retValues = vector<double>(&d[0], &d[0]+levelData->size());
    }
    return retValues;
}

void GribCDMReader::initSpecialLevels_(xmlNodePtr node, const string& extension, const string& levelType, size_t levelPos, const vector<string>& levelShape, DataPtr& levelData) {
    string xpathAdditional = "gr:additional_axis_variable";
    xmlXPathObject_p xpathObj = p_->doc->getXPathObject(xpathAdditional, node);
    xmlNodeSetPtr addNodes = xpathObj->nodesetval;
    int size = (addNodes) ? addNodes->nodeNr : 0;
    if (size > 0) {
        string exampleVar;
        // find example variable
        for (const auto& vltp : p_->varLevelTypePos) {
            const string& vltype = vltp.second.first;
            size_t vltpos = vltp.second.second;
            if ((levelType == vltype) && (levelPos == vltpos)) {
                exampleVar = vltp.first;
                break;
            }
        }
        for (int i = 0; i < size; i++) {
            xmlNodePtr inode = addNodes->nodeTab[i];
            string name = getXmlProp(inode,"name") + extension;
            string type = getXmlProp(inode, "type");
            string axis = getXmlProp(inode, "axis");
            CDMDataType dataType = string2datatype(type);
            vector<string> shape;
            if (!axis.empty() && axis != "1") {
                shape = levelShape;
            }
            try {
                CDMVariable var(name, dataType, shape);
                // get data:
                vector<double> lv = readValuesFromXPath_(inode, levelData, exampleVar, extension);
                DataPtr lvData = createData(dataType, lv.begin(), lv.end());
                var.setData(lvData);
                cdm_->addVariable(var);
            } catch (runtime_error& re) {
                LOG4FIMEX(logger, Logger::WARN, "problem adding auxiliary level " << name << ": "  << re.what());
            }

            // add attributes
            vector<CDMAttribute> levelAttributes;
            fillAttributeListFromXMLNode(levelAttributes, inode->children, p_->templateReplacementAttributes);
            // add the attributes to the CDM
            for (const CDMAttribute& att : levelAttributes) {
                cdm_->addAttribute(name, att);
            }
        }
        // levelData might change, too. Needs to be done after reading all auxiliary variables
        try {
            vector<double> lv = readValuesFromXPath_(node, levelData, exampleVar, extension);
            levelData = createData(CDM_DOUBLE, lv.begin(), lv.end());
        } catch (runtime_error& re) {
            LOG4FIMEX(logger, Logger::WARN, "problem adding level-data for " << exampleVar << ": "  << re.what());
        }
    }
}

void GribCDMReader::initAddTimeDimension()
{
    // get all times, unique and sorted
    {
        set<FimexTime> timesSet;
        for (const GribFileMessage& gfm : p_->indices) {
            const FimexTime vt = getVariableValidTime(gfm);
            if (!is_invalid_time_point(vt)) {
                timesSet.insert(vt);
            }
        }
        p_->times = vector<FimexTime>(timesSet.begin(), timesSet.end());
    }

    xmlXPathObject_p xpathObj = p_->doc->getXPathObject("/gr:cdmGribReaderConfig/gr:axes/gr:time");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size != 1) {
        throw CDMException("unable to find exactly 1 'time'-axis in config: " + p_->configId);
    }
    xmlNodePtr node = nodes->nodeTab[0];
    assert(node->type == XML_ELEMENT_NODE);
    p_->timeDimName = getXmlProp(node, "name");
    CDMDataType timeDataType = string2datatype(getXmlProp(node, "type"));

    CDMDimension timeDim(p_->timeDimName, p_->times.size());
    timeDim.setUnlimited(true);
    cdm_->addDimension(timeDim);
    vector<string> timeShape;
    timeShape.push_back(timeDim.getName());
    CDMVariable timeVar(p_->timeDimName, timeDataType, timeShape);
    cdm_->addVariable(timeVar);
    vector<CDMAttribute> timeAttributes;
    fillAttributeListFromXMLNode(timeAttributes, nodes->nodeTab[0]->children, p_->templateReplacementAttributes);
    for (vector<CDMAttribute>::iterator it = timeAttributes.begin(); it != timeAttributes.end(); ++it) {
        cdm_->addAttribute(timeVar.getName(), *it);
    }
    CDMAttribute tunit;
    if (! cdm_->getAttribute(timeVar.getName(), "units", tunit)) {
        // use seconds since 1970 as default
        tunit = CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00");
        cdm_->addAttribute(timeVar.getName(), tunit);
    }
    const TimeUnit tu(tunit.getStringValue());
    vector<double> timeVecLong;
    std::transform(p_->times.begin(), p_->times.end(), std::back_inserter(timeVecLong), [tu](const FimexTime& tp) { return tu.fimexTime2unitTime(tp); });
    DataPtr timeData = createData(timeDataType, timeVecLong.begin(), timeVecLong.end());
    cdm_->getVariable(timeVar.getName()).setData(timeData);

    // TODO check if reference time changes, assuming they are all alike
    FimexTime refTime;
    for (size_t i = 0; is_invalid_time_point(refTime) && (i < p_->indices.size()); ++i) {
        refTime = p_->indices.at(i).getReferenceTime();
    }
    if (!is_invalid_time_point(refTime)) {
        // TODO: move reference time name to config
        string referenceTime = "forecast_reference_time";
        vector<string> nullShape;
        CDMVariable refTimeVar(referenceTime, timeDataType, nullShape);
        DataPtr refTimeData = createData(timeDataType, 1);
        // TODO: this forces times to be seconds since 1970-01-01, maybe I should interpret the config-file unit first
        refTimeData->setValue(0, fimexTime2epochTime(refTime));
        refTimeVar.setData(refTimeData);
        cdm_->addVariable(refTimeVar);
        cdm_->addAttribute(referenceTime, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
        cdm_->addAttribute(referenceTime, CDMAttribute("standard_name", "forecast_reference_time"));
    }

}

string GribCDMReader::getVariableName(const GribFileMessage& gfm) const
{
    xmlNodePtr node = findVariableXMLNode(gfm);
    string varName;
    if (!node) {
        // prepend names from grib-api with 'ga_'
        // since they might otherwise start numerical, which is against CF, and buggy in netcdf 3.6.3, 4.0.*
        varName = "ga_" + gfm.getShortName() + "_" + type2string(gfm.getLevelType());
    } else {
        varName = getXmlProp(node, "name");
    }
    return varName;
}

FimexTime GribCDMReader::getVariableValidTime(const GribFileMessage& gfm) const
{
    xmlNodePtr node = findVariableXMLNode(gfm);
    if (node && getXmlProp(node, "constantTime") == "true")
        return FimexTime();
    else
        return gfm.getValidTime();
}

// IN1 and IN2 should both be collections
template<typename IN1, typename IN2>
class EqualFunctor : public unary_function<IN2, bool> {
private:
    IN1 in1_;
public:
    EqualFunctor(IN1 in1) : in1_(in1) {}
    bool operator()(IN2 in2) {
        if (in1_.size() == in2.size()) {
            return std::equal(in1_.begin(), in1_.end(), in2.begin());
        } else {
            return false;
        }
    }
};

void GribCDMReader::initCreateGFIBoxes()
{
    // go through all indices and create a box for each variable
    // with time,level,ensemble -> GribFileIndex
    // create also lists with total levels and ensembles per variable
    map<string, set<long> > varLevels;
    map<string, string> varLevelType;
    p_->maxEnsembles = 0;
    int pos = 0;
    for (const GribFileMessage& gfm : p_->indices) {
        const string varName = getVariableName(gfm);
        const FimexTime valTime = getVariableValidTime(gfm);
        size_t unlimDimPos = std::numeric_limits<std::size_t>::max();
        if (!is_invalid_time_point(valTime)) {
            vector<FimexTime>::iterator pTimesIt = find(p_->times.begin(), p_->times.end(), valTime);
            assert(pTimesIt != p_->times.end());
            unlimDimPos = distance(p_->times.begin(), pTimesIt);
        }

        const size_t total_ensembles = gfm.getTotalNumberOfEnsembles(), perturbation_number = gfm.getPerturbationNumber();
        const bool hasEnsemble = (total_ensembles > 1);

        if (hasEnsemble) {
            if (p_->maxEnsembles == 0) { // first member
                p_->maxEnsembles = total_ensembles;
            } else if (p_->maxEnsembles < total_ensembles) {
                LOG4FIMEX(logger, Logger::WARN, "increased total number of ensembles from " << p_->maxEnsembles << " to " << total_ensembles);
                p_->maxEnsembles = total_ensembles;
            }
            if (p_->maxEnsembles <= perturbation_number) {
                LOG4FIMEX(logger, Logger::WARN,
                          "increasing ensemble count from " << p_->maxEnsembles << " to " << perturbation_number + 1
                                                            << " -- total number of ensembles might be wrong");
                p_->maxEnsembles = perturbation_number + 1;
            }
        }

        const auto vhit = p_->varHasEnsemble.find(varName);
        if (vhit == p_->varHasEnsemble.end()) {
            p_->varHasEnsemble.insert(std::make_pair(varName, hasEnsemble));
        } else if (vhit->second != hasEnsemble) {
            throw CDMException("grib-variable " + varName + " has messages within ensembles, and without: fimex can't proceed");
        }

        // varName -> time (unlimDimPos) -> level (val) -> ensemble (val) -> GFI (index)
        //map<string, map<size_t, map<long, map<size_t, size_t> > > > varTimeLevelEnsembleGFIBox;
        p_->varTimeLevelEnsembleGFIBox[varName][unlimDimPos][gfm.getLevelNumber()][perturbation_number] = pos++;

        // remember level and levelType
        varLevels[varName].insert(gfm.getLevelNumber());

        const string levelType = type2string(gfm.getEdition()) + "_" + type2string(gfm.getLevelType());
        const auto vltit = varLevelType.find(varName);
        if (vltit == varLevelType.end()) {
            varLevelType.insert(std::make_pair(varName, levelType));
        } else if (vltit->second != levelType) {
            throw CDMException("typeOfLevel change for variable " + varName + " from " + vltit->second + " to " + levelType);
        }
    }

    // map from variableName to levelType and position in levelValsOfType
    for (const auto& varTypeIt : varLevelType) {
        const string& varName = varTypeIt.first;
        const string& levelType = varTypeIt.second;
        const set<long>& levels = varLevels[varName];


        // addLevelForType
        vector<vector<long> > levelForType;
        if (p_->levelValsOfType.find(levelType) != p_->levelValsOfType.end()) {
            levelForType = p_->levelValsOfType[levelType];
        }
        vector<vector<long> >::iterator posIt =
                find_if(levelForType.begin(), levelForType.end(), EqualFunctor<set<long>, vector<long> >(levels));
        size_t pos;
        if (posIt == levelForType.end()) {
            pos = levelForType.size();
            levelForType.push_back(vector<long>(levels.begin(), levels.end()));
        } else {
            pos = distance(levelForType.begin(), posIt);
        }
        p_->levelValsOfType[levelType] = levelForType;

        p_->varLevelTypePos[varName] = make_pair(levelType, pos);
    }
}

void GribCDMReader::initAddEnsembles()
{
    if (p_->maxEnsembles <= 1)
        return;
    // TODO: read names from config?

    p_->ensembleDimName = "ensemble_member";
    cdm_->addDimension(CDMDimension(p_->ensembleDimName, p_->maxEnsembles));

    CDMVariable ensembleVar(p_->ensembleDimName, CDM_SHORT, {p_->ensembleDimName});
    shared_array<short> eMembers(new short[p_->maxEnsembles]);
    std::iota(eMembers.get(), eMembers.get() + p_->maxEnsembles, 0);
    DataPtr eData = createData(p_->maxEnsembles, eMembers);
    ensembleVar.setData(eData);
    cdm_->addVariable(ensembleVar);
    cdm_->addAttribute(ensembleVar.getName(), CDMAttribute("long_name", "ensemble run number"));
    cdm_->addAttribute(ensembleVar.getName(), CDMAttribute("standard_name", "realization"));

    if (p_->ensembleMemberIds.size() == p_->maxEnsembles) {
        // add a character dimension, naming all ensemble members
        size_t maxLen = 0;
        for (const auto& emi : p_->ensembleMemberIds) {
            maxLen = std::max(maxLen, emi.first.size());
        }
        const string charDim = p_->ensembleDimName + "_strlen";
        cdm_->addDimension(CDMDimension(charDim, maxLen));
        CDMVariable names(p_->ensembleDimName + "_names", CDM_STRING, {charDim, p_->ensembleDimName});

        std::string names_values(maxLen * p_->maxEnsembles, 0);
        std::ostringstream members;
        size_t mi = 0;
        for (const auto& emi : p_->ensembleMemberIds) {
            const string& id = emi.first;
            names_values.replace(mi * maxLen, id.size(), id);
            if (mi > 0)
                members << ' ';
            members << id;
            mi += 1;
        }
        names.setData(createData(names_values));
        cdm_->addVariable(names);
        cdm_->addAttribute(names.getName(), CDMAttribute("long_name", "names of ensemble members"));

        // add the names as flags (needed by ADAGUC)
        vector<int> memberFlags(p_->maxEnsembles);
        std::iota(memberFlags.begin(), memberFlags.end(), 0);
        cdm_->addAttribute(p_->ensembleDimName, CDMAttribute("flag_values", createData(CDM_INT, memberFlags.begin(), memberFlags.end())));
        cdm_->addAttribute(p_->ensembleDimName, CDMAttribute("flag_meanings", members.str()));
    } else if (!p_->ensembleMemberIds.empty()) {
        LOG4FIMEX(logger, Logger::ERROR, "have " << p_->ensembleMemberIds.size() << " ensemble member names for " << p_->maxEnsembles << " ensemble members");
    }
}

void GribCDMReader::initAddProjection()
{
    // get the overruled earthform
    string replaceEarthString = getConfigEarthFigure(p_->doc);
    if (replaceEarthString != "") {
        LOG4FIMEX(logger, Logger::DEBUG,"overruling earth-parametes with " << replaceEarthString);
    }

    // gridDefinition -> gridType
    map<GridDefinition, string> gridDefs;
    for (const GribFileMessage& gfm : p_->indices) {
        bool inserted = gridDefs.insert(std::make_pair(gfm.getGridDefinition(), gfm.getTypeOfGrid())).second;
        if (inserted) {
            LOG4FIMEX(logger, Logger::DEBUG, "found gridDef:" << gfm.getGridDefinition().id());
        }
    }
    assert(!gridDefs.empty());

    int lastXYId = 0;
    for (map<GridDefinition, string>::iterator gd = gridDefs.begin(); gd != gridDefs.end(); gd++, lastXYId++) {
        const GridDefinition& gridDef(gd->first);
        LOG4FIMEX(logger, Logger::DEBUG,"adding gridDef:" << gridDef.id());
        string& gridType(gd->second);
        string projStr = replaceProj4Earthfigure(gridDef.getProjDefinition(), replaceEarthString);
        ProjectionInfo pi;

        string appendix;
        if (lastXYId > 0) {
            appendix = type2string(lastXYId);
        }

        pi.gridMapping = string("projection_" + gridType + appendix);
        // projection-variable without datatype and dimension
        CDMVariable projVar(pi.gridMapping, CDM_NAT, vector<string>());
        cdm_->addVariable(projVar);
        for (const auto& attr : Projection::createByProj4(projStr)->getParameters()) {
            cdm_->addAttribute(pi.gridMapping, attr);
        }

        {
            // create the x dimension variables and dimensions
            string xpathStringX("/gr:cdmGribReaderConfig/gr:axes/gr:spatial_axis[@typeOfGrid='"+gridType+"' and @id='x']");
            vector<CDMAttribute> xVarAttributes;
            map<string, string> xXmlAttributes;
            int found = readXPathNodeWithCDMAttributes(*(p_->doc), xpathStringX, xXmlAttributes, xVarAttributes, p_->templateReplacementAttributes);
            if (found != 1) {
                throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringX);
            }
            pi.xDim = xXmlAttributes["name"] + appendix;
            CDMDimension xDim(pi.xDim, gridDef.getXSize());
            CDMDataType xDataType = string2datatype(xXmlAttributes["type"]);
            CDMVariable xVar(pi.xDim, xDataType, {pi.xDim});
            vector<double> xData;
            xData.reserve(gridDef.getXSize());
            double xStart = gridDef.getXStart();
            if (gridDef.isDegree()) {
                // normalize only start, coordinate-axes should be monotonous: http://www.unidata.ucar.edu/netcdf/docs/netcdf.html#Variables
                xStart = normalizeLongitude180(gridDef.getXStart());
            }
            for (size_t i=0; i < gridDef.getXSize(); i++) {
                xData.push_back(xStart + i*gridDef.getXIncrement());
            }
            xVar.setData(createData(xDataType, xData.begin(), xData.end()));
            cdm_->addDimension(xDim);
            cdm_->addVariable(xVar);
            for (const CDMAttribute& attr : xVarAttributes) {
                cdm_->addAttribute(pi.xDim, attr);
            }
        }
        {
            // create the y dimension variables and dimensions
            string xpathStringY("/gr:cdmGribReaderConfig/gr:axes/gr:spatial_axis[@typeOfGrid='"+gridType+"' and @id='y']");
            vector<CDMAttribute> yVarAttributes;
            map<string, string> yXmlAttributes;
            int found = readXPathNodeWithCDMAttributes(*(p_->doc), xpathStringY, yXmlAttributes, yVarAttributes, p_->templateReplacementAttributes);
            if (found != 1) {
                throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringY);
            }
            pi.yDim = yXmlAttributes["name"] + appendix;
            CDMDimension yDim(pi.yDim, gridDef.getYSize());
            CDMDataType yDataType = string2datatype(yXmlAttributes["type"]);
            CDMVariable yVar(pi.yDim, yDataType, {pi.yDim});
            vector<double> yData;
            yData.reserve(gridDef.getYSize());
            for (size_t i=0; i < gridDef.getYSize(); i++) {
                yData.push_back(gridDef.getYStart() + i*gridDef.getYIncrement());
            }
            // no normalization required for latitudes
            yVar.setData(createData(yDataType, yData.begin(), yData.end()));
            cdm_->addDimension(yDim);
            cdm_->addVariable(yVar);
            for (const CDMAttribute& attr : yVarAttributes) {
                cdm_->addAttribute(pi.yDim, attr);
            }
        }

        string longName, latName;
        {
            // read longitude and latitude names for projection axes
            string xpathStringLong("/gr:cdmGribReaderConfig/gr:axes/gr:spatial_axis[@id='longitude']");
            vector<CDMAttribute> lonlatVarAttributes;
            map<string, string> lonlatXmlAttributes;
            int found = readXPathNodeWithCDMAttributes(*(p_->doc), xpathStringLong, lonlatXmlAttributes, lonlatVarAttributes, p_->templateReplacementAttributes);
            if (found != 1) {
                throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringLong);
            }
            longName = lonlatXmlAttributes["name"] + appendix;
            string xpathStringLat("/gr:cdmGribReaderConfig/gr:axes/gr:spatial_axis[@id='latitude']");
            found = readXPathNodeWithCDMAttributes(*(p_->doc), xpathStringLat, lonlatXmlAttributes, lonlatVarAttributes, p_->templateReplacementAttributes);
            if (found != 1) {
                throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringLat);
            }
            latName = lonlatXmlAttributes["name"] + appendix;
        }

        // add projection axes 'coordinates = "lon lat";
        if (pi.xDim != longName && pi.yDim != latName) {
            pi.coordinates = longName + " " + latName;
            cdm_->generateProjectionCoordinates(pi.gridMapping, pi.xDim, pi.yDim, longName, latName);
        }
        p_->gridProjection[gridDef] = pi;
    }
}

void GribCDMReader::initAddVariables()
{
    set<string> initializedVariables;
    for (const GribFileMessage& gfm : p_->indices) {
        const ProjectionInfo pi = p_->gridProjection[gfm.getGridDefinition()];
        assert(pi.xDim != "");
        CDMDataType type = CDM_DOUBLE;
        string varName = getVariableName(gfm);
        if (initializedVariables.find(varName) == initializedVariables.end()) {
            initializedVariables.insert(varName);
            xmlNodePtr node = findVariableXMLNode(gfm);
            vector<CDMAttribute> attributes;
            if (node != 0) {
                fillAttributeListFromXMLNode(attributes, node->children, p_->templateReplacementAttributes);
            }

            // add the projection
            attributes.push_back(CDMAttribute("grid_mapping",pi.gridMapping));
            if (!pi.coordinates.empty()) {
                attributes.push_back(CDMAttribute("coordinates", pi.coordinates));
            }

            string vectorDirection;
            string vectorCounterpart;
            if (node != 0) {
                // set the datatype if exists
                const string stype = getXmlProp(node, "type");
                if (!stype.empty()) {
                    type = string2datatype(stype);
                }
                // check if variable is part of vector
                {
                    xmlNodePtr varNodeChild = node->children;
                    while (varNodeChild != 0) {
                        if ((varNodeChild->type == XML_ELEMENT_NODE) &&
                                (string("spatial_vector") == reinterpret_cast<const char *>(varNodeChild->name))) {
                            vectorDirection = getXmlProp(varNodeChild, "direction");
                            vectorCounterpart = getXmlProp(varNodeChild, "counterpart");
                        }
                        varNodeChild = varNodeChild->next;
                    }
                }
            }
            // check precision
            if (node != 0) {
                xmlNodePtr varNodeChild = node->children;
                while (varNodeChild != 0) {
                    if ((varNodeChild->type == XML_ELEMENT_NODE) &&
                        (string("precision") == reinterpret_cast<const char *>(varNodeChild->name))) {
                        double scale = 1.;
                        double offset = 0.;
                        string str = getXmlProp(varNodeChild, "scale_factor");
                        // TODO: fix scale_factor and add_offset attributes when already existing
                        if (!str.empty()) {
                            scale = string2type<double>(str);
                        }
                        str = getXmlProp(varNodeChild, "add_offset");
                        if (str != "") {
                            offset = string2type<double>(str);
                        }
                        p_->varPrecision[varName] = make_pair(scale, offset);
                        if (type != CDM_FLOAT && type != CDM_DOUBLE) {
                            if (scale != 1.) {
                                attributes.push_back(CDMAttribute("scale_factor", scale));
                            }
                            if (offset != 0.) {
                                attributes.push_back(CDMAttribute("add_offset", offset));
                            }
                        }
                    }
                    varNodeChild = varNodeChild->next;
                }
            }

            // map shape, generate variable, set attributes/variable to CDM (fastest moving index (x) first, slowest (unlimited, time) last
            vector<string> shape = {pi.xDim, pi.yDim};
            if (!p_->ensembleDimName.empty() && gfm.getTotalNumberOfEnsembles() > 1) {
                shape.push_back(p_->ensembleDimName);
            }
            const auto vltit = p_->varLevelTypePos.find(varName);
            assert(vltit != p_->varLevelTypePos.end());
            const pair<string, size_t>& levelTypePos = vltit->second;

            const string& levelDimName = p_->levelDimNames[levelTypePos.first].at(levelTypePos.second);
            shape.push_back(levelDimName);
            if (!is_invalid_time_point(getVariableValidTime(gfm))) {
                shape.push_back(p_->timeDimName);
            }

            CDMVariable var(varName, type, shape);
            if (!vectorCounterpart.empty()) {
                var.setAsSpatialVector(vectorCounterpart, CDMVariable::vectorDirectionFromString(vectorDirection));
            }
            cdm_->addVariable(var);
            for (const CDMAttribute& att : attributes) {
                cdm_->addAttribute(varName, att);
            }
        }
    }
 }


GribCDMReader::~GribCDMReader()
{
}

size_t GribCDMReader::getVariableMaxEnsembles(const string& varName) const
{
    size_t ensembles;
    if (p_->varHasEnsemble.at(varName)) {
        ensembles = p_->maxEnsembles;
    } else {
        ensembles = 1;
    }
    return ensembles;
}

template<typename T>
class RoundValue : public unary_function<T, T> {
private:
    T scale_;
    T scaleInv_;
    T initialMissing_;
    T finalMissing_;
public:
    RoundValue(double scale, double initialMissing, double finalMissing) :
        scale_(scale), scaleInv_(1/scale), initialMissing_(initialMissing), finalMissing_(finalMissing) {}
    T operator()(T in) {
        if (in == initialMissing_)
            return finalMissing_;
        return scale_ * MetNoFimex::round(scaleInv_ * in);
    }
};

template <typename T>
DataPtr roundData(shared_array<T> array, size_t n, double scale, double initialMissing, double finalMissing)
{
    transform(array.get(), array.get()+n, array.get(), RoundValue<T>(scale, initialMissing, finalMissing));
    return createData(n, array);
}


vector<size_t> createVector(size_t id, const vector<size_t>& dimStart, const vector<size_t>& dimSizes)
{
    vector<size_t> retVal;
    if (id == std::numeric_limits<size_t>::max()) {
        retVal.push_back(std::numeric_limits<size_t>::max());
    } else {
        for (size_t i = 0; i < dimSizes.at(id); i++) {
            retVal.push_back(i + dimStart.at(id));
        }
    }
    return retVal;
}

DataPtr GribCDMReader::getDataSlice(const string& varName, const SliceBuilder& sb)
{
    LOG4FIMEX(logger, Logger::DEBUG, "fetching slicebuilder for variable " << varName);
    const CDMVariable& variable = cdm_->getVariable(varName);

    if (variable.getDataType() == CDM_NAT) {
        return createData(CDM_INT,0); // empty
    }

    if (DataPtr mem = getDataSliceFromMemory(variable, sb))
        return mem;

    //map<string, map<size_t, map<long, map<size_t, size_t> > > > varTimeLevelEnsembleGFIBox;
    const auto gmIt = p_->varTimeLevelEnsembleGFIBox.find(varName);
    if (gmIt == p_->varTimeLevelEnsembleGFIBox.end()) {
        throw CDMException("no grib message found for variable '" + varName + "'");
    }

    // grib data can be (x,y,[ensemble,]level,time) or (x,y,[ensemble,]level) or just (x,y[,ensemble])
    const vector<string>& dimNames = sb.getDimensionNames();
    //cerr << join(dimNames.begin(), dimNames.end()) << endl;
    assert(dimNames.at(0) != p_->timeDimName);
    const vector<size_t>& dimSizes = sb.getDimensionSizes();
    const vector<size_t>& dimStart = sb.getDimensionStartPositions();
    const vector<size_t>& maxSizes = sb.getMaxDimensionSizes();

    // x/y = dimNames/Size 0,1
    const size_t xySliceSize = dimSizes.at(0) * dimSizes.at(1);
    size_t sliceSize = xySliceSize;
    size_t levelId = std::numeric_limits<size_t>::max(); // undefined
    size_t timeId = std::numeric_limits<size_t>::max(); // undefined
    size_t ensembleId = std::numeric_limits<size_t>::max(); // undefined
    for (size_t i = 2; i < dimNames.size(); ++i) {
        sliceSize *= dimSizes.at(i);
        if (dimNames.at(i) == p_->ensembleDimName) {
            ensembleId = i;
        } else if (dimNames.at(i) == p_->timeDimName) {
            timeId = i;
        } else if (p_->levelDimSet.find(dimNames.at(i)) != p_->levelDimSet.end()) {
            // this is a level
            levelId = i;
        } else {
            throw CDMException("unknown dimension '" + dimNames.at(i) + "' for variable '" + varName + " in Slicebuilder");
        }
    }

    LOG4FIMEX(logger, Logger::DEBUG, "building slices for variable " << varName << ": size: " << sliceSize);
    const vector<size_t> timeSlices = createVector(timeId, dimStart, dimSizes);
    const vector<size_t> levelSlices = createVector(levelId, dimStart, dimSizes);
    const vector<size_t> ensembleSlices = createVector(ensembleId, dimStart, dimSizes);
    vector<GribFileMessage> slices;
    for (size_t ts : timeSlices) {
        const auto gmt = gmIt->second.find(ts);
        if (gmt == gmIt->second.end()) {
            for (size_t e = 0; e < ensembleSlices.size(); ++e) {
                for (size_t l = 0; l < levelSlices.size(); ++l) {
                    slices.push_back(GribFileMessage()); // add empty slices
                }
            }
        } else {
            const auto& typePos = p_->varLevelTypePos.at(varName);
            const vector<long>& levels = p_->levelValsOfType.at(typePos.first).at(typePos.second);
            for (size_t ls : levelSlices) {
                const size_t lev = (ls == std::numeric_limits<size_t>::max()) ? 0 : ls; // undefined added as 0
                const auto gmtl = gmt->second.find(levels.at(lev));
                if (gmtl != gmt->second.end()) {
                    for (size_t es : ensembleSlices) {
                        const size_t ens = (es == std::numeric_limits<size_t>::max()) ? 0 : es; // undefined added as 0
                        const auto gmtle = gmtl->second.find(ens);
                        if (gmtle != gmtl->second.end()) {
                            slices.push_back(p_->indices.at(gmtle->second));
                        } else {
                            slices.push_back(GribFileMessage()); // add empty slice
                        }
                    }
                } else {
                    // layer not found, add empty slices
                    for (size_t e = 0; e < ensembleSlices.size(); ++e) {
                        slices.push_back(GribFileMessage()); // add empty slices
                    }
                }
            }
        }
    }

    // read data from file
    if (slices.empty())
        return createData(variable.getDataType(), 0);

    const size_t maxXySize = maxSizes.at(0) * maxSizes.at(1);

    // storage for complete data
    shared_array<double> doubleArray(new double[sliceSize]);
    // prefill with missing values

    double missingValue = cdm_->getFillValue(varName);
    if (p_->varPrecision.find(varName) != p_->varPrecision.end()) {
        // varPrecision used, use default missing
        missingValue = MIFI_FILL_DOUBLE;
    }
    fill(&doubleArray[0], &doubleArray[sliceSize], missingValue);
    DataPtr data = createData(sliceSize, doubleArray);
    size_t dataCurrentPos = 0;

    const bool xyslice = (maxXySize != xySliceSize);

    // storage for one layer, required only if making xy-slice
    shared_array<double> full_data_array;
    vector<size_t> orgSizes, orgSliceSize, newStart, newSizes;
    if (xyslice) {
        LOG4FIMEX(logger, Logger::DEBUG, "need xy slicing");
        full_data_array = shared_array<double>(new double[maxXySize]);

        orgSizes = {maxSizes.at(0), maxSizes.at(1)};
        orgSliceSize = {1, maxSizes.at(0)};
        newStart = {dimStart.at(0), dimStart.at(1)};
        newSizes = {dimSizes.at(0), dimSizes.at(1)};
    }

    for (const auto& gfm : slices) {
        // join the data of the different levels
        if (gfm.isValid()) {
            double* data_out = &doubleArray[dataCurrentPos];
            double* grib_out = xyslice ? full_data_array.get() : data_out;
            LOG4FIMEX(logger, Logger::DEBUG,
                      "start reading variable " << gfm.getShortName() << ", level " << gfm.getLevelNumber() << ", store at " << dataCurrentPos);
            size_t dataRead;
            {
#ifndef HAVE_GRIB_THREADSAFE
                OmpScopedLock lock(p_->mutex);
#endif
                dataRead = gfm.readData(grib_out, maxXySize, missingValue);
            }
            LOG4FIMEX(logger, Logger::DEBUG, "done reading variable");
            if (dataRead != maxXySize) {
                LOG4FIMEX(logger, Logger::WARN, "unexpected data size " << dataRead << ", setting to missingValue");
                fill(data_out, data_out + xySliceSize, missingValue);
            } else if (xyslice) { // slicing on xy-data
                recursiveCopyMultiDimData(&full_data_array[0], data_out, orgSizes, orgSliceSize, newStart, newSizes);
            }
        } else {
            LOG4FIMEX(logger, Logger::DEBUG,
                      "skipping variable " << varName << ", 1 level, "
                                           << " size " << xySliceSize);
        }
        dataCurrentPos += xySliceSize; // always forward a complete slice
    }
    std::map<string, std::pair<double, double>>::const_iterator it = p_->varPrecision.find(varName);
    if (it != p_->varPrecision.end()) {
        const double scale = it->second.first;
        const double offset = it->second.second;
        if (variable.getDataType() == CDM_FLOAT || variable.getDataType() == CDM_DOUBLE) {
            if (variable.getDataType() == CDM_FLOAT) {
                data = roundData(data->asFloat(), data->size(), scale, missingValue, cdm_->getFillValue(varName));
            } else {
                data = roundData(data->asDouble(), data->size(), scale, missingValue, cdm_->getFillValue(varName));
            }
        } else {
            data = data->convertDataType(missingValue, 1, 0, variable.getDataType(), cdm_->getFillValue(varName), scale, offset);
        }
    }
    return data;

}

DataPtr GribCDMReader::getDataSlice(const string& varName, size_t unLimDimPos)
{
    LOG4FIMEX(logger, Logger::DEBUG, "fetching unlim-slice " << unLimDimPos << " for variable " << varName);
    const CDMVariable& variable = cdm_->getVariable(varName);

    if (variable.getDataType() == CDM_NAT) {
        return createData(CDM_INT,0); // empty
    }
    if (variable.hasData()) {
        return getDataSliceFromMemory(variable, unLimDimPos);
    }
    // only time can be unLimDim for grib
    SliceBuilder sb(*cdm_, varName);
    if (cdm_->hasUnlimitedDim(variable)) {
        if (unLimDimPos >= cdm_->getDimension(p_->timeDimName).getLength()) {
            throw CDMException("requested time outside data-region");
        }
        sb.setStartAndSize(p_->timeDimName, unLimDimPos, 1);
    }
    return getDataSlice(varName, sb);
}

} // namespace MetNoFimex
