/*
 * Fimex
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
 */

#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "FeltCDMReader2.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED

#include "CDM_XMLConfigHelper.h"
#include "Felt_Array2.h"
#include "Felt_File2.h"
#include "Felt_File_Error.h"
#include "felt/FeltGridDefinition.h"
#include "fimex/CDM.h"
#include "fimex/CDMDataType.h"
#include "fimex/Data.h"
#include "fimex/ReplaceStringTimeObject.h"
#include "fimex/String2Type.h"
#include "fimex/StringUtils.h"
#include "fimex/TimeUnit.h"
#include "fimex/Type2String.h"
#include "fimex/XMLDoc.h"
#include "fimex/XMLInputFile.h"
#include "fimex/coordSys/Projection.h"

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <memory>
#include <regex>
#include <sstream>

namespace MetNoFimex
{

using namespace std;
using namespace MetNoFelt;

static Logger_p logger = getLogger("fimex.FeltCDMReader2");

vector<double> FeltCDMReader2::readValuesFromXPath(const XMLDoc& doc, const string& variableXPath)
{
    vector<double> retValues;
    string valuesXPath(variableXPath + "/values");
    xmlXPathObject_p xpathObj = doc.getXPathObject(valuesXPath);
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        xmlNodePtr node = nodes->nodeTab[i];
        if (node->type == XML_ELEMENT_NODE) {
            string mode = getXmlProp(node, "mode");
            if (mode == "" || mode == "inline") {
                // add all space delimited values to the retVal vector
                xmlChar *valuePtr = xmlNodeGetContent(node);
                string
                values(reinterpret_cast<const char *>(valuePtr));
                xmlFree(valuePtr);
                vector<string> tokens = tokenize(values, " ");
                transform(tokens.begin(), tokens.end(),
                        back_inserter(retValues), string2type<double>);
            } else if (mode == "level2") {
                // get level-id from variableXPath
                std::smatch matches;
                std::regex rgx(std::regex("felt_id=[\"'](\\d+)[\"']"));
                if (std::regex_search(valuesXPath, matches, rgx)) {
                    short verticalId = string2type<short>(matches[1]);
                    // cannot take reference here, feltfile_->getFeltLevelPairs container will be deleted
                    const vector<pair<short, short> > level2s = (feltfile_->getFeltLevelPairs())[verticalId];
                    for (vector<pair<short, short> >::const_iterator it = level2s.begin(); it != level2s.end(); ++it) {
                        retValues.push_back(it->second);
                    }
                } else {
                    throw CDMException("cannot find felt_id for vertical axes needed to detect level2 in " + valuesXPath);
                }
            } else if (mode == "hybridLevels") {
                // get level-id from variableXPath
                std::smatch matches;
                std::regex rgx(std::regex("felt_id=[\"'](\\d+)[\"']"));
                if (std::regex_search(valuesXPath, matches, rgx)) {
                    short verticalId = string2type<short>(matches[1]);
                    // cannot take reference here, feltfile_->getFeltLevelPairs container will be deleted
                    const vector<pair<short, short> > level2s = (feltfile_->getFeltLevelPairs())[verticalId];
                    const map<LevelPair, int>& hybridLevels = feltfile_->getHybridLevels();
                    for (vector<pair<short, short> >::const_iterator it = level2s.begin(); it != level2s.end(); ++it) {
                        map<LevelPair, int>::const_iterator hl = hybridLevels.find(*it);
                        if (hl != hybridLevels.end()) {
                            retValues.push_back(hl->second);
                        } else {
                            throw CDMException("cannot find hybrid-level for pair: " + type2string(it->first) + "," + type2string(it->second));
                        }
                    }
                } else {
                    throw CDMException("cannot find felt_id for vertical axes needed to detect level2 in " + valuesXPath);
                }
            } else if (mode == "hybridSigmaCalc(ap,b)") {
                // fetch ap, b, and cacl
                // TODO: read reference pressure p0 from CDM (currently not there)
                double p0 = 100000;
                const CDMVariable& ap = getCDM().getVariable("ap");
                const CDMVariable& b = getCDM().getVariable("b");
                shared_array<double> apData = ap.getData()->asDouble();
                shared_array<double> bData = b.getData()->asDouble();
                for (size_t i = 0; i < ap.getData()->size(); ++i) {
                    retValues.push_back(apData[i]/p0 + bData[i]);
                }
            }
            string sscale = getXmlProp(node, "scale_factor");
            if (sscale != "") {
                double scale = string2type<double>(sscale);
                transform(retValues.begin(), retValues.end(),
                        retValues.begin(), bind1st(multiplies<double>(), scale));
            }
        }
    }
    return retValues;
}
void FeltCDMReader2::readAdditionalAxisVariablesFromXPath(const XMLDoc& doc, const string& xpathLevelString,
                                                          const map<string, std::shared_ptr<ReplaceStringObject>>& templateReplacements)
{
    string addAxisXPath(xpathLevelString + "/additional_axis_variable");
    xmlXPathObject_p xpathObj = doc.getXPathObject(addAxisXPath);
    if (xpathObj.get() != 0) {
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        for (int i = 0; i < size; i++) {
            xmlNodePtr node = nodes->nodeTab[i];
            string name = getXmlProp(node,"name");
            string type = getXmlProp(node, "type");
            string axis = getXmlProp(node, "axis");
            CDMDataType dataType = string2datatype(type);
            vector<double> values = readValuesFromXPath(doc, addAxisXPath + "[@name='" + name + "']");
            vector<string> shape;
            if (axis != "") { // not a scalar
                shape.push_back(axis);
            }
            CDMVariable var(name, dataType, shape);
            var.setData(createData(dataType, values.begin(), values.end()));
            cdm_->addVariable(var);


            // add the attributes of the extra variables
            vector<CDMAttribute> attributes;
            fillAttributeListFromXMLNode(attributes, node->children, templateReplacements);
            for (vector<CDMAttribute>::iterator it = attributes.begin(); it != attributes.end(); ++it) {
                cdm_->addAttribute(name, *it);
            }
        }
    }
}


FeltCDMReader2::FeltCDMReader2(string filename, string configFilename)
: filename(filename)
{
    try {
        XMLInputFile config(configFilename);
        init(config);
    } catch (MetNoFelt::Felt_File_Error& ffe) {
        throw CDMException(string("Felt_File_Error: ") + ffe.what());
    }
}

FeltCDMReader2::FeltCDMReader2(string filename, const XMLInput& configInput)
: filename(filename)
{
    try {
        init(configInput);
    } catch (MetNoFelt::Felt_File_Error& ffe) {
        throw CDMException(string("Felt_File_Error: ") + ffe.what());
    }
}


FeltCDMReader2::~FeltCDMReader2()
{
}

void FeltCDMReader2::init(const XMLInput& configInput) {
    // test lib vs compile version
    XMLDoc_p doc = configInput.getXMLDoc();
    configId = configInput.id();
    xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_felt_config");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    if (nodes->nodeNr != 1) {
        throw CDMException("config "+configId+" is not a /cdm_felt_config configuration");
    }

    map<string, string> options = initGetOptionsFromXML(*doc);
    // open the feltFile with the desired parameters
    vector<string> knownFeltIds = initGetKnownFeltIdsFromXML(*doc, options);
    feltfile_ = std::shared_ptr<MetNoFelt::Felt_File2>(new MetNoFelt::Felt_File2(filename, knownFeltIds, options));
    {
        // fill templateReplacementAttributes: MIN_DATETIME, MAX_DATETIME
        vector<MetNoFimex::FimexTime> feltTimes = feltfile_->getFeltTimes();
        if (!feltTimes.empty()) {
            templateReplacementAttributes["MIN_DATETIME"] = std::make_shared<ReplaceStringTimeObject>(fimexTime2epochTime(feltTimes[0]));
            templateReplacementAttributes["MAX_DATETIME"] = std::make_shared<ReplaceStringTimeObject>(fimexTime2epochTime(feltTimes[feltTimes.size() - 1]));
        }
    }

    // fill the CDM;
    // set the global data for this feltFile derived from first data
    // TODO: translate producer-ids to something useful?

    // global attributes from config
    initAddGlobalAttributesFromXML(*doc);


    // add axes
    // time
    CDMDimension timeDim = initAddTimeDimensionFromXML(*doc);
    // ensembleMembers
    CDMDimension ensembleDim;
    vector<short> eMembers = feltfile_->getEnsembleMembers();
    if (eMembers.size() > 0) {
        ensembleDim = CDMDimension("ensemble_member", eMembers.size());
        cdm_->addDimension(ensembleDim);
        vector<string> varShape(1, "ensemble_member");
        CDMVariable ensembleVar("ensemble_member", CDM_SHORT, varShape);
        DataPtr eData = createData(CDM_SHORT, eMembers.begin(), eMembers.end());
        ensembleVar.setData(eData);
        cdm_->addVariable(ensembleVar);
        cdm_->addAttribute(ensembleVar.getName(), CDMAttribute("long_name", "ensemble run number"));
        cdm_->addAttribute(ensembleVar.getName(), CDMAttribute("standard_name", "realization"));
    }
    // levels
    map<short, CDMDimension> levelDims = initAddLevelDimensionsFromXML(*doc);
    //x,y dim will be set with the projection, can also = long/lat
    // setting default-value
    xDim = CDMDimension("x", feltfile_->getNX());
    yDim = CDMDimension("y", feltfile_->getNY());

    // projection of the array (currently only one allowed
    string projName, coordinates;
    // get projection and coordinates
    initAddProjectionFromXML(*doc, projName, coordinates);

    // add variables
    initAddVariablesFromXML(*doc, projName, coordinates, timeDim, ensembleDim, levelDims);
}

vector<string> FeltCDMReader2::initGetKnownFeltIdsFromXML(const XMLDoc& doc, const map<string, string>& options)
{
    string globalRestrictions;
    if (options.find("globalParameterRestrictions") != options.end()) {
        globalRestrictions = ":" + options.find("globalParameterRestrictions")->second;
    }

    vector<string> knownFeltIds;
    xmlXPathObject_p xpathObj = doc.getXPathObject("/cdm_felt_config/variables/parameter");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; ++i) {
        xmlNodePtr node = nodes->nodeTab[i];
        string id = getXmlProp(node, "id");
        // get the datatype
        string dataType = getXmlProp(node, "type");
        if (dataType != "") {
            dataType = ":dataType=" + dataType;
        }
        // get the fill value
        xmlXPathObject_p xpathObj = doc.getXPathObject("/cdm_felt_config/variables/parameter[@id=\"" + id + "\"]/attribute[@name=\"_FillValue\"]");
        string fillValue;
        if (xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
            fillValue = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            if (fillValue != "") {
                fillValue = ":fillValue=" + fillValue;
            }
        }
        knownFeltIds.push_back(id + dataType + fillValue + globalRestrictions);
    }
    return knownFeltIds;
}

map<string, string> FeltCDMReader2::initGetOptionsFromXML(const XMLDoc& doc)
{
    map<string, string> options;
    // optional processing options
    xmlXPathObject_p xpathObj = doc.getXPathObject("/cdm_felt_config/processOptions/option");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; ++i) {
        string name = getXmlProp(nodes->nodeTab[i], "name");
        string value = getXmlProp(nodes->nodeTab[i], "value");
        options[name] = value;
    }
    return options;
}
void FeltCDMReader2::initAddGlobalAttributesFromXML(const XMLDoc& doc)
{
    string xpathString("/cdm_felt_config/global_attributes");
    xmlXPathObject_p xpathObj = doc.getXPathObject(xpathString);
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size != 1) {
        throw MetNoFelt::Felt_File_Error("unable to find " + xpathString + " in config: " + configId);
    }
    for (int i = 0; i < size; ++i) {
        xmlNodePtr node = nodes->nodeTab[i];
        assert(node->type == XML_ELEMENT_NODE);
        vector<CDMAttribute> globAttributes;
        fillAttributeListFromXMLNode(globAttributes, nodes->nodeTab[0]->children, templateReplacementAttributes);
        for (vector<CDMAttribute>::iterator it = globAttributes.begin(); it != globAttributes.end(); ++it) {
            cdm_->addAttribute(cdm_->globalAttributeNS(), *it);
        }
    }
    // FIXME -- same as GribCDMReader
    if (!cdm_->checkVariableAttribute(cdm_->globalAttributeNS(), "history", std::regex(".*"))) {
        const std::string now = make_time_string_extended(make_time_utc_now());
        cdm_->addAttribute(cdm_->globalAttributeNS(), CDMAttribute("history", now + " creation by fimex"));
    }
}

CDMDimension FeltCDMReader2::initAddTimeDimensionFromXML(const XMLDoc& doc)
{
    string timeName;
    string timeType;
    xmlNodePtr timeNode;
    {
        xmlXPathObject_p xpathObj = doc.getXPathObject("/cdm_felt_config/axes/time");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        if (size != 1) {
            throw MetNoFelt::Felt_File_Error("unable to find exactly 1 'time'-axis in config: " + configId);
        }
        timeNode = nodes->nodeTab[0];
        assert(timeNode->type == XML_ELEMENT_NODE);
        timeName = getXmlProp(timeNode, "name");
        timeType = getXmlProp(timeNode, "type");
    }
    string timeUnits;
    {
        xmlXPathObject_p xpathObj = doc.getXPathObject("/cdm_felt_config/axes/time/attribute[@name='units']");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        if (size >= 1) {
            xmlNodePtr node = nodes->nodeTab[0];
            timeUnits = getXmlProp(node, "value");
        }
    }
    if (timeUnits == "") timeUnits = "seconds since 1970-01-01 00:00:00 +00:00";

    long timeSize = feltfile_->getFeltTimes().size();
    CDMDimension timeDim(timeName, timeSize);
    timeDim.setUnlimited(true);
    cdm_->addDimension(timeDim);
    vector<string> timeShape;
    timeShape.push_back(timeDim.getName());
    CDMDataType timeDataType = string2datatype(timeType);
    CDMVariable timeVar(timeName, timeDataType, timeShape);
    timeVec = feltfile_->getFeltTimes();
    vector<double> timeUnitVec;
    TimeUnit tu(timeUnits);
    std::transform(timeVec.begin(), timeVec.end(), std::back_inserter(timeUnitVec), [tu](const FimexTime& tp) { return tu.fimexTime2unitTime(tp); });
    DataPtr timeData = createData(timeDataType, timeUnitVec.begin(), timeUnitVec.end());
    timeVar.setData(timeData);
    cdm_->addVariable(timeVar);
    vector<CDMAttribute> timeAttributes;
    fillAttributeListFromXMLNode(timeAttributes, timeNode->children, templateReplacementAttributes);
    for (vector<CDMAttribute>::iterator it = timeAttributes.begin(); it != timeAttributes.end(); ++it) {
        cdm_->addAttribute(timeVar.getName(), *it);
    }

    // add the unique reference time, if exists
    try {
        FimexTime refTime = feltfile_->getUniqueReferenceTime();
        if (!refTime.invalid()) {
            // TODO: move reference time name to config
            string referenceTime = "forecast_reference_time";
            vector<string> nullShape;
            CDMVariable refTimeVar(referenceTime, timeDataType, nullShape);
            DataPtr timeData = createData(timeDataType, 1);
            timeData->setValue(0, tu.fimexTime2unitTime(refTime));
            refTimeVar.setData(timeData);
            cdm_->addVariable(refTimeVar);
            cdm_->addAttribute(referenceTime, CDMAttribute("units", timeUnits));
            cdm_->addAttribute(referenceTime, CDMAttribute("standard_name", "forecast_reference_time"));
        }
    } catch (Felt_File_Error& ffe) {
        LOG4FIMEX(logger, Logger::DEBUG, ffe.what());
    }

    return timeDim;
}

map<short, CDMDimension> FeltCDMReader2::initAddLevelDimensionsFromXML(const XMLDoc& doc)
{
    map<short, CDMDimension> levelDims;
    map<short, vector<LevelPair> > levels = feltfile_->getFeltLevelPairs();
    for (map<short, vector<LevelPair> >::const_iterator it = levels.begin(); it != levels.end(); ++it) {
        // add a level
        string xpathLevelString("/cdm_felt_config/axes/vertical_axis[@felt_id='"+type2string(it->first)+"']");
        xmlXPathObject_p xpathObj = doc.getXPathObject(xpathLevelString);
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        if (size != 1) {
            throw MetNoFelt::Felt_File_Error("unable to find 'vertical'-axis "+type2string(it->first)+" in config: " + configId);
        }
        xmlNodePtr node = nodes->nodeTab[0];
        assert(node->type == XML_ELEMENT_NODE);
        string levelName = getXmlProp(node, "name");
        string levelId = getXmlProp(node, "id");
        string levelType = getXmlProp(node, "type");
        CDMDataType levelDataType = string2datatype(levelType);
        long levelSize = it->second.size();
        CDMDimension levelDim(levelId, levelSize);
        levelDims.insert(pair<short, CDMDimension>(it->first, levelDim));
        cdm_->addDimension(levelDim);
        levelVecMap[levelDim.getName()] = it->second;

        // create level variable without data!
        vector<string> levelShape;
        levelShape.push_back(levelDim.getName());
        CDMVariable levelVar(levelId, levelDataType, levelShape);
        cdm_->addVariable(levelVar);

        // add attributes
        vector<CDMAttribute> levelAttributes;
        fillAttributeListFromXMLNode(levelAttributes, nodes->nodeTab[0]->children, templateReplacementAttributes);
        for (vector<CDMAttribute>::iterator ait = levelAttributes.begin(); ait != levelAttributes.end(); ++ait) {
            cdm_->addAttribute(levelVar.getName(), *ait);
        }

        // read additional axis variables
        readAdditionalAxisVariablesFromXPath(doc, xpathLevelString, templateReplacementAttributes);

        // read level data after! additional axis variables since additional axis variable might contain
        // data needed from level data (i.e. for hybrid_sigma levels)
        vector<double> lv = readValuesFromXPath(doc, xpathLevelString);
        DataPtr data;
        if (lv.size() > 0) {
            // use values from xml-file
            data = createData(levelDataType, lv.begin(), lv.end());
        } else {
            //use values from felt-file
            const vector<LevelPair>& lpv = it->second;
            vector<short> lvs;
            for (vector<LevelPair>::const_iterator level_it =  lpv.begin(); level_it != lpv.end(); ++level_it) {
                lvs.push_back(level_it->first);
            }
            data = createData(levelDataType, lvs.begin(), lvs.end());
        }
        cdm_->getVariable(levelDim.getName()).setData(data);


    }
    return levelDims;
}

void FeltCDMReader2::initAddProjectionFromXML(const XMLDoc& doc, string& projName, string& coordinates)
{
    std::shared_ptr<felt::FeltGridDefinition> gridDef = feltfile_->getGridDefinition();
    string projStr = gridDef->projDefinition();

    // get the overruled earthform
    {
        xmlXPathObject_p xpathObj = doc.getXPathObject("/cdm_felt_config/overrule/earthFigure");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        string replaceEarthString = "";
        if (size == 1) {
            replaceEarthString = getXmlProp(nodes->nodeTab[0], "proj4");
            LOG4FIMEX(logger, Logger::DEBUG,"overruling earth-parametes with " << replaceEarthString);
            projStr = replaceProj4Earthfigure(projStr, replaceEarthString);
        }
    }

    int gridType = feltfile_->getGridType();
    projName = string("projection_" + type2string(gridType));
    // projection-variable without datatype and dimension
    CDMVariable projVar(projName, CDM_NAT, vector<string>());
    cdm_->addVariable(projVar);
    vector<CDMAttribute> projAttr = Projection::createByProj4(projStr)->getParameters();
    for (vector<CDMAttribute>::iterator attrIt = projAttr.begin(); attrIt != projAttr.end(); ++attrIt) {
        cdm_->addAttribute(projName, *attrIt);
    }

    {
        // create the x dimension variables and dimensions
        string xpathStringX("/cdm_felt_config/axes/spatial_axis[@projection_felt_id='"+type2string(gridType)+"' and @id='x']");
        vector<CDMAttribute> xVarAttributes;
        map<string, string> xXmlAttributes;
        int found = readXPathNodeWithCDMAttributes(doc, xpathStringX, xXmlAttributes, xVarAttributes, templateReplacementAttributes);
        if (found != 1) {
            throw MetNoFelt::Felt_File_Error("error in config-file: not exactly 1 entry for xpath: " + xpathStringX);
        }
        string xName(xXmlAttributes["name"]);
        xDim = CDMDimension(xName, feltfile_->getNX());
        vector<string> xDimShape;
        xDimShape.push_back(xDim.getName());
        DataPtr xData = feltfile_->getXData();
        CDMVariable xVar(xName, xData->getDataType(), xDimShape);
        xVar.setData(xData);
        cdm_->addDimension(xDim);
        cdm_->addVariable(xVar);
        for (vector<CDMAttribute>::iterator attrIt = xVarAttributes.begin(); attrIt != xVarAttributes.end(); ++attrIt) {
            cdm_->addAttribute(xName, *attrIt);
        }
    }
    {
        // create the y dimension variables and dimensions
        string xpathStringY("/cdm_felt_config/axes/spatial_axis[@projection_felt_id='"+type2string(gridType)+"' and @id='y']");
        vector<CDMAttribute> yVarAttributes;
        map<string, string> yXmlAttributes;
        int found = readXPathNodeWithCDMAttributes(doc, xpathStringY, yXmlAttributes, yVarAttributes, templateReplacementAttributes);
        if (found != 1) {
            throw MetNoFelt::Felt_File_Error("error in config-file: not exactly 1 entry for xpath: " + xpathStringY);
        }
        string yName(yXmlAttributes["name"]);
        yDim = CDMDimension(yName, feltfile_->getNY());
        vector<string> yDimShape;
        yDimShape.push_back(yDim.getName());
        DataPtr yData = feltfile_->getYData();
        CDMVariable yVar(yName, yData->getDataType(), yDimShape);
        yVar.setData(yData);
        cdm_->addDimension(yDim);
        cdm_->addVariable(yVar);
        for (vector<CDMAttribute>::iterator attrIt = yVarAttributes.begin(); attrIt != yVarAttributes.end(); ++attrIt) {
            cdm_->addAttribute(yName, *attrIt);
        }
    }

    string longName;
    string latName;
    {
        // read longitude and latitude names for projection axes
        string xpathStringLong("/cdm_felt_config/axes/spatial_axis[@id='longitude']");
        vector<CDMAttribute> lonlatVarAttributes;
        map<string, string> lonlatXmlAttributes;
        int found = readXPathNodeWithCDMAttributes(doc, xpathStringLong, lonlatXmlAttributes, lonlatVarAttributes, templateReplacementAttributes);
        if (found != 1) {
            throw MetNoFelt::Felt_File_Error("error in config-file: not exactly 1 entry for xpath: " + xpathStringLong);
        }
        longName = lonlatXmlAttributes["name"];
        string xpathStringLat("/cdm_felt_config/axes/spatial_axis[@id='latitude']");
        found = readXPathNodeWithCDMAttributes(doc, xpathStringLat, lonlatXmlAttributes, lonlatVarAttributes, templateReplacementAttributes);
        if (found != 1) {
            throw MetNoFelt::Felt_File_Error("error in config-file: not exactly 1 entry for xpath: " + xpathStringLat);
        }
        latName = lonlatXmlAttributes["name"];
    }

    // add projection axes 'coordinates = "lon lat";
    if (gridType != 2 && xDim.getName() != longName && yDim.getName() != latName) {
        coordinates = longName + " " + latName;
        try {
            cdm_->generateProjectionCoordinates(projName, xDim.getName(), yDim.getName(), longName, latName);
        } catch (MetNoFimex::CDMException& ex) {
            throw MetNoFelt::Felt_File_Error(ex.what());
        }
    }
}


void FeltCDMReader2::initAddVariablesFromXML(const XMLDoc& doc, const string& projName, const string& coordinates, const CDMDimension& timeDim, const CDMDimension& ensembleDim, const map<short, CDMDimension>& levelDims)
{
    vector<std::shared_ptr<Felt_Array2>> fArrays(feltfile_->listFeltArrays());
    for (vector<std::shared_ptr<Felt_Array2>>::const_iterator it = fArrays.begin(); it != fArrays.end(); ++it) {
        string xpathString("/cdm_felt_config/variables/parameter[@id='"+(*it)->getName()+"']");
        xmlXPathObject_p xpathObj = doc.getXPathObject(xpathString);
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        if (size > 1) {
            throw MetNoFelt::Felt_File_Error("error in config-file: several entries for parameter: " + (*it)->getName());
        }
        if (size < 1) {
            cerr << "config-file doesn't contain parameter: " << xpathString << endl;
        } else {
            assert(nodes->nodeTab[0]->type == XML_ELEMENT_NODE);
            string varName = getXmlProp(nodes->nodeTab[0], "name");
            vector<CDMAttribute> attributes;
            fillAttributeListFromXMLNode(attributes, nodes->nodeTab[0]->children, templateReplacementAttributes);
            // add the projection
            attributes.push_back(CDMAttribute("grid_mapping",projName));
            if (coordinates != "") {
                attributes.push_back(CDMAttribute("coordinates", coordinates));
            }

            // check if variable is part of vector
            string vectorDirection;
            string vectorCounterpart;
            {
                xmlNodePtr varNodeChild = nodes->nodeTab[0]->children;
                while (varNodeChild != 0) {
                    if ((varNodeChild->type == XML_ELEMENT_NODE) &&
                        (string("spatial_vector") == reinterpret_cast<const char *>(varNodeChild->name))) {
                            vectorDirection = getXmlProp(varNodeChild, "direction");
                            vectorCounterpart = getXmlProp(varNodeChild, "counterpart");
                    }
                    varNodeChild = varNodeChild->next;
                }
            }

            // map shape, generate variable, set attributes/variable to CDM (fastest moving index (x) first, slowest (unlimited, time) last
            vector<string> shape;
            shape.push_back(xDim.getName());
            shape.push_back(yDim.getName());
            if ((*it)->getEnsembleMembers().size() > 1) {
                shape.push_back(ensembleDim.getName());
            }
            if ((*it)->getLevelPairs().size() > 0) {
                if ((*it)->getLevelPairs().size() == 1
                    &&
                      (((*it)->getLevelType() == 2 &&
                        (   ((*it)->getLevelPairs().at(0).first == 1000)
                         || ((*it)->getLevelPairs().at(0).first == 850)
                         || ((*it)->getLevelPairs().at(0).first == 500)
                         || ((*it)->getLevelPairs().at(0).first == 300)
                        )
                       )
                      ||
                       ((*it)->getLevelType() == 8)
                      ||
                       ((*it)->getLevelType() == 3)
                      ||
                       ((*it)->getLevelType() == 0)
                      )
                   ) {
                    // no vertical axis for surface parameters 2,1000, or clouds (500, 850, 300)
                    // no vertical axis for sea-surface parameter 8
                    // level 3 used as sea-surface for some wave-models
                } else {
                    shape.push_back(levelDims.find((*it)->getLevelType())->second.getName());
                }
            }
            if ((*it)->getTimes().size() > 0) {
                shape.push_back(timeDim.getName());
            }
            CDMDataType type = string2datatype((*it)->getDatatype());
            CDMVariable var(varName, type, shape);
            if (!vectorCounterpart.empty()) {
                var.setAsSpatialVector(vectorCounterpart, CDMVariable::vectorDirectionFromString(vectorDirection));
            }
            cdm_->addVariable(var);
            varNameFeltIdMap[varName] = (*it)->getName();
            //  update scaling factor attribute with value from felt-file and xml-setup (only for short-values)
            if ((*it)->getScalingFactor() != 1) {
                bool found = false;
                for (vector<CDMAttribute>::iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
                    if (attrIt->getName() == "scale_factor") {
                        found = true;
                        float scale = (attrIt->getData()->asFloat())[0] * (*it)->getScalingFactor();
                        attrIt->getData()->setValue(0, scale);
                    }
                }
                if (! found) {
                    attributes.push_back(CDMAttribute("scale_factor", static_cast<float>((*it)->getScalingFactor())));
                }
            }
            for (vector<CDMAttribute>::const_iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
                cdm_->addAttribute(varName, *attrIt);
            }
        }
    }
}

DataPtr FeltCDMReader2::getDataSlice(const string& varName, size_t unLimDimPos) {
    LOG4FIMEX(logger, Logger::DEBUG, "reading var: "<< varName << " slice: " << unLimDimPos);
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        return getDataSliceFromMemory(variable, unLimDimPos);
    }
    // only time can be unLimDim
    if (unLimDimPos > timeVec.size()) {
        throw CDMException("requested time outside data-region");
    }

    // felt data can be x,y,level,time; x,y,level; x,y,time; x,y;
    const vector<string>& dims = variable.getShape();
    const CDMDimension* layerDim = 0;
    const CDMDimension* ensembleDim = 0;
    size_t xy_size = 1;
    for (vector<string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
        CDMDimension& dim = cdm_->getDimension(*it);
        if (dim.getName() != xDim.getName() &&
            dim.getName() != yDim.getName() &&
            !dim.isUnlimited())
        {
            if (dim.getName() == "ensemble_member") {
                ensembleDim = &dim;
            } else {
                layerDim = &dim;
            }
        }
        if (! dim.isUnlimited()) {
            xy_size *= dim.getLength();
        }
    }
    DataPtr data = createData(variable.getDataType(), xy_size);
    try {
        map<string, string>::const_iterator foundId = varNameFeltIdMap.find(variable.getName());
        if (foundId != varNameFeltIdMap.end()) {
            std::shared_ptr<MetNoFelt::Felt_Array2> fa(feltfile_->getFeltArray(foundId->second));

            // test for availability of the current time in the variable (getSlice will get data for every time)
            vector<MetNoFimex::FimexTime> faTimes = fa->getTimes();
            short contains = false;
            if (faTimes.size() == 0) {
                // time-less variable, time-check irrelevant
                contains = true;
            } else {
                for (vector<MetNoFimex::FimexTime>::const_iterator it = faTimes.begin(); it != faTimes.end(); it++) {
                    if (*it == timeVec[unLimDimPos]) {
                        contains = true;
                    }
                }
            }
            if (!contains) {
                // return empty dataset
                return createData(variable.getDataType(), 0);
            }

            // select all available layers
            vector<LevelPair> layerVals;
            if ((layerDim != 0) && (layerDim->getLength() > 0)) {
                for (size_t i = 0; i < layerDim->getLength(); ++i) {
                    layerVals.push_back(levelVecMap[layerDim->getName()][i]);
                }
            } else {
                // no layers, just 1 level
                vector<LevelPair> levels = fa->getLevelPairs();
                if (levels.size() == 1) {
                    layerVals.push_back(*(levels.begin()));
                } else {
                    throw CDMException("variable " +variable.getName() + " has unspecified levels");
                }
            }
            // combine all available level/ensemble combinations
            if ((ensembleDim != 0)) {
                vector<short> ensembles = feltfile_->getEnsembleMembers();
                vector<LevelPair> ensembleLayerVals;
                for (size_t i = 0; i < layerVals.size(); i++) {
                    LevelPair lp = layerVals.at(i);
                    for (size_t j = 0; j < ensembles.size(); j++) {
                        lp.second = ensembles.at(j);
                        ensembleLayerVals.push_back(lp);
                    }
                }
                layerVals = ensembleLayerVals;
            }

            size_t dataCurrentPos = 0;
            // xa
            unsigned int xDim = fa->getX();
            unsigned int yDim = fa->getY();
            for (vector<LevelPair>::const_iterator lit = layerVals.begin(); lit != layerVals.end(); ++lit) {
                // get the time, if available
                MetNoFimex::FimexTime t;
                if (timeVec.size() > 0) {
                    t = timeVec[unLimDimPos];
                }
                // read the slice
                DataPtr levelData;
                // level-data might be undefined, create a undefined slice then
                try {
                    OmpScopedLock lock(mutex_);
                    levelData = feltfile_->getScaledDataSlice(fa, t, *lit);
                } catch (NoSuchField_Felt_File_Error nsfe) {
                    levelData = createData(variable.getDataType(), xDim * yDim, cdm_->getFillValue(varName));
                }
                assert(levelData->size() == (xDim*yDim));
                data->setValues(dataCurrentPos, *levelData, 0, levelData->size());
                dataCurrentPos += levelData->size();
            }
        }
    } catch (MetNoFelt::Felt_File_Error& ffe) {
        throw CDMException(string("Felt_File_Error: ") + ffe.what());
    } catch (exception& e) {
        throw CDMException(string("non-Felt_File_Error: ") + e.what());
    }
    return data;
}


}
