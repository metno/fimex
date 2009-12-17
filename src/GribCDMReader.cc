/*
 * Fimex, GribCDMReader.cc
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
 *  Created on: Sep 9, 2009
 *      Author: Heiko Klein
 */

#include "fimex/config.h"
#ifdef HAVE_GRIBAPI_H
#include "fimex/CDM.h"
#include <boost/regex.hpp>
#include "fimex/GribCDMReader.h"
#include "fimex/GridDefinition.h"
#include "fimex/GribFileIndex.h"
#include "CDM_XMLConfigHelper.h"
#include "fimex/XMLDoc.h"
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/Data.h"
#include "fimex/DataImpl.h"
#include "fimex/ReplaceStringTimeObject.h"
#include <algorithm>

namespace MetNoFimex
{

using namespace std;

static LoggerPtr logger = getLogger("fimex.GribCDMReader");

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
        return std::lexicographical_compare(params_.begin(), params_.end(),
                                            rhs.params_.begin(), rhs.params_.end());

    }
    bool operator==(GribVarIdx rhs) const {
        if (edition_ == rhs.edition_) {
            if ((params_.size() == rhs.params_.size()) &&
                std::equal(params_.begin(), params_.end(), rhs.params_.begin())) {
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


GribCDMReader::GribCDMReader(const std::vector<std::string>& fileNames, const std::string& configFile)
    : configFile_(configFile)
{
    doc_ = boost::shared_ptr<XMLDoc>(new XMLDoc(configFile_));
    doc_->registerNamespace("gr", "http://www.met.no/schema/fimex/cdmGribReaderConfig");
    {
        // check config for root element
        XPathObjPtr xpathObj = doc_->getXPathObject("/gr:cdmGribReaderConfig");
        size_t rootElements = (xpathObj->nodesetval == 0) ? 0 : xpathObj->nodesetval->nodeNr;
        if (rootElements != 1) throw CDMException("error with rootElement in cdmGribReaderConfig at: " + configFile);
    }


    for (vector<string>::const_iterator fileIt = fileNames.begin(); fileIt != fileNames.end(); ++fileIt) {
        vector<GribFileMessage> messages = GribFileIndex(*fileIt).listMessages();
        copy(messages.begin(), messages.end(), back_inserter(indices_));
    }

    // select wanted indices from doc, default to all
    {
        XPathObjPtr xpathObj = doc_->getXPathObject("/gr:cdmGribReaderConfig/gr:processOptions/gr:option[@name='selectParameters']");
        size_t size = (xpathObj->nodesetval == 0) ? 0 : xpathObj->nodesetval->nodeNr;
        if (size > 0) {
            string select = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            LOG4FIMEX(logger, Logger::DEBUG, "selecting parameters: " << select);
            initSelectParameters(select);
        }
    }


    if (indices_.size() == 0) return;

    // currently only one gridDefinition for all paramters supported
    // search indices for params with same gridDefinition (size,start,incr)
    double gridDefinitionDelta = 0.01;
    {
        XPathObjPtr xpathObj = doc_->getXPathObject("/gr:cdmGribReaderConfig/gr:processOptions/gr:option[@name='gridDefinitionDelta']");
        size_t size = (xpathObj->nodesetval == 0) ? 0 : xpathObj->nodesetval->nodeNr;
        if (size > 0) {
            gridDefinitionDelta = string2type<double>(getXmlProp(xpathObj->nodesetval->nodeTab[0], "value"));
        }
    }

    GridDefinition gd = indices_.at(0).getGridDefinition();
    vector<GribFileMessage> newIndices;
    for (vector<GribFileMessage>::const_iterator gfmIt = indices_.begin(); gfmIt != indices_.end(); ++gfmIt) {
        if (gd.comparableTo(gfmIt->getGridDefinition(), gridDefinitionDelta)) {
            newIndices.push_back(*gfmIt);
        } else {
            LOG4FIMEX(logger, Logger::INFO, "different gridDefinitions between " << indices_.at(0) << " and " << *gfmIt);
        }
    }
    indices_ = newIndices;

    // time-dimension needs to be added before global attributes due to replacements
    initAddTimeDimension();
    // fill templateReplacementAttributes: MIN_DATETIME, MAX_DATETIME
    if (times_.size() > 0) {
        templateReplacementAttributes_["MIN_DATETIME"] = boost::shared_ptr<ReplaceStringObject>(new ReplaceStringTimeObject(posixTime2epochTime(times_.at(0))));
        templateReplacementAttributes_["MAX_DATETIME"] = boost::shared_ptr<ReplaceStringObject>(new ReplaceStringTimeObject(posixTime2epochTime(times_.at(times_.size()-1))));
    }

    initAddGlobalAttributes();
    map<string, CDMDimension> levelDims = initAddLevelDimensions();
    string projName, coordinates;
    initAddProjection(projName, coordinates);
    initAddVariables(projName, coordinates, levelDims);
}

xmlNodePtr GribCDMReader::findVariableXMLNode(const GribFileMessage& msg) const
{
    string xpathString;
    const vector<long>& pars = msg.getParameterIds();
    if (msg.getEdition() == 1) {
        xpathString = ("/gr:cdmGribReaderConfig/gr:variables/gr:parameter/gr:grib1[@indicatorOfParameter='"+type2string(pars.at(0))+"' and @gribTablesVersionNo='"+type2string(pars.at(1))+"' and @identificationOfOriginatingGeneratingCentre='"+type2string(pars.at(2))+"']");
    } else {
        xpathString = ("/gr:cdmGribReaderConfig/gr:variables/gr:parameter/gr:grib2[@parameterNumber='"+type2string(pars.at(0))+"' and @paramterCategory='"+type2string(pars.at(1))+"' and @discipline='"+type2string(pars.at(2))+"']");
    }
    XPathObjPtr xpathObj = doc_->getXPathObject(xpathString);
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size == 1) {
        return nodes->nodeTab[0]->parent; // return the parent, since xpath looks for grib1/2 node
    }
    return 0;
}

void GribCDMReader::initSelectParameters(const std::string& select)
{
    if (select == "all") {
        // nothing to do
    } else if (select == "definedOnly") {
        vector<GribFileMessage> newIndices;
        for (vector<GribFileMessage>::const_iterator gfmIt = indices_.begin(); gfmIt != indices_.end(); ++gfmIt) {
            if (findVariableXMLNode(*gfmIt) != 0) {
                // parameter found
                newIndices.push_back(*gfmIt);
            }
        }
        indices_ = newIndices;
    } else {
        throw runtime_error("unknown select-parameter: " + select);
    }
}


void GribCDMReader::initAddGlobalAttributes()
{
    std::string xpathString("/gr:cdmGribReaderConfig/gr:global_attributes");
    XPathObjPtr xpathObj = doc_->getXPathObject(xpathString);
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size != 1) {
        throw CDMException("unable to find " + xpathString + " in config: " + configFile_);
    }
    for (int i = 0; i < size; ++i) {
        xmlNodePtr node = nodes->nodeTab[i];
        assert(node->type == XML_ELEMENT_NODE);
        std::vector<CDMAttribute> globAttributes;
        fillAttributeListFromXMLNode(globAttributes, nodes->nodeTab[0]->children, templateReplacementAttributes_);
        for (std::vector<CDMAttribute>::iterator it = globAttributes.begin(); it != globAttributes.end(); ++it) {
            cdm_->addAttribute(cdm_->globalAttributeNS(), *it);
        }
    }
    if (! cdm_->checkVariableAttribute(cdm_->globalAttributeNS(), "history", boost::regex(".*"))) {
        boost::posix_time::ptime now(boost::posix_time::second_clock::universal_time());
        cdm_->addAttribute(cdm_->globalAttributeNS(), CDMAttribute("history", boost::gregorian::to_iso_extended_string(now.date()) + " creation by fimex"));
    }
}

/// add the levelOfType for grib-edition edition to the levelDimsOfType, and the levels to the CDM
void GribCDMReader::initLevels(long edition, const map<long, set<long> >& levelsOfType, map<string, CDMDimension>& levelDimsOfType)
{
    for (map<long, set<long> >::const_iterator lit = levelsOfType.begin(); lit != levelsOfType.end(); ++lit) {
        string xpathLevelString("/gr:cdmGribReaderConfig/gr:axes/gr:vertical_axis[@grib"+type2string(edition)+"_id='"+type2string(lit->first)+"']");
        XPathObjPtr xpathObj = doc_->getXPathObject(xpathLevelString);
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        if (size != 1) {
            throw CDMException("unable to find exactly one 'vertical'-axis "+type2string(lit->first)+" in config: " + configFile_ +" and xpath: " + xpathLevelString);
        }
        xmlNodePtr node = nodes->nodeTab[0];
        assert(node->type == XML_ELEMENT_NODE);
        string levelName = getXmlProp(node, "name");
        string levelId = getXmlProp(node, "id");
        string levelType = getXmlProp(node, "type");
        CDMDataType levelDataType = string2datatype(levelType);
        set<long>::size_type levelSize = lit->second.size();
        CDMDimension levelDim(levelId, levelSize);
        levelDimsOfType.insert(std::pair<string, CDMDimension>(type2string(edition)+"_"+type2string(lit->first), levelDim));
        cdm_->addDimension(levelDim);

        // create level variable
        std::vector<std::string> levelShape;
        levelShape.push_back(levelDim.getName());
        CDMVariable levelVar(levelId, levelDataType, levelShape);
        cdm_->addVariable(levelVar);

        // add level variable data
        cdm_->getVariable(levelVar.getName()).setData(createData(CDM_INT, lit->second.begin(), lit->second.end()));
        // enable search of level-information by level-name required by getDataSlice
        for (set<long>::iterator lnit = lit->second.begin(); lnit != lit->second.end(); ++lnit) {
            // add a vector with editon, levelType and levelNumber
            vector<long> v(0,3);
            v.push_back(edition);
            v.push_back(lit->first);
            v.push_back(*lnit);
            levels_[levelVar.getName()].push_back(v);
        }


        // add attributes
        std::vector<CDMAttribute> levelAttributes;
        fillAttributeListFromXMLNode(levelAttributes, nodes->nodeTab[0]->children, templateReplacementAttributes_);
        for (std::vector<CDMAttribute>::iterator ait = levelAttributes.begin(); ait != levelAttributes.end(); ++ait) {
            cdm_->addAttribute(levelVar.getName(), *ait);
        }
    }
}

/// string is gribEdition_levelType, i.e. 2_100
map<string, CDMDimension> GribCDMReader::initAddLevelDimensions()
{
    // level-type mapping, only allow one set of levels per levelType
    map<long, set<long> > levelsOfType1, levelsOfType2;
    for (vector<GribFileMessage>::const_iterator gfmIt = indices_.begin(); gfmIt != indices_.end(); ++gfmIt) {
        if (gfmIt->getEdition() == 1) {
            levelsOfType1[gfmIt->getLevelType()].insert(gfmIt->getLevelNumber());
        } else {
            levelsOfType2[gfmIt->getLevelType()].insert(gfmIt->getLevelNumber());
        }
    }

    map<string, CDMDimension> levelDimsOfType;
    initLevels(1, levelsOfType1, levelDimsOfType);
    initLevels(2, levelsOfType2, levelDimsOfType);

    return levelDimsOfType;
}

void GribCDMReader::initAddTimeDimension()
{
    // get all times, unique and sorted
    {
        set<boost::posix_time::ptime> timesSet;
        for (vector<GribFileMessage>::const_iterator gfmIt = indices_.begin(); gfmIt != indices_.end(); ++gfmIt) {
            timesSet.insert(gfmIt->getDateTime());
        }
        times_ = vector<boost::posix_time::ptime>(timesSet.begin(), timesSet.end());
    }

    XPathObjPtr xpathObj = doc_->getXPathObject("/gr:cdmGribReaderConfig/gr:axes/gr:time");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size != 1) {
        throw CDMException("unable to find exactly 1 'time'-axis in config: " + configFile_);
    }
    xmlNodePtr node = nodes->nodeTab[0];
    assert(node->type == XML_ELEMENT_NODE);
    timeDimName_ = getXmlProp(node, "name");
    string timeType = getXmlProp(node, "type");
    CDMDataType timeDataType = string2datatype(timeType);

    CDMDimension timeDim(timeDimName_, times_.size());
    timeDim.setUnlimited(true);
    cdm_->addDimension(timeDim);
    std::vector<std::string> timeShape;
    timeShape.push_back(timeDim.getName());
    CDMVariable timeVar(timeDimName_, timeDataType, timeShape);
    vector<double> timeVecLong;
    // TODO: this forces times to be seconds since 1970-01-01, maybe I should interpret the config-file unit first
    transform(times_.begin(), times_.end(), back_inserter(timeVecLong), posixTime2epochTime);
    boost::shared_ptr<Data> timeData = createData(timeDataType, timeVecLong.begin(), timeVecLong.end());
    timeVar.setData(timeData);
    cdm_->addVariable(timeVar);
    std::vector<CDMAttribute> timeAttributes;
    fillAttributeListFromXMLNode(timeAttributes, nodes->nodeTab[0]->children, templateReplacementAttributes_);
    for (std::vector<CDMAttribute>::iterator it = timeAttributes.begin(); it != timeAttributes.end(); ++it) {
        cdm_->addAttribute(timeVar.getName(), *it);
    }
}

void GribCDMReader::initAddProjection(std::string& projName, std::string& coordinates)
{
    const GridDefinition& gridDef = indices_[0].getGridDefinition();
    std::string projStr = gridDef.getProjDefinition();
    string gridType = indices_[0].getTypeOfGrid();
    projName = std::string("projection_" + gridType);
    // projection-variable without datatype and dimension
    CDMVariable projVar(projName, CDM_NAT, std::vector<std::string>());
    cdm_->addVariable(projVar);
    std::vector<CDMAttribute> projAttr = projStringToAttributes(projStr);
    for (std::vector<CDMAttribute>::iterator attrIt = projAttr.begin(); attrIt != projAttr.end(); ++attrIt) {
        cdm_->addAttribute(projName, *attrIt);
    }

    {
        // create the x dimension variables and dimensions
        std::string xpathStringX("/gr:cdmGribReaderConfig/gr:axes/gr:spatial_axis[@typeOfGrid='"+gridType+"' and @id='x']");
        std::vector<CDMAttribute> xVarAttributes;
        std::map<string, string> xXmlAttributes;
        int found = readXPathNodeWithCDMAttributes(*doc_, xpathStringX, xXmlAttributes, xVarAttributes, templateReplacementAttributes_);
        if (found != 1) {
            throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringX);
        }
        xDimName_ = xXmlAttributes["name"];
        CDMDimension xDim(xDimName_, gridDef.getXSize());
        CDMDataType xDataType = string2datatype(xXmlAttributes["type"]);
        std::vector<std::string> xDimShape;
        xDimShape.push_back(xDimName_);
        CDMVariable xVar(xDimName_, xDataType, xDimShape);
        vector<double> xData;
        xData.reserve(gridDef.getXSize());
        for (size_t i=0; i < gridDef.getXSize(); i++) {
            xData.push_back(gridDef.getXStart() + i*gridDef.getXIncrement());
        }
        xVar.setData(createData(xDataType, xData.begin(), xData.end()));
        cdm_->addDimension(xDim);
        cdm_->addVariable(xVar);
        for (std::vector<CDMAttribute>::iterator attrIt = xVarAttributes.begin(); attrIt != xVarAttributes.end(); ++attrIt) {
            cdm_->addAttribute(xDimName_, *attrIt);
        }
    }
    {
        // create the y dimension variables and dimensions
        std::string xpathStringY("/gr:cdmGribReaderConfig/gr:axes/gr:spatial_axis[@typeOfGrid='"+gridType+"' and @id='y']");
        std::vector<CDMAttribute> yVarAttributes;
        std::map<string, string> yXmlAttributes;
        int found = readXPathNodeWithCDMAttributes(*doc_, xpathStringY, yXmlAttributes, yVarAttributes, templateReplacementAttributes_);
        if (found != 1) {
            throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringY);
        }
        yDimName_ = yXmlAttributes["name"];
        CDMDimension yDim(yDimName_, gridDef.getYSize());
        CDMDataType yDataType = string2datatype(yXmlAttributes["type"]);
        std::vector<std::string> yDimShape;
        yDimShape.push_back(yDimName_);
        CDMVariable yVar(yDimName_, yDataType, yDimShape);
        vector<double> yData;
        yData.reserve(gridDef.getYSize());
        for (size_t i=0; i < gridDef.getYSize(); i++) {
            yData.push_back(gridDef.getYStart() + i*gridDef.getYIncrement());
        }
        yVar.setData(createData(yDataType, yData.begin(), yData.end()));
        cdm_->addDimension(yDim);
        cdm_->addVariable(yVar);
        for (std::vector<CDMAttribute>::iterator attrIt = yVarAttributes.begin(); attrIt != yVarAttributes.end(); ++attrIt) {
            cdm_->addAttribute(yDimName_, *attrIt);
        }
    }

    std::string longName;
    std::string latName;
    {
        // read longitude and latitude names for projection axes
        std::string xpathStringLong("/gr:cdmGribReaderConfig/gr:axes/gr:spatial_axis[@id='longitude']");
        std::vector<CDMAttribute> lonlatVarAttributes;
        std::map<string, string> lonlatXmlAttributes;
        int found = readXPathNodeWithCDMAttributes(*doc_, xpathStringLong, lonlatXmlAttributes, lonlatVarAttributes, templateReplacementAttributes_);
        if (found != 1) {
            throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringLong);
        }
        longName = lonlatXmlAttributes["name"];
        std::string xpathStringLat("/gr:cdmGribReaderConfig/gr:axes/gr:spatial_axis[@id='latitude']");
        found = readXPathNodeWithCDMAttributes(*doc_, xpathStringLat, lonlatXmlAttributes, lonlatVarAttributes, templateReplacementAttributes_);
        if (found != 1) {
            throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringLat);
        }
        latName = lonlatXmlAttributes["name"];
    }

    // add projection axes 'coordinates = "lon lat";
    if (xDimName_ != longName && yDimName_ != latName) {
        coordinates = longName + " " + latName;
        cdm_->generateProjectionCoordinates(projName, xDimName_, yDimName_, longName, latName);
    }
}

void GribCDMReader::initAddVariables(const std::string& projName, const std::string& coordinates, const map<string, CDMDimension>& levelDimsOfType)
{
    for (vector<GribFileMessage>::const_iterator gfmIt = indices_.begin(); gfmIt != indices_.end(); ++gfmIt) {
        CDMDataType type = CDM_DOUBLE;
        xmlNodePtr node = findVariableXMLNode(*gfmIt);
        string varName;
        if (node == 0) {
            varName = gfmIt->getShortName();
        } else {
            varName = getXmlProp(node, "name");
        }
        if (varName2gribMessages_.find(varName) == varName2gribMessages_.end()) {
            std::vector<CDMAttribute> attributes;
            if (node != 0) {
                fillAttributeListFromXMLNode(attributes, node->children, templateReplacementAttributes_);
            }

            // add the projection
            attributes.push_back(CDMAttribute("grid_mapping",projName));
            if (coordinates != "") {
                attributes.push_back(CDMAttribute("coordinates", coordinates));
            }

            string vectorDirection;
            string vectorCounterpart;
            if (node != 0) {
                // set the datatype if exists
                string stype = getXmlProp(node, "type");
                if (stype != "") {
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

            // map shape, generate variable, set attributes/variable to CDM (fastest moving index (x) first, slowest (unlimited, time) last
             std::vector<std::string> shape;
             shape.push_back(xDimName_);
             shape.push_back(yDimName_);
             string levelDimName = type2string(gfmIt->getEdition()) + "_" + type2string(gfmIt->getLevelType());
             map<string, CDMDimension>::const_iterator levelIt = levelDimsOfType.find(levelDimName);
             if (levelIt != levelDimsOfType.end()) {
                 shape.push_back(levelIt->second.getName());
             }
             shape.push_back(timeDimName_);

             CDMVariable var(varName, type, shape);
             if (vectorCounterpart != "") {
                 var.setAsSpatialVector(vectorCounterpart, vectorDirection);
             }
             cdm_->addVariable(var);
             for (std::vector<CDMAttribute>::const_iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
                 cdm_->addAttribute(varName, *attrIt);
             }


        }
        // add the index
        varName2gribMessages_[varName].push_back(*gfmIt);
    }
}


GribCDMReader::~GribCDMReader()
{
}

boost::shared_ptr<Data> GribCDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        return getDataSliceFromMemory(variable, unLimDimPos);
    }
    // only time can be unLimDim for grib
    if (unLimDimPos >= getData(timeDimName_)->size()) {
        throw CDMException("requested time outside data-region");
    }

    // grib data can be (x,y,level,time) or (x,y,level) or just (x,y)
    // TODO: ensembles?
    const vector<std::string>& dims = variable.getShape();
    const CDMDimension* layerDim = 0;
    size_t xy_size = 1;
    for (vector<std::string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
        CDMDimension& dim = cdm_->getDimension(*it);
        if (dim.getName() != xDimName_ &&
            dim.getName() != yDimName_ &&
            !dim.isUnlimited())
        {
            // level
            layerDim = &dim;
        }
        if (! dim.isUnlimited()) {
            xy_size *= dim.getLength();
        }
    }
    boost::shared_ptr<Data> data;
    std::vector<GribFileMessage> slices;
    // fetch the xy-slices from the grib
    std::map<std::string, std::vector<GribFileMessage> >::const_iterator gribMessagesIt = varName2gribMessages_.find(varName);
    if (gribMessagesIt != varName2gribMessages_.end()) {
        const std::vector<GribFileMessage>& gfmVec = gribMessagesIt->second;
        GribFileMessageEqualTime gfmet(times_.at(unLimDimPos));
        vector<GribFileMessage>::const_iterator gfmIt = find_if(gfmVec.begin(), gfmVec.end(), gfmet);
        if (gfmIt != gfmVec.end()) {
            if (layerDim == 0) {
                // add one xy slice
                slices.push_back(*gfmIt);
            } else {
                // add slice for all levels, search time x
                vector< vector<long> >& levels = levels_[layerDim->getName()];
                for (vector<vector<long> >::iterator lit = levels.begin(); lit != levels.end(); ++lit) {
                    GribFileMessageEqualLevelTime gfmelt(lit->at(0), lit->at(1), lit->at(2), times_.at(unLimDimPos));
                    vector<GribFileMessage>::const_iterator gfmLevelIt = find_if(gfmVec.begin(), gfmVec.end(), gfmelt);
                    if (gfmLevelIt != gfmVec.end()) {
                        slices.push_back(*gfmLevelIt);
                    } else {
                        slices.push_back(GribFileMessage()); // dummy
                    }
                }
            }
        }
    }
    // read data from file
    if (slices.size() == 0) return createData(variable.getDataType(), 0);

    // storage for complete data
    boost::shared_array<double> doubleArray(new double[xy_size]);
    // prefill with missing values
    double missingValue = cdm_->getFillValue(varName);
    fill(&doubleArray[0], &doubleArray[xy_size], missingValue);
    data = boost::shared_ptr<Data>(new DataImpl<double>(doubleArray, xy_size));
    // storage for one layer
    vector<double> gridData(xy_size/slices.size());
    size_t dataCurrentPos = 0;
    for (vector<GribFileMessage>::iterator gfmIt = slices.begin(); gfmIt != slices.end(); ++gfmIt) {
        // join the data of the different levels
        if (gfmIt->isValid()) {
            size_t dataRead = gribDataRead(*gfmIt, gridData, missingValue);
            LOG4FIMEX(logger, Logger::DEBUG, "reading variable " << gfmIt->getShortName() << ", level "<< gfmIt->getLevelNumber() << " size " << dataRead << " starting at " << dataCurrentPos);
            copy(&gridData[0], &gridData[0]+dataRead, &doubleArray[dataCurrentPos]);
        } else {
            LOG4FIMEX(logger, Logger::DEBUG, "skipping variable " << varName << ", 1 level, " << " size " << gridData.size());
        }
        dataCurrentPos += gridData.size(); // always forward a complete slice
    }

    return data;
}


}
#endif /* HAVE_GRIBAPI_H */
