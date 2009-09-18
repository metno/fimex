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

#include "fimex/CDM.h"
#include "fimex/GribCDMReader.h"
#include "fimex/GridDefinition.h"
#include "fimex/GribFileIndex.h"
#include "fimex/CDM_XMLConfigHelper.h"
#include "fimex/XMLDoc.h"
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/Data.h"
#include "fimex/DataImpl.h"
#include <algorithm>

namespace MetNoFimex
{

using namespace std;

static LoggerPtr logger = getLogger("fimex.GribCDMReader");

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
        XPathObjPtr xpathObj = doc_->getXPathObject("/gr:cdmGribReaderConfig/gr:processOptions/gr:option[name='selectParameters']");
        size_t size = (xpathObj->nodesetval == 0) ? 0 : xpathObj->nodesetval->nodeNr;
        if (size > 0) {
            initSelectParamters(getXmlProp(xpathObj->nodesetval->nodeTab[0], "value"));
        }
    }


    if (indices_.size() == 0) return;

    // currently only one gridDefinition for all paramters supported
    // search indices for params with same gridDefinition (size,start,incr)
    double gridDefinitionDelta = 0.01;
    {
        XPathObjPtr xpathObj = doc_->getXPathObject("/gr:cdmGribReaderConfig/gr:processOptions/gr:option[name='gridDefinitionDelta']");
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

    initAddGlobalAttributes();
    CDMDimension timeDim = initAddTimeDimension();
    map<string, CDMDimension> levelDims = initAddLevelDimensions();
    string projName, coordinates;
    initAddProjection(projName, coordinates);
    initAddVariables(projName, coordinates, timeDim, levelDims);
}

void GribCDMReader::initSelectParamters(const std::string& select)
{
    if (select == "all") {
        // nothing to do
    } else if (select == "definedOnly") {
        vector<GribFileMessage> newIndices;
        for (vector<GribFileMessage>::const_iterator gfmIt = indices_.begin(); gfmIt != indices_.end(); ++gfmIt) {
            string xpathString;
            const vector<long>& pars = gfmIt->getParameterIds();
            if (gfmIt->getEdition() == 1) {
                xpathString = ("/gr:cdmGribReaderConfig/gr:variables/gr:parameter/gr:grib1[@indicatorOfParameter='"+type2string(pars.at(0))+"' and @gribTablesVersionNo='"+type2string(pars.at(1))+"' and @identificationOfOriginatingGeneratingCentre='"+type2string(pars.at(2))+"']");
            } else {
                xpathString = ("/gr:cdmGribReaderConfig/gr:variables/gr:parameter/gr:grib2[@parameterNumber='"+type2string(pars.at(0))+"' and @paramterCategory='"+type2string(pars.at(1))+"' and @discipline='"+type2string(pars.at(2))+"']");
            }
            XPathObjPtr xpathObj = doc_->getXPathObject(xpathString);
            xmlNodeSetPtr nodes = xpathObj->nodesetval;
            int size = (nodes) ? nodes->nodeNr : 0;
            if (size == 1) {
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
        string xpathLevelString("/gr:cdmGribReaderConfig/gr:axes/gr:vertical_axis[grib"+type2string(edition)+"_id='"+type2string(lit->first)+"']");
        XPathObjPtr xpathObj = doc_->getXPathObject(xpathLevelString);
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        if (size != 1) {
            throw CDMException("unable to find 'vertical'-axis "+type2string(lit->first)+" in config: " + configFile_);
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

        // create level variable without data!
        std::vector<std::string> levelShape;
        levelShape.push_back(levelDim.getName());
        CDMVariable levelVar(levelId, levelDataType, levelShape);
        cdm_->addVariable(levelVar);

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

CDMDimension GribCDMReader::initAddTimeDimension()
{
    // get all times, unique and sorted
    vector<boost::posix_time::ptime> times;
    {
        set<boost::posix_time::ptime> timesSet;
        for (vector<GribFileMessage>::const_iterator gfmIt = indices_.begin(); gfmIt != indices_.end(); ++gfmIt) {
            timesSet.insert(gfmIt->getDateTime());
        }
        times = vector<boost::posix_time::ptime>(timesSet.begin(), timesSet.end());
    }

    XPathObjPtr xpathObj = doc_->getXPathObject("/gr:cdmGribReaderConfig/gr:axes/gr:time");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size != 1) {
        throw CDMException("unable to find exactly 1 'time'-axis in config: " + configFile_);
    }
    xmlNodePtr node = nodes->nodeTab[0];
    assert(node->type == XML_ELEMENT_NODE);
    string timeName = getXmlProp(node, "name");
    string timeType = getXmlProp(node, "type");
    CDMDataType timeDataType = string2datatype(timeType);

    CDMDimension timeDim(timeName, times.size());
    timeDim.setUnlimited(true);
    cdm_->addDimension(timeDim);
    std::vector<std::string> timeShape;
    timeShape.push_back(timeDim.getName());
    CDMVariable timeVar(timeName, timeDataType, timeShape);
    vector<double> timeVecLong;
    // TODO: this forces times to be seconds since 1970-01-01, maybe I should interpret the config-file unit first
    transform(times.begin(), times.end(), back_inserter(timeVecLong), posixTime2epochTime);
    boost::shared_ptr<Data> timeData = createData(timeDataType, timeVecLong.begin(), timeVecLong.end());
    timeVar.setData(timeData);
    cdm_->addVariable(timeVar);
    std::vector<CDMAttribute> timeAttributes;
    fillAttributeListFromXMLNode(timeAttributes, nodes->nodeTab[0]->children, templateReplacementAttributes_);
    for (std::vector<CDMAttribute>::iterator it = timeAttributes.begin(); it != timeAttributes.end(); ++it) {
        cdm_->addAttribute(timeVar.getName(), *it);
    }

    return timeDim;
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
        std::string xName(xXmlAttributes["name"]);
        xDim_ = CDMDimension(xName, gridDef.getXSize());
        CDMDataType xDataType = string2datatype(xXmlAttributes["type"]);
        std::vector<std::string> xDimShape;
        xDimShape.push_back(xDim_.getName());
        CDMVariable xVar(xName, xDataType, xDimShape);
        vector<double> xData;
        xData.reserve(gridDef.getXSize());
        for (size_t i=0; i < gridDef.getXSize(); i++) {
            xData.push_back(gridDef.getXStart() + i*gridDef.getXIncrement());
        }
        xVar.setData(createData(xDataType, xData.begin(), xData.end()));
        cdm_->addDimension(xDim_);
        cdm_->addVariable(xVar);
        for (std::vector<CDMAttribute>::iterator attrIt = xVarAttributes.begin(); attrIt != xVarAttributes.end(); ++attrIt) {
            cdm_->addAttribute(xName, *attrIt);
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
        std::string yName(yXmlAttributes["name"]);
        yDim_ = CDMDimension(yName, gridDef.getYSize());
        CDMDataType yDataType = string2datatype(yXmlAttributes["type"]);
        std::vector<std::string> yDimShape;
        yDimShape.push_back(yDim_.getName());
        CDMVariable yVar(yName, yDataType, yDimShape);
        vector<double> yData;
        yData.reserve(gridDef.getYSize());
        for (size_t i=0; i < gridDef.getYSize(); i++) {
            yData.push_back(gridDef.getYStart() + i*gridDef.getYIncrement());
        }
        yVar.setData(createData(yDataType, yData.begin(), yData.end()));
        cdm_->addDimension(yDim_);
        cdm_->addVariable(yVar);
        for (std::vector<CDMAttribute>::iterator attrIt = yVarAttributes.begin(); attrIt != yVarAttributes.end(); ++attrIt) {
            cdm_->addAttribute(yName, *attrIt);
        }
    }

    std::string longName;
    std::string latName;
    {
        // read longitude and latitude names for projection axes
        std::string xpathStringLong("/gr:cdmGridReaderConfig/gr:axes/gr:spatial_axis[@id='longitude']");
        std::vector<CDMAttribute> lonlatVarAttributes;
        std::map<string, string> lonlatXmlAttributes;
        int found = readXPathNodeWithCDMAttributes(*doc_, xpathStringLong, lonlatXmlAttributes, lonlatVarAttributes, templateReplacementAttributes_);
        if (found != 1) {
            throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringLong);
        }
        longName = lonlatXmlAttributes["name"];
        std::string xpathStringLat("/gr:cdmGridReaderConfig/gr:axes/gr:spatial_axis[@id='latitude']");
        found = readXPathNodeWithCDMAttributes(*doc_, xpathStringLat, lonlatXmlAttributes, lonlatVarAttributes, templateReplacementAttributes_);
        if (found != 1) {
            throw CDMException("error in config-file: not exactly 1 entry for xpath: " + xpathStringLat);
        }
        latName = lonlatXmlAttributes["name"];
    }

    // add projection axes 'coordinates = "lon lat";
    if (xDim_.getName() != longName && yDim_.getName() != latName) {
        coordinates = longName + " " + latName;
        cdm_->generateProjectionCoordinates(projName, xDim_.getName(), yDim_.getName(), longName, latName);
    }
}

void GribCDMReader::initAddVariables(const std::string& projName, const std::string& coordinates, const CDMDimension& timeDim, const map<string, CDMDimension>& levelDims)
{
    for (vector<GribFileMessage>::const_iterator gfmIt = indices_.begin(); gfmIt != indices_.end(); ++gfmIt) {

    }
}


GribCDMReader::~GribCDMReader()
{
}

boost::shared_ptr<Data> GribCDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
    // TODO: implement
}


}
