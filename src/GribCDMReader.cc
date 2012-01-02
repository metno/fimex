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
#include <boost/regex.hpp>
#include "fimex/GribCDMReader.h"
#include "fimex/GridDefinition.h"
#include "fimex/GribFileIndex.h"
#include "CDM_XMLConfigHelper.h"
#include "fimex/XMLDoc.h"
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/Data.h"
#include "fimex/Data.h"
#include "fimex/ReplaceStringTimeObject.h"
#include "fimex/coordSys/Projection.h"
#include <set>
#include <algorithm>
#include "../config.h"
#ifdef HAVE_OPENMP
#include <omp.h>
#endif

namespace MetNoFimex
{

using namespace std;

static LoggerPtr logger = getLogger("fimex.GribCDMReader");

struct ProjectionInfo {
    string xDim;
    string yDim;
    string coordinates;
    string gridMapping;
};

struct GribCDMReaderImpl {
    string configId;
    vector<GribFileMessage> indices;
    boost::shared_ptr<XMLDoc> doc;
    map<GridDefinition, ProjectionInfo> gridProjection;
    string timeDimName;
    // store ptimes of all times
    vector<boost::posix_time::ptime> times;
    // store level parameters of level-ids: edition, level-type, level-no
    map<string, vector<vector<long> > > levels;
    /**
     * config attributes may contain template parameters marked with %PARAM%
     * which should be replaced by dynamic values from the grib-file and stored
     * temporary in this map
     *
     * Currently implemented parameters are: %MIN_DATETIME%, %MAX_DATETIME%: earliest and latest time in felt-file as ISO string
     */
    map<string, boost::shared_ptr<ReplaceStringObject> > templateReplacementAttributes;
    /**
     * map from cdm variable names to list of gribMessages
     */
    map<string, vector<GribFileMessage> > varName2gribMessages;
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


GribCDMReader::GribCDMReader(const vector<string>& fileNames, const XMLInput& configXML)
    : p_(new GribCDMReaderImpl())
{
    p_->configId = configXML.id();
    p_->doc = configXML.getXMLDoc();
    p_->doc->registerNamespace("gr", "http://www.met.no/schema/fimex/cdmGribReaderConfig");
    {
        // check config for root element
        XPathObjPtr xpathObj = p_->doc->getXPathObject("/gr:cdmGribReaderConfig");
        size_t rootElements = (xpathObj->nodesetval == 0) ? 0 : xpathObj->nodesetval->nodeNr;
        if (rootElements != 1) throw CDMException("error with rootElement in cdmGribReaderConfig at: " + p_->configId);
    }


    for (vector<string>::const_iterator fileIt = fileNames.begin(); fileIt != fileNames.end(); ++fileIt) {
        vector<GribFileMessage> messages = GribFileIndex(*fileIt).listMessages();
        copy(messages.begin(), messages.end(), back_inserter(p_->indices));
    }

    // select wanted indices from doc, default to all
    {
        XPathObjPtr xpathObj = p_->doc->getXPathObject("/gr:cdmGribReaderConfig/gr:processOptions/gr:option[@name='selectParameters']");
        size_t size = (xpathObj->nodesetval == 0) ? 0 : xpathObj->nodesetval->nodeNr;
        if (size > 0) {
            string select = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            LOG4FIMEX(logger, Logger::DEBUG, "selecting parameters: " << select);
            initSelectParameters(select);
        }
    }


    if (p_->indices.size() != 0) {
        // time-dimension needs to be added before global attributes due to replacements
        initAddTimeDimension();
        // fill templateReplacementAttributes: MIN_DATETIME, MAX_DATETIME
        if (p_->times.size() > 0) {
            p_->templateReplacementAttributes["MIN_DATETIME"] = boost::shared_ptr<ReplaceStringObject>(new ReplaceStringTimeObject(posixTime2epochTime(p_->times.at(0))));
            p_->templateReplacementAttributes["MAX_DATETIME"] = boost::shared_ptr<ReplaceStringObject>(new ReplaceStringTimeObject(posixTime2epochTime(p_->times.at(p_->times.size()-1))));
        }

        initAddGlobalAttributes();
        map<string, CDMDimension> levelDims = initAddLevelDimensions();
        initAddProjection();
        initAddVariables(levelDims);
    }
}

xmlNodePtr GribCDMReader::findVariableXMLNode(const GribFileMessage& msg) const
{
    string xpathString;
    const vector<long>& pars = msg.getParameterIds();
    map<string, long> optionals;
    optionals["typeOfLevel"] = msg.getLevelType();
    if (msg.getEdition() == 1) {
        xpathString = ("/gr:cdmGribReaderConfig/gr:variables/gr:parameter/gr:grib1[@indicatorOfParameter='"+type2string(pars.at(0))+"']");
        optionals["gribTablesVersionNo"] = pars.at(1);
        optionals["identificationOfOriginatingGeneratingCentre"] = pars.at(2);
    } else {
        xpathString = ("/gr:cdmGribReaderConfig/gr:variables/gr:parameter/gr:grib2[@parameterNumber='"+type2string(pars.at(0))+"']");
        optionals["parameterCategory"] = pars.at(1);
        optionals["discipline"] = pars.at(2);
    }
    XPathObjPtr xpathObj = p_->doc->getXPathObject(xpathString);
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    size_t size = (nodes) ? nodes->nodeNr : 0;
    if (size >= 1) {
        LOG4FIMEX(logger, Logger::DEBUG, "found parameter at " << xpathString);
        vector<xmlNodePtr> matchingNodes;
        for (size_t i = 0; i < size; ++i) {
            xmlNodePtr node = nodes->nodeTab[0];
            bool allOptionalsMatch = true;
            for (map<string, long>::iterator opt = optionals.begin(); opt != optionals.end(); ++opt) {
                string optVal = getXmlProp(node, opt->first);
                if (optVal != "") {
                    if (opt->second != string2type<long>(optVal)) {
                        // optional set and not the same as this message value, don't use
                        allOptionalsMatch = false;
                    }
                }
            }
            if (allOptionalsMatch) {
                matchingNodes.push_back(node->parent);
            }
        }

        if (matchingNodes.size() >= 1) {
            if (matchingNodes.size() > 1)  LOG4FIMEX(logger, Logger::WARN, "using first of several parameters for " << xpathString);
            return matchingNodes.at(0); // return the parent, since xpath looks for grib1/2 node
        }
    }
    LOG4FIMEX(logger, Logger::DEBUG, "no parameter found in config for " << xpathString);
    return 0;
}

void GribCDMReader::initSelectParameters(const string& select)
{
    if (select == "all") {
        // nothing to do
    } else if (select == "definedOnly") {
        vector<GribFileMessage> newIndices;
        for (vector<GribFileMessage>::const_iterator gfmIt = p_->indices.begin(); gfmIt != p_->indices.end(); ++gfmIt) {
            if (findVariableXMLNode(*gfmIt) != 0) {
                // parameter found
                newIndices.push_back(*gfmIt);
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
    XPathObjPtr xpathObj = p_->doc->getXPathObject(xpathString);
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size != 1) {
        throw CDMException("unable to find " + xpathString + " in config: " + p_->configId);
    }
    for (int i = 0; i < size; ++i) {
        xmlNodePtr node = nodes->nodeTab[i];
        assert(node->type == XML_ELEMENT_NODE);
        vector<CDMAttribute> globAttributes;
        fillAttributeListFromXMLNode(globAttributes, nodes->nodeTab[0]->children, p_->templateReplacementAttributes);
        for (vector<CDMAttribute>::iterator it = globAttributes.begin(); it != globAttributes.end(); ++it) {
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
        XPathObjPtr xpathObj = p_->doc->getXPathObject(xpathLevelString);
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        if (size != 1) {
            throw CDMException("unable to find exactly one 'vertical'-axis "+type2string(lit->first)+" in config: " + p_->configId +" and xpath: " + xpathLevelString);
        }
        xmlNodePtr node = nodes->nodeTab[0];
        assert(node->type == XML_ELEMENT_NODE);
        string levelName = getXmlProp(node, "name");
        string levelId = getXmlProp(node, "id");
        string levelType = getXmlProp(node, "type");
        CDMDataType levelDataType = string2datatype(levelType);
        set<long>::size_type levelSize = lit->second.size();
        CDMDimension levelDim(levelId, levelSize);
        levelDimsOfType.insert(pair<string, CDMDimension>(type2string(edition)+"_"+type2string(lit->first), levelDim));
        cdm_->addDimension(levelDim);

        // create level variable
        vector<string> levelShape;
        levelShape.push_back(levelDim.getName());
        CDMVariable levelVar(levelId, levelDataType, levelShape);
        cdm_->addVariable(levelVar);

        // add level variable data
        cdm_->getVariable(levelVar.getName()).setData(createData(CDM_INT, lit->second.begin(), lit->second.end()));
        // enable search of level-information by level-name required by getDataSlice
        for (set<long>::const_iterator lnit = lit->second.begin(); lnit != lit->second.end(); ++lnit) {
            // add a vector with editon, levelType and levelNumber
            vector<long> v(0,3);
            v.push_back(edition);
            v.push_back(lit->first);
            v.push_back(*lnit);
            p_->levels[levelVar.getName()].push_back(v);
        }


        // add attributes
        vector<CDMAttribute> levelAttributes;
        fillAttributeListFromXMLNode(levelAttributes, nodes->nodeTab[0]->children, p_->templateReplacementAttributes);

        // add special attributes for grib1 / grib2
        {
            string xpathGribLevelString("gr:grib"+type2string(edition));
            XPathObjPtr xpathObj = p_->doc->getXPathObject(xpathGribLevelString, nodes->nodeTab[0]);
            xmlNodeSetPtr gribNodes = xpathObj->nodesetval;
            int size = (gribNodes) ? gribNodes->nodeNr : 0;
            if (size == 1) {
                fillAttributeListFromXMLNode(levelAttributes, gribNodes->nodeTab[0]->children, p_->templateReplacementAttributes);
            }
        }

        // add the attributes to the CDM
        for (vector<CDMAttribute>::iterator ait = levelAttributes.begin(); ait != levelAttributes.end(); ++ait) {
            cdm_->addAttribute(levelVar.getName(), *ait);
        }
    }
}

/// string is gribEdition_levelType, i.e. 2_100
map<string, CDMDimension> GribCDMReader::initAddLevelDimensions()
{
    // level-type mapping, only allow one set of levels per levelType
    map<long, set<long> > levelsOfType1, levelsOfType2;
    for (vector<GribFileMessage>::const_iterator gfmIt = p_->indices.begin(); gfmIt != p_->indices.end(); ++gfmIt) {
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
        for (vector<GribFileMessage>::const_iterator gfmIt = p_->indices.begin(); gfmIt != p_->indices.end(); ++gfmIt) {
            timesSet.insert(gfmIt->getValidTime());
        }
        p_->times = vector<boost::posix_time::ptime>(timesSet.begin(), timesSet.end());
    }

    XPathObjPtr xpathObj = p_->doc->getXPathObject("/gr:cdmGribReaderConfig/gr:axes/gr:time");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size != 1) {
        throw CDMException("unable to find exactly 1 'time'-axis in config: " + p_->configId);
    }
    xmlNodePtr node = nodes->nodeTab[0];
    assert(node->type == XML_ELEMENT_NODE);
    p_->timeDimName = getXmlProp(node, "name");
    string timeType = getXmlProp(node, "type");
    CDMDataType timeDataType = string2datatype(timeType);

    CDMDimension timeDim(p_->timeDimName, p_->times.size());
    timeDim.setUnlimited(true);
    cdm_->addDimension(timeDim);
    vector<string> timeShape;
    timeShape.push_back(timeDim.getName());
    CDMVariable timeVar(p_->timeDimName, timeDataType, timeShape);
    vector<double> timeVecLong;
    // TODO: this forces times to be seconds since 1970-01-01, maybe I should interpret the config-file unit first
    transform(p_->times.begin(), p_->times.end(), back_inserter(timeVecLong), posixTime2epochTime);
    boost::shared_ptr<Data> timeData = createData(timeDataType, timeVecLong.begin(), timeVecLong.end());
    timeVar.setData(timeData);
    cdm_->addVariable(timeVar);
    vector<CDMAttribute> timeAttributes;
    fillAttributeListFromXMLNode(timeAttributes, nodes->nodeTab[0]->children, p_->templateReplacementAttributes);
    for (vector<CDMAttribute>::iterator it = timeAttributes.begin(); it != timeAttributes.end(); ++it) {
        cdm_->addAttribute(timeVar.getName(), *it);
    }

    // TODO check if reference time changes, assuming they are all alike
    boost::posix_time::ptime refTime = p_->indices.begin()->getReferenceTime();
    // TODO: move reference time name to config
    string referenceTime = "forecast_reference_time";
    vector<string> nullShape;
    CDMVariable refTimeVar(referenceTime, timeDataType, nullShape);
    boost::shared_ptr<Data> refTimeData = createData(timeDataType, 1);
    // TODO: this forces times to be seconds since 1970-01-01, maybe I should interpret the config-file unit first
    refTimeData->setValue(0, posixTime2epochTime(refTime));
    refTimeVar.setData(refTimeData);
    cdm_->addVariable(refTimeVar);
    cdm_->addAttribute(referenceTime, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
    cdm_->addAttribute(referenceTime, CDMAttribute("standard_name", "forecast_reference_time"));


}

void GribCDMReader::initAddProjection()
{
    // gridDefinition -> gridType
    map<GridDefinition, string> gridDefs;
    for (vector<GribFileMessage>::const_iterator idx = p_->indices.begin(); idx != p_->indices.end(); ++idx) {
        gridDefs[idx->getGridDefinition()] = idx->getTypeOfGrid();
        LOG4FIMEX(logger, Logger::DEBUG,"found gridDef:" << idx->getGridDefinition().id());
    }
    assert(!gridDefs.empty());

    int lastXYId = 0;
    for (map<GridDefinition, string>::iterator gd = gridDefs.begin(); gd != gridDefs.end(); gd++, lastXYId++) {
        const GridDefinition& gridDef(gd->first);
        LOG4FIMEX(logger, Logger::DEBUG,"adding gridDef:" << gridDef.id());
        string& gridType(gd->second);
        string projStr = gridDef.getProjDefinition();
        ProjectionInfo pi;

        string appendix;
        if (lastXYId > 0) {
            appendix = type2string(lastXYId);
        }

        pi.gridMapping = string("projection_" + gridType + appendix);
        // projection-variable without datatype and dimension
        CDMVariable projVar(pi.gridMapping, CDM_NAT, vector<string>());
        cdm_->addVariable(projVar);
        vector<CDMAttribute> projAttr =  Projection::createByProj4(projStr)->getParameters();
        for (vector<CDMAttribute>::iterator attrIt = projAttr.begin(); attrIt != projAttr.end(); ++attrIt) {
            cdm_->addAttribute(pi.gridMapping, *attrIt);
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
            vector<string> xDimShape;
            xDimShape.push_back(pi.xDim);
            CDMVariable xVar(pi.xDim, xDataType, xDimShape);
            vector<double> xData;
            xData.reserve(gridDef.getXSize());
            for (size_t i=0; i < gridDef.getXSize(); i++) {
                xData.push_back(gridDef.getXStart() + i*gridDef.getXIncrement());
            }
            xVar.setData(createData(xDataType, xData.begin(), xData.end()));
            cdm_->addDimension(xDim);
            cdm_->addVariable(xVar);
            for (vector<CDMAttribute>::iterator attrIt = xVarAttributes.begin(); attrIt != xVarAttributes.end(); ++attrIt) {
                cdm_->addAttribute(pi.xDim, *attrIt);
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
            vector<string> yDimShape;
            yDimShape.push_back(pi.yDim);
            CDMVariable yVar(pi.yDim, yDataType, yDimShape);
            vector<double> yData;
            yData.reserve(gridDef.getYSize());
            for (size_t i=0; i < gridDef.getYSize(); i++) {
                yData.push_back(gridDef.getYStart() + i*gridDef.getYIncrement());
            }
            yVar.setData(createData(yDataType, yData.begin(), yData.end()));
            cdm_->addDimension(yDim);
            cdm_->addVariable(yVar);
            for (vector<CDMAttribute>::iterator attrIt = yVarAttributes.begin(); attrIt != yVarAttributes.end(); ++attrIt) {
                cdm_->addAttribute(pi.yDim, *attrIt);
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
            latName = lonlatXmlAttributes["name"];
        }

        // add projection axes 'coordinates = "lon lat";
        if (pi.xDim != longName && pi.yDim != latName) {
            pi.coordinates = longName + " " + latName;
            cdm_->generateProjectionCoordinates(pi.gridMapping, pi.xDim, pi.yDim, longName, latName);
        }
        p_->gridProjection[gridDef] = pi;
    }
}

void GribCDMReader::initAddVariables(const map<string, CDMDimension>& levelDimsOfType)
{
    for (vector<GribFileMessage>::const_iterator gfmIt = p_->indices.begin(); gfmIt != p_->indices.end(); ++gfmIt) {
        const ProjectionInfo pi = p_->gridProjection[gfmIt->getGridDefinition()];
        assert(pi.xDim != "");
        CDMDataType type = CDM_DOUBLE;
        xmlNodePtr node = findVariableXMLNode(*gfmIt);
        string varName;
        if (node == 0) {
            // prepend names from grib-api with 'ga_'
            // since they might otherwise start numerical, which is against CF, and buggy in netcdf 3.6.3, 4.0.*
            varName = "ga_" + gfmIt->getShortName() + "_" + type2string(gfmIt->getLevelType());
        } else {
            varName = getXmlProp(node, "name");
        }
        if (p_->varName2gribMessages.find(varName) == p_->varName2gribMessages.end()) {
            vector<CDMAttribute> attributes;
            if (node != 0) {
                fillAttributeListFromXMLNode(attributes, node->children, p_->templateReplacementAttributes);
            }

            // add the projection
            attributes.push_back(CDMAttribute("grid_mapping",pi.gridMapping));
            if (pi.coordinates != "") {
                attributes.push_back(CDMAttribute("coordinates", pi.coordinates));
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
             vector<string> shape;
             shape.push_back(pi.xDim);
             shape.push_back(pi.yDim);
             string levelDimName = type2string(gfmIt->getEdition()) + "_" + type2string(gfmIt->getLevelType());
             map<string, CDMDimension>::const_iterator levelIt = levelDimsOfType.find(levelDimName);
             if (levelIt != levelDimsOfType.end()) {
                 shape.push_back(levelIt->second.getName());
             }
             shape.push_back(p_->timeDimName);

             CDMVariable var(varName, type, shape);
             if (vectorCounterpart != "") {
                 var.setAsSpatialVector(vectorCounterpart, vectorDirection);
             }
             cdm_->addVariable(var);
             for (vector<CDMAttribute>::const_iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
                 cdm_->addAttribute(varName, *attrIt);
             }


        }
        // add the index
        p_->varName2gribMessages[varName].push_back(*gfmIt);
    }
}


GribCDMReader::~GribCDMReader()
{
}

boost::shared_ptr<Data> GribCDMReader::getDataSlice(const string& varName, size_t unLimDimPos)
{
    const CDMVariable& variable = cdm_->getVariable(varName);

    if (variable.getDataType() == CDM_NAT) {
        return createData(CDM_INT,0); // empty
    }
    if (variable.hasData()) {
        return getDataSliceFromMemory(variable, unLimDimPos);
    }
    // only time can be unLimDim for grib
    if (unLimDimPos >= getData(p_->timeDimName)->size()) {
        throw CDMException("requested time outside data-region");
    }

    vector<GribFileMessage> slices;
    map<string, vector<GribFileMessage> >::const_iterator gribMessagesIt = p_->varName2gribMessages.find(varName);
    if (gribMessagesIt == p_->varName2gribMessages.end()) {
        throw CDMException("no grib message found for variable '" + varName + "'");
    }


    const vector<GribFileMessage>& gfmVec = gribMessagesIt->second;
    // fetch the xy-slices from the grib
    const GridDefinition varGridDef = gribMessagesIt->second.begin()->getGridDefinition();
    assert(gribMessagesIt->second.begin() != gribMessagesIt->second.end());
    ProjectionInfo pi = p_->gridProjection[varGridDef];
    assert(pi.xDim != "");

    // grib data can be (x,y,level,time) or (x,y,level) or just (x,y)
    // TODO: ensembles?
    const vector<string>& dims = variable.getShape();
    const CDMDimension* layerDim = 0;
    size_t xy_size = 1;
    for (vector<string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
        CDMDimension& dim = cdm_->getDimension(*it);
        if (dim.getName() != pi.xDim &&
            dim.getName() != pi.yDim &&
            !dim.isUnlimited())
        {
            // level
            layerDim = &dim;
        }
        if (! dim.isUnlimited()) {
            xy_size *= dim.getLength();
        }
    }

    GribFileMessageEqualTime gfmet(p_->times.at(unLimDimPos));
    vector<GribFileMessage>::const_iterator gfmIt = find_if(gfmVec.begin(), gfmVec.end(), gfmet);
    if (gfmIt != gfmVec.end()) {
        if (layerDim == 0) {
            // add one xy slice
            slices.push_back(*gfmIt);
        } else {
            // add slice for all levels, search time x
            vector< vector<long> >& levels = p_->levels[layerDim->getName()];
            for (vector<vector<long> >::iterator lit = levels.begin(); lit != levels.end(); ++lit) {
                GribFileMessageEqualLevelTime gfmelt(lit->at(0), lit->at(1), lit->at(2), p_->times.at(unLimDimPos));
                vector<GribFileMessage>::const_iterator gfmLevelIt = find_if(gfmVec.begin(), gfmVec.end(), gfmelt);
                if (gfmLevelIt != gfmVec.end()) {
                    slices.push_back(*gfmLevelIt);
                } else {
                    slices.push_back(GribFileMessage()); // dummy
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
    boost::shared_ptr<Data> data = createData(xy_size, doubleArray);
    // storage for one layer
    vector<double> gridData(xy_size/slices.size());
    size_t dataCurrentPos = 0;
    for (vector<GribFileMessage>::iterator gfmIt = slices.begin(); gfmIt != slices.end(); ++gfmIt) {
        // join the data of the different levels
        if (gfmIt->isValid()) {
            boost::shared_ptr<Data> data;
            size_t dataRead;
            #ifdef HAVE_OPENMP
            #pragma omp critical (mifi_gribcdmreader)
            {
            #endif
            dataRead = gfmIt->readData(gridData, missingValue);
            #ifdef HAVE_OPENMP
            }
            #endif

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
