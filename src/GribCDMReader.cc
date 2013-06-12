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
#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/GribCDMReader.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/GridDefinition.h"
#include "fimex/GribFileIndex.h"
#include "CDM_XMLConfigHelper.h"
#include "fimex/XMLDoc.h"
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/Data.h"
#include "fimex/Data.h"
#include "fimex/ReplaceStringTimeObject.h"
#include "fimex/ReplaceStringTemplateObject.h"
#include "fimex/coordSys/Projection.h"
#include <set>
#include <map>
#include <algorithm>
#include <stdexcept>
#include <MutexLock.h>

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
    MutexType mutex;
    map<GridDefinition, ProjectionInfo> gridProjection;
    string timeDimName;
    string ensembleDimName;
    vector<pair<string, boost::regex> > ensembleMemberIds;
    size_t maxEnsembles;
    // store ptimes of all times
    vector<boost::posix_time::ptime> times;

    // varName -> time (unlimDimPos) -> level (val) -> ensemble (val) -> GFI (index)
    map<string, map<size_t, map<long, map<size_t, size_t> > > > varTimeLevelEnsembleGFIBox;
    // varName -> has Ensemble
    map<string, bool> varHasEnsemble;

    // list of different vectors per edition _ typeOfLevel
    map<string, vector<vector<long> > > levelValsOfType;
    // edition_typeOfLevel -> [ dimensionName ]
    map<string, vector<string> > levelDimNames;
    // varName -> (edition_levelType, position n levelValsOfType
    map<string, pair<string, size_t> > varLevelTypePos;

    /**
     * config attributes may contain template parameters marked with %PARAM%
     * which should be replaced by dynamic values from the grib-file and stored
     * temporary in this map
     *
     * Currently implemented parameters are: %MIN_DATETIME%, %MAX_DATETIME%: earliest and latest time in felt-file as ISO string
     */
    map<string, boost::shared_ptr<ReplaceStringObject> > templateReplacementAttributes;
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


GribCDMReader::GribCDMReader(const vector<string>& fileNames, const XMLInput& configXML, const std::vector<std::pair<std::string, std::string> >& members)
    : p_(new GribCDMReaderImpl())
{
    for (vector<pair<string, string> >::const_iterator memIt = members.begin(); memIt != members.end(); ++memIt) {
        p_->ensembleMemberIds.push_back(make_pair(memIt->first, boost::regex(memIt->second)));
    }
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
        vector<GribFileMessage> messages = GribFileIndex(*fileIt, p_->ensembleMemberIds).listMessages();
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
        initCreateGFIBoxes();
        initAddEnsembles();
        initLevels();
        initAddProjection();
        initAddVariables();
    }
}

xmlNodePtr GribCDMReader::findVariableXMLNode(const GribFileMessage& msg) const
{
    string xpathString;
    const vector<long>& pars = msg.getParameterIds();
    map<string, long> optionals;
    optionals["typeOfLevel"] = msg.getLevelType();
    optionals["levelNo"] = msg.getLevelNumber();
    optionals["timeRangeIndicator"] = msg.getTimeRangeIndicator();
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
            xmlNodePtr node = nodes->nodeTab[i];
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
void GribCDMReader::initLevels()
{
    for (map<string, vector<vector<long> > >::const_iterator lit = p_->levelValsOfType.begin(); lit != p_->levelValsOfType.end(); ++lit) {
        vector<string> editionType = tokenize(lit->first, "_");
        long edition = string2type<long>(editionType.at(0));
        long typeId = string2type<long>(editionType.at(1));
        string xpathLevelString("/gr:cdmGribReaderConfig/gr:axes/gr:vertical_axis[@grib"+type2string(edition)+"_id='"+type2string(typeId)+"']");
        XPathObjPtr xpathObj = p_->doc->getXPathObject(xpathLevelString);
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        if (size > 1) {
            throw CDMException("more than one 'vertical'-axis "+type2string(typeId)+" in config: " + p_->configId +" and xpath: " + xpathLevelString);
        }
        string levelName, levelId, levelType;
        xmlNodePtr node;
        if (size == 0) {
            LOG4FIMEX(logger, Logger::INFO, "no definition for vertical axis " + type2string(typeId) + " found, using default");
            levelName = "grib"+type2string(edition) +"_vLevel" + type2string(typeId);
            levelId = levelName;
            levelType = "float";
        } else {
            node = nodes->nodeTab[0];
            assert(node->type == XML_ELEMENT_NODE);
            levelName = getXmlProp(node, "name");
            levelId = getXmlProp(node, "id");
            levelType = getXmlProp(node, "type");
        }
        CDMDataType levelDataType = string2datatype(levelType);
        vector<string> dimNames(lit->second.size());
        for (size_t i = 0; i < lit->second.size(); ++i) {
            string myExtension = (lit->second.size() > 1  ? type2string(i) : "");
            string myLevelId = levelId + myExtension;
            dimNames.at(i) = myLevelId;
            LOG4FIMEX(logger, Logger::DEBUG, "declaring level: " << myLevelId);
            set<long>::size_type levelSize = lit->second.at(i).size();
            CDMDimension levelDim(myLevelId, levelSize);
            cdm_->addDimension(levelDim);

            // create level variable
            vector<string> levelShape;
            levelShape.push_back(levelDim.getName());
            CDMVariable levelVar(levelDim.getName(), levelDataType, levelShape);
            cdm_->addVariable(levelVar);

            // add level variable data
            DataPtr levelData = createData(CDM_INT, lit->second.at(i).begin(), lit->second.at(i).end());
            // add special data for hybrid levels
            if (node != 0) {
                initSpecialLevels_(node, myExtension, lit->first, i, levelShape, levelData);
            }
            cdm_->getVariable(levelVar.getName()).setData(levelData);

            // add attributes
            vector<CDMAttribute> levelAttributes;
            if (node != 0) {
                map<string, boost::shared_ptr<ReplaceStringObject> > replacements;
                replacements["EXT"] = boost::shared_ptr<ReplaceStringObject>(new ReplaceStringTemplateObject<string>(myExtension));
                fillAttributeListFromXMLNode(levelAttributes, nodes->nodeTab[0]->children, replacements);

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
            } else {
                levelAttributes.push_back(CDMAttribute("units", "1"));
                levelAttributes.push_back(CDMAttribute("long_name", "grib"+type2string(edition)+ "-level " + type2string(typeId)));
            }

            // add the attributes to the CDM
            for (vector<CDMAttribute>::iterator ait = levelAttributes.begin(); ait != levelAttributes.end(); ++ait) {
                cdm_->addAttribute(levelVar.getName(), *ait);
            }
        }
        p_->levelDimNames[lit->first] = dimNames;
    }
}


vector<double> GribCDMReader::readVarPv_(string exampleVar)
{
    vector<double> pv;
    // example gribFileMessage
    size_t gfiPos = p_->varTimeLevelEnsembleGFIBox[exampleVar].begin()->second.begin()->second.begin()->second;
    //
    size_t count = p_->indices.at(gfiPos).readLevelData(pv, MIFI_FILL_DOUBLE);
    if (count <= 0) {
        LOG4FIMEX(logger, Logger::INFO, "could not find extra level data (PV)");
    }
    return pv;
}

vector<double> GribCDMReader::readValuesFromXPath_(xmlNodePtr node, DataPtr levelData, string exampleVar, string extension)
{
    vector<double> retValues;
    string valuesXPath("./gr:values");
    XPathObjPtr xpathObj = p_->doc->getXPathObject(valuesXPath, node);
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        xmlNodePtr node = nodes->nodeTab[i];
        if (node->type == XML_ELEMENT_NODE) {
            string mode = getXmlProp(node, "mode");
            if (mode == "" || mode == "inline") {
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
                boost::shared_array<unsigned int> lv = levelData->asUInt();
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
                vector<double> pv = readVarPv_(exampleVar);
                size_t offset = (mode == "extraHalvLevel1") ? 0 : pv.size()/2;
                boost::shared_array<unsigned int> lv = levelData->asUInt();
                for (size_t i = 0; i < levelData->size(); i++) {
                    size_t pos = offset + lv[i];
                    if (pos < pv.size()) {
                        double value = (pos == 0) ? pv.at(0) : (pv[pos] + pv[pos-1])/2;
                        retValues.push_back(value);
                    } else {
                        throw CDMException("levelData (pv) of " + exampleVar + " has not enough elements, need " + type2string(pos+1));
                    }
                }
            } else if (mode == "hybridSigmaCalc(ap,b,p0)") {
                // fetch ap, b, and calc
                const CDMVariable& p0 = getCDM().getVariable("p0"+ extension);
                const CDMVariable& ap = getCDM().getVariable("ap" + extension);
                const CDMVariable& b = getCDM().getVariable("b" + extension);
                boost::shared_array<double> p0Data = p0.getData()->asDouble();
                boost::shared_array<double> apData = ap.getData()->asDouble();
                boost::shared_array<double> bData = b.getData()->asDouble();
                for (size_t i = 0; i < ap.getData()->size(); ++i) {
                    retValues.push_back(apData[i]/p0Data[0] + bData[i]);
                }
            } else {
                throw CDMException("unkown mode '"+ mode +"' to extract level-data for variable " + exampleVar);
            }
            string sscale = getXmlProp(node, "scale_factor");
            if (sscale != "") {
                double scale = string2type<double>(sscale);
                transform(retValues.begin(), retValues.end(),
                        retValues.begin(), bind1st(multiplies<double>(), scale));
            }
        }
    }
    if (size == 0) {
        boost::shared_array<double> d = levelData->asDouble();
        retValues = vector<double>(&d[0], &d[0]+levelData->size());
    }
    return retValues;
}

void GribCDMReader::initSpecialLevels_(xmlNodePtr node, const string& extension, const string& levelType, size_t levelPos, const vector<string>& levelShape, DataPtr& levelData) {
    string xpathAdditional = "gr:additional_axis_variable";
    XPathObjPtr xpathObj = p_->doc->getXPathObject(xpathAdditional, node);
    xmlNodeSetPtr addNodes = xpathObj->nodesetval;
    int size = (addNodes) ? addNodes->nodeNr : 0;
    if (size > 0) {
        string exampleVar;
        // find example variable
        for (map<string, pair<string,size_t> >::iterator vltp = p_->varLevelTypePos.begin(); vltp != p_->varLevelTypePos.end(); vltp++) {
            string vltype = vltp->second.first;
            size_t vltpos = vltp->second.second;
            if ((levelType == vltype) && (levelPos == vltpos)) {
                exampleVar = vltp->first;
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
            if (axis != "" && axis != "1") {
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
            for (vector<CDMAttribute>::iterator ait = levelAttributes.begin(); ait != levelAttributes.end(); ++ait) {
                cdm_->addAttribute(name, *ait);
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
    DataPtr timeData = createData(timeDataType, timeVecLong.begin(), timeVecLong.end());
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
    DataPtr refTimeData = createData(timeDataType, 1);
    // TODO: this forces times to be seconds since 1970-01-01, maybe I should interpret the config-file unit first
    refTimeData->setValue(0, posixTime2epochTime(refTime));
    refTimeVar.setData(refTimeData);
    cdm_->addVariable(refTimeVar);
    cdm_->addAttribute(referenceTime, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
    cdm_->addAttribute(referenceTime, CDMAttribute("standard_name", "forecast_reference_time"));


}

string GribCDMReader::getVariableName(const GribFileMessage& gfm) const
{
    xmlNodePtr node = findVariableXMLNode(gfm);
    string varName;
    if (node == 0) {
        // prepend names from grib-api with 'ga_'
        // since they might otherwise start numerical, which is against CF, and buggy in netcdf 3.6.3, 4.0.*
        varName = "ga_" + gfm.getShortName() + "_" + type2string(gfm.getLevelType());
    } else {
        varName = getXmlProp(node, "name");
    }
    return varName;
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
    for (vector<GribFileMessage>::const_iterator gfmIt = p_->indices.begin(); gfmIt != p_->indices.end(); ++gfmIt, ++pos) {
        string varName = getVariableName(*gfmIt);
        vector<boost::posix_time::ptime>::iterator pTimesIt = find(p_->times.begin(), p_->times.end(), gfmIt->getValidTime());
        assert(pTimesIt != p_->times.end());
        size_t unlimDimPos = distance(p_->times.begin(), pTimesIt);

        bool hasEnsemble;
        if (gfmIt->getTotalNumberOfEnsembles() > 1) {
            hasEnsemble = true;
            p_->maxEnsembles = std::max(p_->maxEnsembles, gfmIt->getPerturbationNumber()+1); // gfmIt->getTotalNumberOfEnsembles() is often wrong
            // perturbation number start at 0
            if (gfmIt->getPerturbationNumber() >= p_->maxEnsembles) {
                throw CDMException("grib-perturbation number " + type2string(gfmIt->getPerturbationNumber()) + " larger than max-ensembles: " + type2string(p_->maxEnsembles));
            }
        } else {
            hasEnsemble = false;
        }
        if (p_->varHasEnsemble.find(varName) != p_->varHasEnsemble.end() && p_->varHasEnsemble[varName] != hasEnsemble) {
            throw CDMException("grib-variable " + varName + " has messages within ensembles, and outside: fimex can't proceed");
        }
        p_->varHasEnsemble[varName] = hasEnsemble;

        // varName -> time (unlimDimPos) -> level (val) -> ensemble (val) -> GFI (index)
        //map<string, map<size_t, map<long, map<size_t, size_t> > > > varTimeLevelEnsembleGFIBox;
        p_->varTimeLevelEnsembleGFIBox[varName][unlimDimPos][gfmIt->getLevelNumber()][gfmIt->getPerturbationNumber()] = pos;

        // remember level and levelType
        varLevels[varName].insert(gfmIt->getLevelNumber());
        string levelType = type2string(gfmIt->getEdition()) + "_" + type2string(gfmIt->getLevelType());
        if (varLevelType.find(varName) == varLevelType.end()) {
            varLevelType[varName] = levelType;
        } else if (varLevelType[varName] != levelType){
            throw CDMException("typeOfLevel change for variable " + varName + " from " + varLevelType[varName] + " to " + levelType);
        }
    }

    // map from variableName to levelType and position in levelValsOfType
    for (map<string, string>::iterator varTypeIt = varLevelType.begin(); varTypeIt != varLevelType.end(); ++varTypeIt) {
        const string& varName = varTypeIt->first;
        const string& levelType = varTypeIt->second;
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
    if (p_->maxEnsembles > 1) {
        // TODO: read those from config?
        p_->ensembleDimName = "ensemble_member";
        CDMDimension ensembleDim(p_->ensembleDimName, p_->maxEnsembles);
        cdm_->addDimension(ensembleDim);
        vector<string> varShape(1, p_->ensembleDimName);
        CDMVariable ensembleVar(p_->ensembleDimName, CDM_SHORT, varShape);
        vector<short> eMembers(p_->maxEnsembles);
        for (size_t i = 0; i < p_->maxEnsembles; ++i) eMembers.at(i) = i;
        DataPtr eData = createData(CDM_SHORT, eMembers.begin(), eMembers.end());
        ensembleVar.setData(eData);
        cdm_->addVariable(ensembleVar);
        cdm_->addAttribute(ensembleVar.getName(), CDMAttribute("long_name", "ensemble run number"));
        cdm_->addAttribute(ensembleVar.getName(), CDMAttribute("standard_name", "realization"));

        if (p_->ensembleMemberIds.size() == p_->maxEnsembles) {
            // add a character dimension, naming all ensemble members
            size_t maxLen = 0;
            for (vector<pair<string, boost::regex> >::iterator emi = p_->ensembleMemberIds.begin(); emi != p_->ensembleMemberIds.end(); ++emi) {
                maxLen = std::max(maxLen, emi->first.size());
            }
            string charDim = p_->ensembleDimName + "_strlen";
            cdm_->addDimension(CDMDimension(charDim, maxLen));
            vector<string> nameShape;
            nameShape.push_back(charDim);
            nameShape.push_back(p_->ensembleDimName);
            CDMVariable names(p_->ensembleDimName + "_names", CDM_STRING, nameShape);
            boost::shared_array<char> namesAry(new char[maxLen*p_->maxEnsembles]());
            vector<string> members;
            vector<int> memberFlags;
            for (size_t i = 0; i < p_->ensembleMemberIds.size(); ++i) {
                string id = p_->ensembleMemberIds.at(i).first;
                std::copy(id.begin(), id.end(), &namesAry[i*maxLen]);
                members.push_back(id);
                memberFlags.push_back(i);
            }
            names.setData(createData(maxLen*p_->maxEnsembles, namesAry));
            cdm_->addVariable(names);
            cdm_->addAttribute(names.getName(), CDMAttribute("long_name", "names of ensemble members"));
            // add the names as flags (needed by ADAGUC)
            cdm_->addAttribute(p_->ensembleDimName, CDMAttribute("flag_values", CDM_INT, createData(CDM_INT, memberFlags.begin(), memberFlags.end())));
            cdm_->addAttribute(p_->ensembleDimName, CDMAttribute("flag_meanings", join(members.begin(), members.end(), " ")));
        }

    }
}

void GribCDMReader::initAddProjection()
{
    // get the overruled earthform
    XPathObjPtr xpathObj = p_->doc->getXPathObject("/gr:cdmGribReaderConfig/gr:overrule/gr:earthFigure");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    string replaceEarthString = "";
    if (size == 1) {
        replaceEarthString = getXmlProp(nodes->nodeTab[0], "proj4");
        LOG4FIMEX(logger, Logger::DEBUG,"overruling earth-parametes with " << replaceEarthString);
    }


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
            if (gridDef.isDegree()) {
                transform(xData.begin(), xData.end(), xData.begin(), &normalizeLongitude180<double>);
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
            // no normalization required for latitudes
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
    for (vector<GribFileMessage>::const_iterator gfmIt = p_->indices.begin(); gfmIt != p_->indices.end(); ++gfmIt) {
        const ProjectionInfo pi = p_->gridProjection[gfmIt->getGridDefinition()];
        assert(pi.xDim != "");
        CDMDataType type = CDM_DOUBLE;
        string varName = getVariableName(*gfmIt);
        if (initializedVariables.find(varName) == initializedVariables.end()) {
            initializedVariables.insert(varName);
            xmlNodePtr node = findVariableXMLNode(*gfmIt);
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
             if (gfmIt->getTotalNumberOfEnsembles() > 1) {
                 shape.push_back(p_->ensembleDimName);
             }
             assert(p_->varLevelTypePos.find(varName) != p_->varLevelTypePos.end());
             pair<string, size_t> levelTypePos = p_->varLevelTypePos[varName];

             string levelDimName = p_->levelDimNames[levelTypePos.first].at(levelTypePos.second);
             shape.push_back(levelDimName);
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
    }
 }


GribCDMReader::~GribCDMReader()
{
}

size_t GribCDMReader::getVariableMaxEnsembles(string varName) const {
    size_t ensembles;
    if (p_->varHasEnsemble.at(varName)) {
        ensembles = p_->maxEnsembles;
    } else {
        ensembles = 1;
    }
    return ensembles;
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
    if (unLimDimPos >= getData(p_->timeDimName)->size()) {
        throw CDMException("requested time outside data-region");
    }

    //map<string, map<size_t, map<long, map<size_t, size_t> > > > varTimeLevelEnsembleGFIBox;

    map<string, map<size_t, map<long, map<size_t, size_t> > > >::const_iterator gmIt = p_->varTimeLevelEnsembleGFIBox.find(varName);
    if (gmIt == p_->varTimeLevelEnsembleGFIBox.end()) {
        throw CDMException("no grib message found for variable '" + varName + "'");
    }


    // grib data can be (x,y,[ensemble,]level,time) or (x,y,[ensemble,]level) or just (x,y[,ensemble])
    const vector<string>& dims = variable.getShape();
    size_t slice_size = 1;
    for (vector<string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
        CDMDimension& dim = cdm_->getDimension(*it);
        if (! dim.isUnlimited()) {
            slice_size *= dim.getLength();
        }
    }

    LOG4FIMEX(logger, Logger::DEBUG, "building unlim-slice " << unLimDimPos << " for variable " << varName << ": size: " << slice_size);
    vector<GribFileMessage> slices;
    map<size_t, map<long, map<size_t, size_t> > >::const_iterator gmt = gmIt->second.find(unLimDimPos);
    if (gmt != gmIt->second.end()) {
        pair<string, size_t> typePos = p_->varLevelTypePos.at(varName);
        vector<long> levels = p_->levelValsOfType.at(typePos.first).at(typePos.second);
        for (size_t l = 0; l < levels.size(); ++l) {
            map<long, map<size_t, size_t> >::const_iterator gmtl = gmt->second.find(levels[l]);
            if (gmtl != gmt->second.end()) {
                for (size_t e = 0; e < getVariableMaxEnsembles(varName); ++e) {
                    map<size_t, size_t>::const_iterator gmtle = gmtl->second.find(e);
                    if (gmtle != gmtl->second.end()) {
                        slices.push_back(p_->indices.at(gmtle->second));
                    } else {
                        slices.push_back(GribFileMessage()); // add empty slice
                    }
                }
            } else {
                // layer not found, add empty slices
                for (size_t e = 0; e < getVariableMaxEnsembles(varName); ++e) {
                    slices.push_back(GribFileMessage()); // add empty slices
                }
            }
        }
    }

    // read data from file
    if (slices.size() == 0) return createData(variable.getDataType(), 0);

    // storage for complete data
    boost::shared_array<double> doubleArray(new double[slice_size]);
    // prefill with missing values
    double missingValue = cdm_->getFillValue(varName);
    fill(&doubleArray[0], &doubleArray[slice_size], missingValue);
    DataPtr data = createData(slice_size, doubleArray);
    // storage for one layer
    vector<double> gridData(slice_size/slices.size());
    size_t dataCurrentPos = 0;
    for (vector<GribFileMessage>::iterator gfmIt = slices.begin(); gfmIt != slices.end(); ++gfmIt) {
        // join the data of the different levels
        if (gfmIt->isValid()) {
            DataPtr data;
            size_t dataRead;
            {
                ScopedCritical lock(p_->mutex);
                dataRead = gfmIt->readData(gridData, missingValue);
            }
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
