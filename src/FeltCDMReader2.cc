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

#include "fimex/CDM.h"
#include "fimex/FeltCDMReader2.h"
#include "fimex/Felt_File2.h"
#include "fimex/Utils.h"
#include "fimex/Felt_File_Error.h"
#include "fimex/Felt_Array2.h"
#include "fimex/interpolation.h"
#include "fimex/CDMDataType.h"
#include "fimex/DataImpl.h"
#include "fimex/ReplaceStringTimeObject.h"
#include "fimex/TimeUnit.h"
#include "fimex/Utils.h"
#include "fimex/XMLDoc.h"
#include "fimex/coordSys/Projection.h"
#include "CDM_XMLConfigHelper.h"
#include "felt/FeltGridDefinition.h"
#include "boost/date_time/gregorian/gregorian.hpp"
#include <boost/shared_ptr.hpp>
#include <boost/regex.hpp>
#include <boost/bind.hpp>
#include <sstream>
#include <iostream>
#include <cassert>
#include <cstdlib>
#include <functional>
#include <algorithm>

namespace MetNoFimex
{

using namespace std;
using namespace MetNoFelt;

static LoggerPtr logger = getLogger("fimex.FeltCDMReader2");

std::vector<double> FeltCDMReader2::readValuesFromXPath(const XMLDoc& doc, const std::string& variableXPath)
{
	std::vector<double> retValues;
	std::string valuesXPath(variableXPath + "/values");
	XPathObjPtr xpathObj = doc.getXPathObject(valuesXPath);
	xmlNodeSetPtr nodes = xpathObj->nodesetval;
	int size = (nodes) ? nodes->nodeNr : 0;
	for (int i = 0; i < size; i++) {
		xmlNodePtr node = nodes->nodeTab[i];
		if (node->type == XML_ELEMENT_NODE) {
			std::string mode = getXmlProp(node, "mode");
			if (mode == "" || mode == "inline") {
				// add all space delimited values to the retVal vector
				xmlChar *valuePtr = xmlNodeGetContent(node);
				std::string
				values(reinterpret_cast<const char *>(valuePtr));
				xmlFree(valuePtr);
				std::vector<std::string> tokens = tokenize(values, " ");
				std::transform(tokens.begin(), tokens.end(),
						std::back_inserter(retValues), string2type<double>);
			} else if (mode == "level2") {
				// get level-id from variableXPath
				boost::smatch matches;
				boost::regex rgx(boost::regex("felt_id=[\"'](\\d+)[\"']"));
				if (boost::regex_search(valuesXPath, matches, rgx)) {
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
				boost::smatch matches;
				boost::regex rgx(boost::regex("felt_id=[\"'](\\d+)[\"']"));
				if (boost::regex_search(valuesXPath, matches, rgx)) {
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
				const boost::shared_array<double> apData = ap.getData()->asConstDouble();
				const boost::shared_array<double> bData = b.getData()->asConstDouble();
				for (size_t i = 0; i < ap.getData()->size(); ++i) {
					retValues.push_back(apData[i]/p0 + bData[i]);
				}
			}
			std::string sscale = getXmlProp(node, "scale_factor");
			if (sscale != "") {
				double scale = string2type<double>(sscale);
				std::transform(retValues.begin(), retValues.end(),
						retValues.begin(), std::bind1st(multiplies<double>(), scale));
			}
		}
	}
	return retValues;
}
void FeltCDMReader2::readAdditionalAxisVariablesFromXPath(const XMLDoc& doc, const std::string& xpathLevelString, const map<string, boost::shared_ptr<ReplaceStringObject> >& templateReplacements) throw(MetNoFelt::Felt_File_Error)
{
	std::string addAxisXPath(xpathLevelString + "/additional_axis_variable");
	XPathObjPtr xpathObj = doc.getXPathObject(addAxisXPath);
	if (xpathObj.get() != 0) {
		xmlNodeSetPtr nodes = xpathObj->nodesetval;
		int size = (nodes) ? nodes->nodeNr : 0;
		for (int i = 0; i < size; i++) {
			xmlNodePtr node = nodes->nodeTab[i];
			string name = getXmlProp(node,"name");
			string type = getXmlProp(node, "type");
			string axis = getXmlProp(node, "axis");
			CDMDataType dataType = string2datatype(type);
			std::vector<double> values = readValuesFromXPath(doc, addAxisXPath + "[@name='" + name + "']");
			std::vector<std::string> shape;
			shape.push_back(axis);
			CDMVariable var(name, dataType, shape);
			var.setData(createData(dataType, values.begin(), values.end()));
			cdm_->addVariable(var);


			// add the attributes of the extra variables
			std::vector<CDMAttribute> attributes;
			fillAttributeListFromXMLNode(attributes, node->children, templateReplacements);
			for (std::vector<CDMAttribute>::iterator it = attributes.begin(); it != attributes.end(); ++it) {
				cdm_->addAttribute(name, *it);
			}
		}
	}
}


FeltCDMReader2::FeltCDMReader2(std::string filename, std::string configFilename) throw (CDMException)
: filename(filename), configFilename(configFilename)
{
	try {
		init();
	} catch (MetNoFelt::Felt_File_Error& ffe) {
		throw CDMException(std::string("Felt_File_Error: ") + ffe.what());
	}
}

FeltCDMReader2::~FeltCDMReader2()
{
}

void FeltCDMReader2::init() throw(MetNoFelt::Felt_File_Error, CDMException) {
    // test lib vs compile version
    MetNoFimex::XMLDoc doc(configFilename);
	// open the feltFile with the desired parameters
	std::vector<std::string> knownFeltIds = initGetKnownFeltIdsFromXML(doc);
	std::map<std::string, std::string> options = initGetOptionsFromXML(doc);
	feltfile_ = boost::shared_ptr<MetNoFelt::Felt_File2>(new MetNoFelt::Felt_File2(filename, knownFeltIds, options));
	{
		// fill templateReplacementAttributes: MIN_DATETIME, MAX_DATETIME
		std::vector<boost::posix_time::ptime> feltTimes = feltfile_->getFeltTimes();
		templateReplacementAttributes["MIN_DATETIME"] = boost::shared_ptr<ReplaceStringObject>(new ReplaceStringTimeObject(posixTime2epochTime(feltTimes[0])));
		templateReplacementAttributes["MAX_DATETIME"] = boost::shared_ptr<ReplaceStringObject>(new ReplaceStringTimeObject(posixTime2epochTime(feltTimes[feltTimes.size()-1])));
	}

	// fill the CDM;
	// set the global data for this feltFile derived from first data
	// TODO: translate producer-ids to something useful?

	// global attributes from config
	initAddGlobalAttributesFromXML(doc);


	// add axes
	// time
	CDMDimension timeDim = initAddTimeDimensionFromXML(doc);
	// levels
	std::map<short, CDMDimension> levelDims = initAddLevelDimensionsFromXML(doc);
	//x,y dim will be set with the projection, can also = long/lat
	// setting default-value
    xDim = CDMDimension("x", feltfile_->getNX());
    yDim = CDMDimension("y", feltfile_->getNY());

    // projection of the array (currently only one allowed
    std::string projName, coordinates;
    // get projection and coordinates
    initAddProjectionFromXML(doc, projName, coordinates);

    // add variables
    initAddVariablesFromXML(doc, projName, coordinates, timeDim, levelDims);
}

std::vector<std::string> FeltCDMReader2::initGetKnownFeltIdsFromXML(const XMLDoc& doc)
{
	std::vector<std::string> knownFeltIds;
	XPathObjPtr xpathObj = doc.getXPathObject("/cdm_felt_config/variables/parameter");
	xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
	for (int i = 0; i < size; ++i) {
		xmlNodePtr node = nodes->nodeTab[i];
		string id = getXmlProp(node, "id");
		// get the datatype
		std::string dataType = getXmlProp(node, "type");
		if (dataType != "") {
			dataType = ":dataType=" + dataType;
		}
		// get the fill value
		XPathObjPtr xpathObj = doc.getXPathObject("/cdm_felt_config/variables/parameter[@id=\""+id+"\"]/attribute[@name=\"_FillValue\"]");
		std::string fillValue;
		if (xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
			fillValue = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
			if (fillValue != "") {
				fillValue = ":fillValue=" + fillValue;
			}
		}
		knownFeltIds.push_back(id + dataType + fillValue);
	}
	return knownFeltIds;
}

std::map<std::string, std::string> FeltCDMReader2::initGetOptionsFromXML(const XMLDoc& doc)
{
	std::map<std::string, std::string> options;
	// optional processing options
	XPathObjPtr xpathObj = doc.getXPathObject("/cdm_felt_config/processOptions/option");
	xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
	for (int i = 0; i < size; ++i) {
		std::string name = getXmlProp(nodes->nodeTab[i], "name");
		std::string value = getXmlProp(nodes->nodeTab[i], "value");
		options[name] = value;
	}
	return options;
}
void FeltCDMReader2::initAddGlobalAttributesFromXML(const XMLDoc& doc)
{
	std::string xpathString("/cdm_felt_config/global_attributes");
	XPathObjPtr xpathObj = doc.getXPathObject(xpathString);
	xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
	if (size != 1) {
		throw MetNoFelt::Felt_File_Error("unable to find " + xpathString + " in config: " + configFilename);
	}
	for (int i = 0; i < size; ++i) {
		xmlNodePtr node = nodes->nodeTab[i];
		assert(node->type == XML_ELEMENT_NODE);
		std::vector<CDMAttribute> globAttributes;
		fillAttributeListFromXMLNode(globAttributes, nodes->nodeTab[0]->children, templateReplacementAttributes);
		for (std::vector<CDMAttribute>::iterator it = globAttributes.begin(); it != globAttributes.end(); ++it) {
			cdm_->addAttribute(cdm_->globalAttributeNS(), *it);
		}
	}
	if (! cdm_->checkVariableAttribute(cdm_->globalAttributeNS(), "history", boost::regex(".*"))) {
	    boost::posix_time::ptime now(boost::posix_time::second_clock::universal_time());
		cdm_->addAttribute(cdm_->globalAttributeNS(), CDMAttribute("history", boost::gregorian::to_iso_extended_string(now.date()) + " creation by fimex from file '"+ filename+"'"));
	}
}

CDMDimension FeltCDMReader2::initAddTimeDimensionFromXML(const XMLDoc& doc)
{
    string timeName;
    string timeType;
    xmlNodePtr timeNode;
    {
        XPathObjPtr xpathObj = doc.getXPathObject("/cdm_felt_config/axes/time");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        if (size != 1) {
            throw MetNoFelt::Felt_File_Error("unable to find exactly 1 'time'-axis in config: " + configFilename);
        }
        timeNode = nodes->nodeTab[0];
        assert(timeNode->type == XML_ELEMENT_NODE);
        timeName = getXmlProp(timeNode, "name");
        timeType = getXmlProp(timeNode, "type");
    }
    string timeUnits;
    {
        XPathObjPtr xpathObj = doc.getXPathObject("/cdm_felt_config/axes/time/attribute[@name='units']");
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
	std::vector<std::string> timeShape;
	timeShape.push_back(timeDim.getName());
    CDMDataType timeDataType = string2datatype(timeType);
	CDMVariable timeVar(timeName, timeDataType, timeShape);
	timeVec = feltfile_->getFeltTimes();
	vector<double> timeUnitVec;
	TimeUnit tu(timeUnits);
	transform(timeVec.begin(), timeVec.end(),
	          back_inserter(timeUnitVec),
	          bind1st(mem_fun(&TimeUnit::posixTime2unitTime), &tu));
	boost::shared_ptr<Data> timeData = createData(timeDataType, timeUnitVec.begin(), timeUnitVec.end());
	timeVar.setData(timeData);
	cdm_->addVariable(timeVar);
	std::vector<CDMAttribute> timeAttributes;
	fillAttributeListFromXMLNode(timeAttributes, timeNode->children, templateReplacementAttributes);
	for (std::vector<CDMAttribute>::iterator it = timeAttributes.begin(); it != timeAttributes.end(); ++it) {
		cdm_->addAttribute(timeVar.getName(), *it);
	}

	// add the unique reference time, if exists
	try {
	    boost::shared_ptr<boost::posix_time::ptime> refTime = feltfile_->getUniqueReferenceTime();
	    if (refTime.get() != 0) {
	        // TODO: move reference time name to config
	        std::string referenceTime = "forecast_reference_time";
	        std::vector<std::string> nullShape;
	        CDMVariable refTimeVar(referenceTime, timeDataType, nullShape);
	        boost::shared_ptr<Data> timeData = createData(timeDataType, 1);
	        timeData->setValue(0, tu.posixTime2unitTime(*refTime));
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

std::map<short, CDMDimension> FeltCDMReader2::initAddLevelDimensionsFromXML(const XMLDoc& doc)
{
	map<short, CDMDimension> levelDims;
	std::map<short, vector<LevelPair> > levels = feltfile_->getFeltLevelPairs();
	for (std::map<short, vector<LevelPair> >::const_iterator it = levels.begin(); it != levels.end(); ++it) {
		// add a level
		std::string xpathLevelString("/cdm_felt_config/axes/vertical_axis[@felt_id='"+type2string(it->first)+"']");
		XPathObjPtr xpathObj = doc.getXPathObject(xpathLevelString);
		xmlNodeSetPtr nodes = xpathObj->nodesetval;
	    int size = (nodes) ? nodes->nodeNr : 0;
		if (size != 1) {
			throw MetNoFelt::Felt_File_Error("unable to find 'vertical'-axis "+type2string(it->first)+" in config: " + configFilename);
		}
		xmlNodePtr node = nodes->nodeTab[0];
		assert(node->type == XML_ELEMENT_NODE);
		string levelName = getXmlProp(node, "name");
		string levelId = getXmlProp(node, "id");
		string levelType = getXmlProp(node, "type");
		CDMDataType levelDataType = string2datatype(levelType);
		long levelSize = it->second.size();
		CDMDimension levelDim(levelId, levelSize);
		levelDims.insert(std::pair<short, CDMDimension>(it->first, levelDim));
		cdm_->addDimension(levelDim);
		levelVecMap[levelDim.getName()] = it->second;

		// create level variable without data!
		std::vector<std::string> levelShape;
		levelShape.push_back(levelDim.getName());
		CDMVariable levelVar(levelId, levelDataType, levelShape);
		cdm_->addVariable(levelVar);

		// add attributes
		std::vector<CDMAttribute> levelAttributes;
		fillAttributeListFromXMLNode(levelAttributes, nodes->nodeTab[0]->children, templateReplacementAttributes);
		for (std::vector<CDMAttribute>::iterator ait = levelAttributes.begin(); ait != levelAttributes.end(); ++ait) {
			cdm_->addAttribute(levelVar.getName(), *ait);
		}

		// read additional axis variables
		readAdditionalAxisVariablesFromXPath(doc, xpathLevelString, templateReplacementAttributes);

		// read level data after! additional axis variables since additional axis variable might contain
		// data needed from level data (i.e. for hybrid_sigma levels)
		std::vector<double> lv = readValuesFromXPath(doc, xpathLevelString);
		boost::shared_ptr<Data> data;
		if (lv.size() > 0) {
			// use values from xml-file
			data = createData(levelDataType, lv.begin(), lv.end());
		} else {
			//use values from felt-file
		    const vector<LevelPair>& lpv = it->second;
			std::vector<short> lvs;
			for (vector<LevelPair>::const_iterator level_it =  lpv.begin(); level_it != lpv.end(); ++level_it) {
			    lvs.push_back(level_it->first);
			}
			data = createData(levelDataType, lvs.begin(), lvs.end());
		}
		cdm_->getVariable(levelDim.getName()).setData(data);


	}
	return levelDims;
}

void FeltCDMReader2::initAddProjectionFromXML(const XMLDoc& doc, std::string& projName, std::string& coordinates)
{
	boost::shared_ptr<felt::FeltGridDefinition> gridDef = feltfile_->getGridDefinition();
	std::string projStr = gridDef->projDefinition();
	int gridType = feltfile_->getGridType();
	projName = std::string("projection_" + type2string(gridType));
	// projection-variable without datatype and dimension
	CDMVariable projVar(projName, CDM_NAT, std::vector<std::string>());
	cdm_->addVariable(projVar);
	std::vector<CDMAttribute> projAttr = Projection::createByProj4(projStr)->getParameters();
	for (std::vector<CDMAttribute>::iterator attrIt = projAttr.begin(); attrIt != projAttr.end(); ++attrIt) {
		cdm_->addAttribute(projName, *attrIt);
	}

	{
		// create the x dimension variables and dimensions
		std::string xpathStringX("/cdm_felt_config/axes/spatial_axis[@projection_felt_id='"+type2string(gridType)+"' and @id='x']");
		std::vector<CDMAttribute> xVarAttributes;
		std::map<string, string> xXmlAttributes;
		int found = readXPathNodeWithCDMAttributes(doc, xpathStringX, xXmlAttributes, xVarAttributes, templateReplacementAttributes);
		if (found != 1) {
			throw MetNoFelt::Felt_File_Error("error in config-file: not exactly 1 entry for xpath: " + xpathStringX);
		}
		std::string xName(xXmlAttributes["name"]);
		xDim = CDMDimension(xName, feltfile_->getNX());
		CDMDataType xDataType = string2datatype(xXmlAttributes["type"]);
		std::vector<std::string> xDimShape;
		xDimShape.push_back(xDim.getName());
		CDMVariable xVar(xName, xDataType, xDimShape);
		xVar.setData(feltfile_->getXData());
		cdm_->addDimension(xDim);
		cdm_->addVariable(xVar);
		for (std::vector<CDMAttribute>::iterator attrIt = xVarAttributes.begin(); attrIt != xVarAttributes.end(); ++attrIt) {
			cdm_->addAttribute(xName, *attrIt);
		}
	}
	{
		// create the y dimension variables and dimensions
		std::string xpathStringY("/cdm_felt_config/axes/spatial_axis[@projection_felt_id='"+type2string(gridType)+"' and @id='y']");
		std::vector<CDMAttribute> yVarAttributes;
		std::map<string, string> yXmlAttributes;
		int found = readXPathNodeWithCDMAttributes(doc, xpathStringY, yXmlAttributes, yVarAttributes, templateReplacementAttributes);
		if (found != 1) {
			throw MetNoFelt::Felt_File_Error("error in config-file: not exactly 1 entry for xpath: " + xpathStringY);
		}
		std::string yName(yXmlAttributes["name"]);
		yDim = CDMDimension(yName, feltfile_->getNY());
		CDMDataType yDataType = string2datatype(yXmlAttributes["type"]);
		std::vector<std::string> yDimShape;
		yDimShape.push_back(yDim.getName());
		CDMVariable yVar(yName, yDataType, yDimShape);
		yVar.setData(feltfile_->getYData());
		cdm_->addDimension(yDim);
		cdm_->addVariable(yVar);
		for (std::vector<CDMAttribute>::iterator attrIt = yVarAttributes.begin(); attrIt != yVarAttributes.end(); ++attrIt) {
			cdm_->addAttribute(yName, *attrIt);
		}
	}

	std::string longName;
	std::string latName;
	{
		// read longitude and latitude names for projection axes
		std::string xpathStringLong("/cdm_felt_config/axes/spatial_axis[@id='longitude']");
		std::vector<CDMAttribute> lonlatVarAttributes;
		std::map<string, string> lonlatXmlAttributes;
		int found = readXPathNodeWithCDMAttributes(doc, xpathStringLong, lonlatXmlAttributes, lonlatVarAttributes, templateReplacementAttributes);
		if (found != 1) {
			throw MetNoFelt::Felt_File_Error("error in config-file: not exactly 1 entry for xpath: " + xpathStringLong);
		}
		longName = lonlatXmlAttributes["name"];
		std::string xpathStringLat("/cdm_felt_config/axes/spatial_axis[@id='latitude']");
		found = readXPathNodeWithCDMAttributes(doc, xpathStringLat, lonlatXmlAttributes, lonlatVarAttributes, templateReplacementAttributes);
		if (found != 1) {
			throw MetNoFelt::Felt_File_Error("error in config-file: not exactly 1 entry for xpath: " + xpathStringLat);
		}
		latName = lonlatXmlAttributes["name"];
	}

	// add projection axes 'coordinates = "lon lat";
	if (xDim.getName() != longName && yDim.getName() != latName) {
		coordinates = longName + " " + latName;
		try {
			cdm_->generateProjectionCoordinates(projName, xDim.getName(), yDim.getName(), longName, latName);
		} catch (MetNoFimex::CDMException& ex) {
			throw MetNoFelt::Felt_File_Error(ex.what());
		}
	}
}


void FeltCDMReader2::initAddVariablesFromXML(const XMLDoc& doc, const std::string& projName, const std::string& coordinates, const CDMDimension& timeDim, const std::map<short, CDMDimension>& levelDims)
{
	vector<boost::shared_ptr<Felt_Array2> > fArrays(feltfile_->listFeltArrays());
	for (std::vector<boost::shared_ptr<Felt_Array2> >::const_iterator it = fArrays.begin(); it != fArrays.end(); ++it) {
		std::string xpathString("/cdm_felt_config/variables/parameter[@id='"+(*it)->getName()+"']");
		XPathObjPtr xpathObj = doc.getXPathObject(xpathString);
		xmlNodeSetPtr nodes = xpathObj->nodesetval;
		int size = (nodes) ? nodes->nodeNr : 0;
		if (size > 1) {
			throw MetNoFelt::Felt_File_Error("error in config-file: several entries for parameter: " + (*it)->getName());
		}
    	if (size < 1) {
    		std::cerr << "config-file doesn't contain parameter: " << xpathString << std::endl;
    	} else {
    		assert(nodes->nodeTab[0]->type == XML_ELEMENT_NODE);
    		std::string varName = getXmlProp(nodes->nodeTab[0], "name");
    		std::vector<CDMAttribute> attributes;
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
    		std::vector<std::string> shape;
    		shape.push_back(xDim.getName());
    		shape.push_back(yDim.getName());
    		if ((*it)->getLevelPairs().size() > 1) {
    			shape.push_back(levelDims.find((*it)->getLevelType())->second.getName());
    		}
    		if ((*it)->getTimes().size() > 0) {
    			shape.push_back(timeDim.getName());
    		}
    		CDMDataType type = string2datatype((*it)->getDatatype());
    		CDMVariable var(varName, type, shape);
    		if (vectorCounterpart != "") {
    			var.setAsSpatialVector(vectorCounterpart, vectorDirection);
    		}
    		cdm_->addVariable(var);
    		varNameFeltIdMap[varName] = (*it)->getName();
    		//  update scaling factor attribute with value from felt-file and xml-setup (only for short-values)
    		if ((*it)->getScalingFactor() != 1) {
    			bool found = false;
        		for (std::vector<CDMAttribute>::iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
        			if (attrIt->getName() == "scale_factor") {
        				found = true;
        				float scale = (attrIt->getData()->asConstFloat())[0] * (*it)->getScalingFactor();
        				attrIt->getData()->setValue(0, scale);
        			}
        		}
        		if (! found) {
        			attributes.push_back(CDMAttribute("scale_factor", static_cast<float>((*it)->getScalingFactor())));
        		}
    		}
    		for (std::vector<CDMAttribute>::const_iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
    			cdm_->addAttribute(varName, *attrIt);
    		}
    	}
	}
}

boost::shared_ptr<Data> FeltCDMReader2::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException) {
	const CDMVariable& variable = cdm_->getVariable(varName);
	if (variable.hasData()) {
		return getDataSliceFromMemory(variable, unLimDimPos);
	}
	// only time can be unLimDim
	if (unLimDimPos > timeVec.size()) {
		throw CDMException("requested time outside data-region");
	}

	// felt data can be x,y,level,time; x,y,level; x,y,time; x,y;
	const vector<std::string>& dims = variable.getShape();
	const CDMDimension* layerDim = 0;
	size_t xy_size = 1;
	for (vector<std::string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
		CDMDimension& dim = cdm_->getDimension(*it);
		if (dim.getName() != xDim.getName() &&
			dim.getName() != yDim.getName() &&
			!dim.isUnlimited())
		{
			layerDim = &dim;
		}
		if (! dim.isUnlimited()) {
			xy_size *= dim.getLength();
		}
	}
	boost::shared_ptr<Data> data = createData(variable.getDataType(), xy_size);
	try {
		std::map<std::string, std::string>::const_iterator foundId = varNameFeltIdMap.find(variable.getName());
		if (foundId != varNameFeltIdMap.end()) {
			boost::shared_ptr<MetNoFelt::Felt_Array2> fa(feltfile_->getFeltArray(foundId->second));

			// test for availability of the current time in the variable (getSlice will get data for every time)
			vector<boost::posix_time::ptime> faTimes = fa->getTimes();
			short contains = false;
			if (faTimes.size() == 0) {
			    // time-less variable, time-check irrelevant
			    contains = true;
			} else {
			    for (vector<boost::posix_time::ptime>::const_iterator it = faTimes.begin(); it != faTimes.end(); it++) {
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
			std::vector<LevelPair> layerVals;
			if ((layerDim != 0) && (layerDim->getLength() > 0)) {
				for (size_t i = 0; i < layerDim->getLength(); ++i) {
					layerVals.push_back(levelVecMap[layerDim->getName()][i]);
				}
			} else {
				// no layers, just 1 level
				std::vector<LevelPair> levels = fa->getLevelPairs();
				if (levels.size() == 1) {
					layerVals.push_back(*(levels.begin()));
				} else {
					throw CDMException("variable " +variable.getName() + " has unspecified levels");
				}
			}
			size_t dataCurrentPos = 0;
			// xa
			int xDim = fa->getX();
			int yDim = fa->getY();
			vector<short> gridData;
			gridData.reserve(xDim*yDim);
			for (std::vector<LevelPair>::const_iterator lit = layerVals.begin(); lit != layerVals.end(); ++lit) {
			    // read the slice
				boost::shared_ptr<Data> levelData = feltfile_->getScaledDataSlice(fa, timeVec[unLimDimPos], *lit);
				data->setValues(dataCurrentPos, *levelData, 0, levelData->size());
				dataCurrentPos += levelData->size();
			}
		}
	} catch (MetNoFelt::Felt_File_Error& ffe) {
		throw CDMException(std::string("Felt_File_Error: ") + ffe.what());
	}
	return data;
}


boost::shared_ptr<FeltCDMReader2> getFeltReader(std::string filename, std::string configFilename)
{
    return boost::shared_ptr<FeltCDMReader2>(new FeltCDMReader2(filename, configFilename));
}


}
