#include "FeltCDMReader.h"
#include "Utils.h"
#include <boost/shared_ptr.hpp>
#include "Felt_File_Error.h"
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include <vector>
#include <sstream>
#include <iostream>
#include <cassert>

namespace MetNoUtplukk
{

static CDMAttribute createCDMAttribute(string name, string datatype, string value) throw(MetNoFelt::Felt_File_Error)
{
	std::string type(string2lowerCase(datatype));
	if (type == "float") {
		float f;
		std::stringstream str(value);
		str >> f;
		return CDMAttribute(name, f);
	} else if (type == "double") {
		double d;
		std::stringstream str(value); 
		str >> d;
		return CDMAttribute(name, d);		
	} else if (type == "int") {
		int i;
		std::stringstream str(value);
		str >> i;
		return CDMAttribute(name, i);
	} else if (type == "short") {
		short s;
		std::stringstream str(value);
		str >> s;
		return CDMAttribute(name, s);
	} else if (type == "char") {
		char c;
		std::stringstream str(value);
		str >> c;
		return CDMAttribute(name, c);
	} else if (type == "string") {
		return CDMAttribute(name, value);
	} else {
		throw MetNoFelt::Felt_File_Error("unknown type: " + type);
	}
}

static void fillAttributeList(vector<CDMAttribute>& attributes, xmlNodePtr node) {
	if (node == 0) return;
	string attribute("attribute");
	if ((node->type == XML_ELEMENT_NODE) &&
		(attribute == reinterpret_cast<const char *>(node->name))) {
			string name(reinterpret_cast<char *>(xmlGetProp(node, reinterpret_cast<const xmlChar *>("name"))));
			string value(reinterpret_cast<char *>(xmlGetProp(node, reinterpret_cast<const xmlChar *>("value"))));
			string type(reinterpret_cast<char *>(xmlGetProp(node, reinterpret_cast<const xmlChar *>("type"))));
			attributes.push_back(createCDMAttribute(name,type,value));
	}
	fillAttributeList(attributes, node->children);
	fillAttributeList(attributes, node->next);
}

static int readXPathNode(boost::shared_ptr<xmlXPathContext>& xpathCtx, string& xpathString, std::map<string, string>& xmlAttributes, std::vector<CDMAttribute>& varAttributes ) throw(MetNoFelt::Felt_File_Error)
{
	boost::shared_ptr<xmlXPathObject> xpathObj(xmlXPathEvalExpression(reinterpret_cast<const xmlChar*>(xpathString.c_str()), xpathCtx.get()), xmlXPathFreeObject);
	if (xpathObj.get() == 0) {
		throw MetNoFelt::Felt_File_Error("unable to parse xpath" + xpathString);
	}
	xmlNodeSetPtr nodes = xpathObj->nodesetval;
	int size = (nodes) ? nodes->nodeNr : 0; 
	if (size == 0) return 0;
	// only parsing node[0]
	xmlNodePtr node = nodes->nodeTab[0];
	if (node->type != XML_ELEMENT_NODE) {
		throw MetNoFelt::Felt_File_Error("xpath does not point to XML_ELEMENT_NODE: " + xpathString);
	}
	xmlAttrPtr attr = node->properties;
	while (attr != 0) {
		string name(reinterpret_cast<const char *>(attr->name));
		string value(reinterpret_cast<const char *>(attr->children->content));
		xmlAttributes[name] = value;
		attr = attr->next;
	}
	fillAttributeList(varAttributes, node->children);
	return size;
}

// simple RAII for xmlCleanup
static void myXmlCleanupParser(int* i) {
	//std::cerr << "running xmlCleanupParser code" << std::endl;
	xmlCleanupParser();
	delete i;
}


FeltCDMReader::FeltCDMReader(std::string filename, std::string configFilename) throw (MetNoFelt::Felt_File_Error)
: filename(filename), configFilename(configFilename), feltFile(filename)
{
    // test lib vs compile version
	xmlInitParser();
    LIBXML_TEST_VERSION
    boost::shared_ptr<int> xmlCleanupRAII(new int(1), myXmlCleanupParser);
    boost::shared_ptr<xmlDoc> doc(xmlReadFile(configFilename.c_str(), NULL, 0), xmlFreeDoc);
    if (doc.get() == 0) {
    	throw MetNoFelt::Felt_File_Error("unable to parse config file: " + configFilename);
    }
	boost::shared_ptr<xmlXPathContext> xpathCtx(xmlXPathNewContext(doc.get()), xmlXPathFreeContext);
	if (xpathCtx.get() == 0) {
		throw MetNoFelt::Felt_File_Error("unable to generate xpath context");
	}
	

	// fill the CDM;
	// set the global data for this feltFile derived from first data
	// TODO: translate producer-ids to something useful

	// add axes
	// time
	CDMDimension timeDim("null", 0);
	{
		std::string xpathTimeString("/config/axes/time");
		boost::shared_ptr<xmlXPathObject> xpathObj(xmlXPathEvalExpression(reinterpret_cast<const xmlChar*>(xpathTimeString.c_str()), xpathCtx.get()), xmlXPathFreeObject);
		if (xpathObj.get() == 0) {
			throw MetNoFelt::Felt_File_Error("unable to parse xpath" + xpathTimeString);
		}
		xmlNodeSetPtr nodes = xpathObj->nodesetval;
		if (nodes->nodeNr != 1) {
			throw MetNoFelt::Felt_File_Error("unable to find 'time'-axis in config: " + configFilename);
		}
		xmlNodePtr node = nodes->nodeTab[0];
		assert(node->type == XML_ELEMENT_NODE);
		string timeName(reinterpret_cast<char *>(xmlGetProp(node, reinterpret_cast<const xmlChar *>("name"))));
		string timeType(reinterpret_cast<char *>(xmlGetProp(node, reinterpret_cast<const xmlChar *>("type"))));
		CDMDataType timeDataType = string2datatype(timeType);
		long timeSize = feltFile.getFeltTimes().size();
		timeDim = CDMDimension(timeName, timeSize);
		timeDim.setUnlimited(true);
		cdm.addDimension(timeDim);
		std::vector<CDMDimension> timeShape;
		timeShape.push_back(timeDim);
		CDMVariable timeVar(timeName, timeDataType, timeShape);
		cdm.addVariable(timeVar);
		std::vector<CDMAttribute> timeAttributes;
		fillAttributeList(timeAttributes, nodes->nodeTab[0]->children);
		for (std::vector<CDMAttribute>::iterator it = timeAttributes.begin(); it != timeAttributes.end(); ++it) {
			cdm.addAttribute(timeVar.getName(), *it);
		}
	}
	
	// levels
	map<short, CDMDimension> levelDims;
	{
		std::map<short, std::vector<short> > levels = feltFile.getFeltLevels();
		for (std::map<short, std::vector<short> >::const_iterator it = levels.begin(); it != levels.end(); ++it) {
			// add a level
			std::string xpathLevelString("/config/axes/vertical_axis[@felt_id='"+type2string(it->first)+"']");
			boost::shared_ptr<xmlXPathObject> xpathObj(xmlXPathEvalExpression(reinterpret_cast<const xmlChar*>(xpathLevelString.c_str()), xpathCtx.get()), xmlXPathFreeObject);
			if (xpathObj.get() == 0) {
				throw MetNoFelt::Felt_File_Error("unable to parse xpath" + xpathLevelString);
			}
			xmlNodeSetPtr nodes = xpathObj->nodesetval;
			if (nodes->nodeNr != 1) {
				throw MetNoFelt::Felt_File_Error("unable to find 'vertical'-axis "+type2string(it->first)+" in config: " + configFilename);
			}
			xmlNodePtr node = nodes->nodeTab[0];
			assert(node->type == XML_ELEMENT_NODE);
			string levelName(reinterpret_cast<char *>(xmlGetProp(node, reinterpret_cast<const xmlChar *>("name"))));
			string levelId(reinterpret_cast<char *>(xmlGetProp(node, reinterpret_cast<const xmlChar *>("id"))));
			string levelType(reinterpret_cast<char *>(xmlGetProp(node, reinterpret_cast<const xmlChar *>("type"))));
			CDMDataType levelDataType = string2datatype(levelType);
			long levelSize = it->second.size();
			CDMDimension levelDim(levelId, levelSize);
			levelDims.insert(std::pair<short, CDMDimension>(it->first, levelDim));
			cdm.addDimension(levelDim);
			std::vector<CDMDimension> levelShape;
			levelShape.push_back(levelDim);
			CDMVariable levelVar(levelId, levelDataType, levelShape);
			cdm.addVariable(levelVar);
			std::vector<CDMAttribute> levelAttributes;
			fillAttributeList(levelAttributes, nodes->nodeTab[0]->children);
			for (std::vector<CDMAttribute>::iterator it = levelAttributes.begin(); it != levelAttributes.end(); ++it) {
				cdm.addAttribute(levelVar.getName(), *it);
			}
		}
	}
    
	//x,y dim will be set with the projection, can also = long/lat
	// TODO: read attr from config, corresponding to projection
    CDMDimension xDim("x", feltFile.getNX());
    CDMDimension yDim("y", feltFile.getNY());
	
    // projection of the array (currently only one allowed
    {
    	const boost::array<float, 6>& gridParams = feltFile.getGridParameters();
    	short gridType = feltFile.getGridType();
    	std::string projStr = MetNoFelt::getProjString(gridType, gridParams);
    	std::string projName("projection_" + gridType);
    	// projection-variable without datatype and dimension
    	CDMVariable projVar(projName, CDM_NAT, std::vector<CDMDimension>());
    	cdm.addVariable(projVar);
    	std::vector<CDMAttribute> projAttr = projStringToAttributes(projStr);
    	for (std::vector<CDMAttribute>::iterator attrIt = projAttr.begin(); attrIt != projAttr.end(); ++attrIt) {
    		cdm.addAttribute(projName, *attrIt);
    	}
    	
    	{
    	// create the x dimension variables and dimensions
			std::string xpathStringX("/config/axes/spatial_axis[@projection_felt_id='"+type2string(gridType)+"' and @id='x']");
			std::vector<CDMAttribute> xVarAttributes;
			std::map<string, string> xXmlAttributes;
			int found = readXPathNode(xpathCtx, xpathStringX, xXmlAttributes, xVarAttributes);
			if (found != 1) {
				throw MetNoFelt::Felt_File_Error("error in config-file: not exactly 1 entry for xpath: " + xpathStringX);
			}
			std::string xName(xXmlAttributes["name"]);
			xDim = CDMDimension(xName, feltFile.getNX());
			CDMDataType xDataType = string2datatype(xXmlAttributes["type"]);
			std::vector<CDMDimension> xDimShape;
			xDimShape.push_back(xDim);
			CDMVariable xVar(xName, xDataType, xDimShape);
			xVar.setData(feltFile.getXData());
			cdm.addDimension(xDim);
			cdm.addVariable(xVar);
			for (std::vector<CDMAttribute>::iterator attrIt = xVarAttributes.begin(); attrIt != xVarAttributes.end(); ++attrIt) {
				cdm.addAttribute(xName, *attrIt);
			}
    	}
    	{
    		// create the y dimension variables and dimensions
    		std::string xpathStringY("/config/axes/spatial_axis[@projection_felt_id='"+type2string(gridType)+"' and @id='y']");
    		std::vector<CDMAttribute> yVarAttributes;
    		std::map<string, string> yXmlAttributes;
    		int found = readXPathNode(xpathCtx, xpathStringY, yXmlAttributes, yVarAttributes);
    		if (found != 1) {
    			throw MetNoFelt::Felt_File_Error("error in config-file: not exactly 1 entry for xpath: " + xpathStringY);    		
    		}
    		std::string yName(yXmlAttributes["name"]);
    		yDim = CDMDimension(yName, feltFile.getNY());
    		CDMDataType yDataType = string2datatype(yXmlAttributes["type"]);
    		std::vector<CDMDimension> yDimShape;
    		yDimShape.push_back(yDim);
    		CDMVariable yVar(yName, yDataType, yDimShape);
    		yVar.setData(feltFile.getYData());
    		cdm.addDimension(yDim);
    		cdm.addVariable(yVar);
    		for (std::vector<CDMAttribute>::iterator attrIt = yVarAttributes.begin(); attrIt != yVarAttributes.end(); ++attrIt) {
    			cdm.addAttribute(yName, *attrIt);
    		}
    	}
    	
    	// TODO:  add extra projection axes for lon lat etc
    }


	std::vector<MetNoFelt::Felt_Array> fArrays(feltFile.listFeltArrays());
	for (std::vector<MetNoFelt::Felt_Array>::iterator it = fArrays.begin(); it != fArrays.end(); ++it) {
		std::cerr << it->getName() << ":" << it->getIdentifier() << std::endl;
		std::string xpathString("/config/variables/parameter[@id='"+it->getIdentifier()+"']");
		boost::shared_ptr<xmlXPathObject> xpathObj(xmlXPathEvalExpression(reinterpret_cast<const xmlChar*>(xpathString.c_str()), xpathCtx.get()), xmlXPathFreeObject);
		if (xpathObj.get() == 0) {
			throw MetNoFelt::Felt_File_Error("unable to parse xpath" + xpathString);
		}
		xmlNodeSetPtr nodes = xpathObj->nodesetval;
		int size = (nodes) ? nodes->nodeNr : 0;
		if (size > 1) {
			throw MetNoFelt::Felt_File_Error("error in config-file: several entries for parameter: " + it->getIdentifier());
		}
    	if (size < 1) {
    		std::cerr << "config-file doesn't contain parameter: " << xpathString << std::endl;
    	} else {
    		assert(nodes->nodeTab[0]->type == XML_ELEMENT_NODE);
    		std::string varName(reinterpret_cast<char *>(xmlGetProp(nodes->nodeTab[0], reinterpret_cast<const xmlChar *>("name"))));
    		std::vector<CDMAttribute> attributes;
    		fillAttributeList(attributes, nodes->nodeTab[0]->children);
    	
    	
    		// TODO: map shape, generate variable, set attributes/variable to CDM
    		std::vector<CDMDimension> shape;
    		shape.push_back(xDim);
    		shape.push_back(yDim);
    		if (it->getLevels().size() > 1) {
    			shape.push_back(levelDims[it->getLevelType()]);
    		}
    		if (it->getTimes().size() > 1) {
    			shape.push_back(timeDim);
    		}
    		CDMVariable var(varName, CDM_SHORT, shape);
    		cdm.addVariable(var);
    		for (std::vector<CDMAttribute>::const_iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
    			cdm.addAttribute(varName, *attrIt);
    		}
    	}
	}
}

FeltCDMReader::~FeltCDMReader()
{
}



TimeSliceData FeltCDMReader::getDataSlice(int variableId, Time time) {
	// TODO
	throw 1;
}

}
