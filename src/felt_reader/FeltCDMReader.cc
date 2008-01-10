#include "FeltCDMReader.h"
#include "Utils.h"
#include <boost/shared_ptr.hpp>
#include "Felt_File_Error.h"
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include <vector>
#include <sstream>
#include <cassert>

namespace MetNoUtplukk
{

static CDMAttribute createCDMAttribute(string name, string type, string value) throw(MetNoFelt::Felt_File_Error)
{
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

FeltCDMReader::FeltCDMReader(std::string filename, std::string configFilename) throw (MetNoFelt::Felt_File_Error)
: filename(filename), configFilename(configFilename), feltFile(filename)
{
    // test lib vs compile version
	xmlInitParser();
    LIBXML_TEST_VERSION
    boost::shared_ptr<xmlDoc> doc(xmlReadFile(configFilename.c_str(), NULL, 0), xmlFreeDoc);
    if (doc.get() == 0) {
    	xmlCleanupParser();
    	throw MetNoFelt::Felt_File_Error("unable to parse config file: " + configFilename);
    }
	boost::shared_ptr<xmlXPathContext> xpathCtx(xmlXPathNewContext(doc.get()), xmlXPathFreeContext);
	if (xpathCtx.get() == 0) {
		xmlCleanupParser();
		throw MetNoFelt::Felt_File_Error("unable to generate xpath context");
	}
	

	// fill the CDM;
	// set the global data for this feltFile derived from first data
	// TODO: translate producer-ids to something useful
    // TODO: add axes
    // time
	{
		std::string xpathTimeString("/config/axes/time");
		boost::shared_ptr<xmlXPathObject> xpathObj(xmlXPathEvalExpression(reinterpret_cast<const xmlChar*>(xpathTimeString.c_str()), xpathCtx.get()), xmlXPathFreeObject);
		if (xpathObj.get() == 0) {
			xmlCleanupParser();
			throw MetNoFelt::Felt_File_Error("unable to parse xpath" + xpathTimeString);
		}
		xmlNodeSetPtr nodes = xpathObj->nodesetval;
		if (nodes->nodeNr != 1) {
			xmlCleanupParser();
			throw MetNoFelt::Felt_File_Error("unable to find 'time'-axis in config: " + configFilename);
		}
		xmlNodePtr node = nodes->nodeTab[0];
		assert(node->type == XML_ELEMENT_NODE);
		string timeName(reinterpret_cast<char *>(xmlGetProp(node, reinterpret_cast<const xmlChar *>("name"))));
		string timeType(reinterpret_cast<char *>(xmlGetProp(node, reinterpret_cast<const xmlChar *>("type"))));
		CDMDataType timeDataType = string2datatype(timeType);
		long timeSize = feltFile.getFeltTimes().size();
		CDMDimension timeDim(timeName, timeSize);
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
    
    
	std::vector<MetNoFelt::Felt_Array> fArrays(feltFile.listFeltArrays());
	for (std::vector<MetNoFelt::Felt_Array>::iterator it = fArrays.begin(); it != fArrays.end(); ++it) {
		// projection of the array
		std::string projStr = MetNoFelt::getProjString(it->getGridType(), it->getGridParameters());
		if (projectionVariables.find(projStr) == projectionVariables.end()) {
			std::string projName("Projection" + projectionVariables.size());
			// projection-variable without datatype and dimension
			CDMVariable projVar(projName, CDM_NAT, std::vector<CDMDimension>());
			cdm.addVariable(projVar);
			projectionVariables[projStr] = projName;
			std::vector<CDMAttribute> projAttr = projStringToAttributes(projStr);
			for (std::vector<CDMAttribute>::iterator attrIt = projAttr.begin(); attrIt != projAttr.end(); ++attrIt) {
				cdm.addAttribute(projName, *attrIt);
			}
			
			// TODO:  add extra projection axes for lon lat etc
		}
		
		std::cerr << it->getName() << ":" << it->getIdentifier() << std::endl;
		std::string xpathString("/config/variables/parameter[@id='"+it->getIdentifier()+"']");
		boost::shared_ptr<xmlXPathObject> xpathObj(xmlXPathEvalExpression(reinterpret_cast<const xmlChar*>(xpathString.c_str()), xpathCtx.get()), xmlXPathFreeObject);
		if (xpathObj.get() == 0) {
			xmlCleanupParser();
			throw MetNoFelt::Felt_File_Error("unable to parse xpath" + xpathString);
		}
		xmlNodeSetPtr nodes = xpathObj->nodesetval;
		int size = (nodes) ? nodes->nodeNr : 0;
		if (size > 1) {
			xmlCleanupParser();
			throw MetNoFelt::Felt_File_Error("error in config-file: several entries for parameter: " + it->getIdentifier());
		}
    	if (size < 1) {
    		std::cerr << "config-file doesn't contain parameter: " << xpathString << std::endl;
    	} else {
    		assert(nodes->nodeTab[0]->type == XML_ELEMENT_NODE);
    		std::string varName(reinterpret_cast<char *>(xmlGetProp(nodes->nodeTab[0], reinterpret_cast<const xmlChar *>("name"))));
    		std::vector<CDMAttribute> attributes;
    		fillAttributeList(attributes, nodes->nodeTab[0]->children);
    	}
    	// TODO: map shape, generate variable, set attributes/variable to CDM
		
	}
	xmlCleanupParser();
}

FeltCDMReader::~FeltCDMReader()
{
}



TimeSliceData FeltCDMReader::getDataSlice(int variableId, Time time) {
	// TODO
	throw 1;
}

}
