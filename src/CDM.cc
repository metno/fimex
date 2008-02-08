#include "CDM.h"

namespace MetNoUtplukk
{

CDM::CDM()
{
}

CDM::~CDM()
{
}

void CDM::addVariable(CDMVariable var) throw(CDMException)
{
	if (variables.find(var.getName()) == variables.end()) {
		variables.insert(std::pair<std::string, CDMVariable>(var.getName(), var));
	} else {
		throw CDMException("cannot add variable: " + var.getName() + " already exists");
	}
}

void CDM::addDimension(CDMDimension dim) throw(CDMException)
{
	if (dimensions.find(dim.getName()) == dimensions.end()) {
		dimensions.insert(std::pair<std::string, CDMDimension>(dim.getName(), dim));
	} else {
		throw CDMException("cannot add dimension: " + dim.getName() + " already exists");
	}
}


void CDM::addAttribute(std::string varName, CDMAttribute attr) throw(CDMException)
{
	if ((varName != globalAttributeNS ()) && (variables.find(varName) == variables.end())) {
		throw CDMException("cannot add attribute: variable " + varName + " does not exist");
	} else {
		std::map<std::string, CDMAttribute>& attrMap = attributes[varName];
		if (attrMap.find(attr.getName()) == attrMap.end()) {
			attrMap.insert(std::pair<std::string, CDMAttribute>(attr.getName(),attr));
		} else {
			throw CDMException("cannot add attribute: attribute " + varName + "." + attr.getName() + " already exists");
		}
	}  	
}

CDMAttribute& CDM::getAttribute(std::string varName, std::string attrName) throw(CDMException)
{
	StrStrAttrMap::iterator varIt = attributes.find(varName); 
	if (varIt != attributes.end()) {
		StrAttrMap::iterator attrIt = (varIt->second).find(attrName);
		if (attrIt != (varIt->second).end()) {
			return attrIt->second;
		} else {
			throw CDMException("Attribute " + attrName + " not found for varName");
		}
	} else {
		throw CDMException("Variable " + varName + " not found");
	}
}



void CDM::toXMLStream(std::ostream& out) const
{
	out << "<cdm>" << std::endl;
	for (std::map<std::string, CDMDimension>::const_iterator it = dimensions.begin(); it != dimensions.end(); ++it) {
		it->second.toXMLStream(out);
	}
	if (attributes.find(globalAttributeNS()) != attributes.end()) {
		const std::map<std::string, CDMAttribute> attrs = attributes.find(globalAttributeNS())->second;
		for (std::map<std::string, CDMAttribute>::const_iterator it = attrs.begin(); it != attrs.end(); ++it) {
			it->second.toXMLStream(out);
		}
	}
	for (std::map<std::string, CDMVariable>::const_iterator it = variables.begin(); it != variables.end(); ++it) {
		if (attributes.find(it->first) != attributes.end()) {
			it->second.toXMLStream(out, attributes.find(it->first)->second);
		} else {
			it->second.toXMLStream(out);
		}
	}
	
	out << "</cdm>" << std::endl;
}


}
