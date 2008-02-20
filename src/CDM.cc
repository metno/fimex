#include "CDM.h"

namespace MetNoUtplukk
{

CDM::CDM()
{
}

CDM::~CDM()
{
}

void CDM::addVariable(const CDMVariable& var) throw(CDMException)
{
	if (variables.find(var.getName()) == variables.end()) {
		variables.insert(std::pair<std::string, CDMVariable>(var.getName(), var));
	} else {
		throw CDMException("cannot add variable: " + var.getName() + " already exists");
	}
}

const CDMVariable& CDM::getVariable(const std::string& varName) const throw(CDMException) {
	StrVarMap::const_iterator varPos = variables.find(varName); 
	if (varPos != variables.end()) {
		return varPos->second;
	} else {
		throw CDMException("cannot find variable: " + varName);
	}
}
CDMVariable& CDM::getVariable(const std::string& varName) throw(CDMException) {
	// call constant version and cast
	return const_cast<CDMVariable&>(
			static_cast<const CDM&>(*this).getVariable(varName)
			);
}

std::vector<std::string> CDM::findVariable(const std::string& attrName, const std::string& attrValue) const {
	std::vector<std::string> results;
	for (StrStrAttrMap::const_iterator varIt = attributes.begin(); varIt != attributes.end(); ++varIt) {
		if (varIt->first == globalAttributeNS())
			continue;
		StrAttrMap::const_iterator attrIt = varIt->second.find(attrName);
		if (attrIt != varIt->second.end()) {
			if (attrIt->second.getStringValue() == attrValue) {
				results.push_back(varIt->first);
			}
		}
	}
	return results;
}


void CDM::removeVariable(const std::string& variableName) throw(CDMException) {
	StrVarMap::iterator varPos = variables.find(variableName); 
	if (varPos != variables.end()) {
		variables.erase(varPos);
		StrStrAttrMap::iterator varAttrPos = attributes.find(variableName); 
		if (varAttrPos != attributes.end()) {
			attributes.erase(varAttrPos);
		}
	} else {
		throw CDMException("cannot remove variable: " + variableName + " does not exists");
	}
}


void CDM::addDimension(const CDMDimension& dim) throw(CDMException)
{
	if (dimensions.find(dim.getName()) == dimensions.end()) {
		dimensions.insert(std::pair<std::string, CDMDimension>(dim.getName(), dim));
	} else {
		throw CDMException("cannot add dimension: " + dim.getName() + " already exists");
	}
}

const CDMDimension& CDM::getDimension(const std::string& dimName) const throw(CDMException)
{
	StrDimMap::const_iterator dimIt = dimensions.find(dimName); 
	if (dimIt != dimensions.end()) {
		return dimIt->second;
	} else {
		throw CDMException("cannot find dimension: " + dimName);
	}	
}

CDMDimension& CDM::getDimension(const std::string& dimName) throw(CDMException) {
	return const_cast<CDMDimension&>(
			static_cast<const CDM&>(*this).getDimension(dimName)
			);
}


const CDMDimension* CDM::getUnlimitedDim() const {
	for (StrDimMap::const_iterator it = dimensions.begin(); it != dimensions.end(); ++it) {
		if (it->second.isUnlimited()) {
			return &(it->second);
		}
	}
	return 0;
}

bool CDM::hasUnlimitedDim(const CDMVariable& var) const {
	const std::vector<std::string>& shape = var.getShape();
	for (std::vector<std::string>::const_iterator it = shape.begin(); it != shape.end(); ++it) {
		StrDimMap::const_iterator dim = dimensions.find(*it);
		if (dim != dimensions.end()) {
			if (dim->second.isUnlimited()) {
				return true;
			}
		}
	}
	return false;
}


void CDM::addAttribute(const std::string& varName, const CDMAttribute& attr) throw(CDMException)
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
