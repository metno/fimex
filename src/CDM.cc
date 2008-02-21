#include "CDM.h"
#include <boost/regex.hpp>

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

std::vector<std::string> CDM::findVariable(const std::string& attrName, const std::string& attrValueRegExp) const {
	boost::regex valRegExp(attrValueRegExp);
	boost::smatch what;
	std::vector<std::string> results;
	for (StrStrAttrMap::const_iterator varIt = attributes.begin(); varIt != attributes.end(); ++varIt) {
		if (varIt->first == globalAttributeNS())
			continue;
		StrAttrMap::const_iterator attrIt = varIt->second.find(attrName);
		if (attrIt != varIt->second.end()) {
			if (boost::regex_match(attrIt->second.getStringValue(), what, valRegExp)) {
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

const CDMAttribute& CDM::getAttribute(const std::string& varName, const std::string& attrName) const throw(CDMException)
{
	StrStrAttrMap::const_iterator varIt = attributes.find(varName); 
	if (varIt != attributes.end()) {
		StrAttrMap::const_iterator attrIt = (varIt->second).find(attrName);
		if (attrIt != (varIt->second).end()) {
			return attrIt->second;
		} else {
			throw CDMException("Attribute " + attrName + " not found for varName");
		}
	} else {
		throw CDMException("Variable " + varName + " not found");
	}
}
CDMAttribute& CDM::getAttribute(const std::string& varName, const std::string& attrName) throw(CDMException) {
	return const_cast<CDMAttribute&>(
			static_cast<const CDM&>(*this).getAttribute(varName, attrName)
			);	
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

bool getProjectionAndAxesFromCDM(const CDM& cdm, std::string& projectionName, std::string& xAxis, std::string& yAxis) throw(CDMException) {
	bool retVal = true;
	projectionName = "latlong"; // default
	std::vector<std::string> projs = cdm.findVariable("grid_mapping_name", ".*");
	if (projs.empty()) {
		// assuming latlong
	} else {
		projectionName = projs[0];
		if (projs.size() > 1) {
			retVal = false;
			std::cerr << "found several projections, changing " << projs[0] << std::endl;
		}
	}
	// detect original projection axes (x,y,lon,lat,rlat,rlon) (via projection_x/y_coordinate, degrees_east/north, grid_longitude/latitutde)
	std::vector<std::string> dims;
	if (projectionName == "latlong") {
		std::string longUnits("degrees?_(east|west)");
		dims = cdm.findVariable("units", longUnits);
		if (dims.empty()) {
			throw CDMException("couldn't find projection axis with units "+ longUnits + " for projection " + projectionName);
		} else {
			xAxis = dims[0];
			if (dims.size() > 1) {
				retVal = false;
				std::cerr << "found several dimensions with units " << longUnits << ", using " << xAxis << std::endl;  
			}
		}
		std::string latUnits("degrees?_(north|south)");
		dims = cdm.findVariable("units", latUnits);
		if (dims.empty()) {
			throw CDMException("couldn't find projection axis with units "+ latUnits + " for projection " + projectionName);
		} else {
			yAxis = dims[0];
			if (dims.size() > 1) {
				retVal = false;
				std::cerr << "found several dimensions with units " << latUnits << ", using " << yAxis << std::endl;
			}
		}
	} else {
		std::string orgProjName = cdm.getAttribute(projectionName, "grid_mapping_name").getStringValue();
		std::string xStandardName("projection_x_coordinate");
		std::string yStandardName("projection_y_coordinate");
		if (orgProjName == "rotated_latitude_longitude") {
			std::string xStandardName("grid_longitude");
			std::string yStandardName("grid_longitude");
		}
		
		dims = cdm.findVariable("standard_name", xStandardName);
		if (dims.empty()) {
			throw CDMException("couldn't find projection axis with standard_name "+ xStandardName + " for projection " + projectionName);
		} else {
			xAxis = dims[0];
			if (dims.size() > 1) {
				retVal = false;
				std::cerr << "found several dimensions with standard_name "
						<< xStandardName << ", using " << xAxis << std::endl;
			}
		}
		dims = cdm.findVariable("standard_name", yStandardName);
		if (dims.empty()) {
			throw CDMException("couldn't find projection axis with standard_name "+ yStandardName + " for projection " + projectionName);
		} else {
			yAxis = dims[0];
			if (dims.size() > 1) {
				retVal = false;
				std::cerr << "found several dimensions with standard_name "
						<< yStandardName << " using " << yAxis << std::endl;
			}
		}
	}
	return retVal;	
}



}
