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
#include "fimex/interpolation.h"
#include "fimex/DataImpl.h"
#include "fimex/Units.h"
#include <boost/bind.hpp>
#include <boost/regex.hpp>
#include <functional>
#include <algorithm>

namespace MetNoFimex
{

CDM::CDM()
{
}

CDM::~CDM()
{
}

void CDM::addVariable(const CDMVariable& var) throw(CDMException)
{
	if (!hasVariable(var.getName())) {
		variables.push_back(var);
	} else {
		throw CDMException("cannot add variable: " + var.getName() + " already exists");
	}
}
bool CDM::hasVariable(const std::string& varName) const
{
	return (find_if(variables.begin(), variables.end(), CDMNameEqual(varName)) != variables.end());
}

const CDMVariable& CDM::getVariable(const std::string& varName) const throw(CDMException) {
	VarVec::const_iterator varPos = find_if(variables.begin(), variables.end(), CDMNameEqual(varName));
	if (varPos != variables.end()) {
		return *varPos;
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

std::vector<std::string> CDM::findVariables(const std::string& attrName, const std::string& attrValueRegExp) const {
	std::map<std::string, std::string> findAttributes;
	findAttributes[attrName] = attrValueRegExp;
	std::vector<std::string> dims;
	return findVariables(findAttributes, dims);
}

bool CDM::checkVariableAttribute(const std::string& varName, const std::string& attribute, const boost::regex& attrValue) const {
	StrAttrVecMap::const_iterator varIt = attributes.find(varName);
	if (varIt != attributes.end()) {
		AttrVec::const_iterator attrIt = find_if(varIt->second.begin(), varIt->second.end(), CDMNameEqual(attribute));
		if (attrIt != varIt->second.end()) {
			boost::smatch what;
			if (boost::regex_match(attrIt->getStringValue(), what, attrValue)) {
				return true;
			}
		}
	}
	return false;
}

/** object function for CDMVariable::checkDimension  (problems with boost::bind and std::not1, boost v 1.32 has no ! operator)*/
class VariableDimensionCheck : public std::unary_function<std::string, bool> {
	const CDMVariable& variable;
public:
	VariableDimensionCheck(const CDMVariable& var) : variable(var) {}
	bool operator() (const std::string& dim) const { return variable.checkDimension(dim); }
};

/** object-function for checkVariableAttribute */
class VariableAttributeCheck : public std::unary_function<std::pair<std::string, boost::regex>, bool> {
	const CDM& cdm;
	const std::string& varName;
public:
	VariableAttributeCheck(const CDM& cdm, const std::string& varName) : cdm(cdm), varName(varName) { };
	bool operator() (const std::pair<std::string, boost::regex>& attrRegex) const { return cdm.checkVariableAttribute(varName, attrRegex.first, attrRegex.second); }
};

std::vector<std::string> CDM::findVariables(const std::map<std::string, std::string>& findAttributes, const std::vector<std::string>& findDimensions) const {
	std::vector<std::string> results;
	// precalc regexp
	std::map<std::string, boost::regex> attrRegExps;
	for (std::map<std::string, std::string>::const_iterator attrIt = findAttributes.begin(); attrIt != findAttributes.end(); ++attrIt) {
		attrRegExps[attrIt->first] = boost::regex(attrIt->second);
	}
	for (VarVec::const_iterator varIt = variables.begin(); varIt != variables.end(); ++varIt) {
		// test if all attributes are found in variable (find_if finds the first not found)
		if (find_if(attrRegExps.begin(), attrRegExps.end(), std::not1(VariableAttributeCheck(*this, varIt->getName()))) == attrRegExps.end()) {
			// test if all dimensions are found in variable (find_if finds the first not found)
			if (find_if(findDimensions.begin(), findDimensions.end(), std::not1(VariableDimensionCheck(*varIt))) == findDimensions.end()) {
				results.push_back(varIt->getName());
			}
		}
	}
	return results;
}


void CDM::removeVariable(const std::string& variableName) {
	VarVec::iterator newEnd = remove_if(variables.begin(), variables.end(), CDMNameEqual(variableName));
	if (newEnd != variables.end()) {
		variables.erase(newEnd, variables.end());
		StrAttrVecMap::iterator varAttrPos = attributes.find(variableName);
		if (varAttrPos != attributes.end()) {
			attributes.erase(varAttrPos);
		}
	}
}


void CDM::addDimension(const CDMDimension& dim) throw(CDMException)
{
	if (!hasDimension(dim.getName())) {
		dimensions.push_back(dim);
	} else {
		throw CDMException("cannot add dimension: " + dim.getName() + " already exists");
	}
}

bool CDM::hasDimension(const std::string& dimName) const
{
	return (find_if(dimensions.begin(), dimensions.end(), CDMNameEqual(dimName)) != dimensions.end());
}

const CDMDimension& CDM::getDimension(const std::string& dimName) const throw(CDMException)
{
	DimVec::const_iterator dimIt = find_if(dimensions.begin(), dimensions.end(), CDMNameEqual(dimName));
	if (dimIt != dimensions.end()) {
		return *dimIt;
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
	DimVec::const_iterator it = find_if(dimensions.begin(), dimensions.end(), std::mem_fun_ref(&CDMDimension::isUnlimited));
	if (it == dimensions.end()) {
		return 0;
	} else {
		return &(*it);
	}
}

bool CDM::hasUnlimitedDim(const CDMVariable& var) const {
	const std::vector<std::string>& shape = var.getShape();
	const CDMDimension* unlimDim = getUnlimitedDim();
	if (unlimDim == 0) {
		return false;
	} else {
		return (find(shape.begin(), shape.end(), unlimDim->getName()) != shape.end());
	}
}


void CDM::addAttribute(const std::string& varName, const CDMAttribute& attr) throw(CDMException)
{
	if ((varName != globalAttributeNS ()) && !hasVariable(varName)) {
		throw CDMException("cannot add attribute: variable " + varName + " does not exist");
	} else {
		AttrVec& attrVec = attributes[varName];
		if (find_if(attrVec.begin(), attrVec.end(), CDMNameEqual(attr.getName())) == attrVec.end()) {
			attrVec.push_back(attr);
		} else {
			throw CDMException("cannot add attribute: attribute " + varName + "." + attr.getName() + " already exists");
		}
	}
}

void CDM::addOrReplaceAttribute(const std::string& varName, const CDMAttribute& attr) throw(CDMException)
{
	if ((varName != globalAttributeNS ()) && !hasVariable(varName)) {
		throw CDMException("cannot add attribute: variable " + varName + " does not exist");
	} else {
		removeAttribute(varName, attr.getName());
		attributes[varName].push_back(attr);
	}
}

void CDM::removeAttribute(const std::string& varName, const std::string& attrName)
{
	StrAttrVecMap::iterator varIt = attributes.find(varName);
	if (varIt != attributes.end()) {
		AttrVec& attrVec = varIt->second;
		attrVec.erase(remove_if(attrVec.begin(), attrVec.end(), CDMNameEqual(attrName)), attrVec.end());
	}
}


const CDMAttribute& CDM::getAttribute(const std::string& varName, const std::string& attrName) const throw(CDMException)
{
	StrAttrVecMap::const_iterator varIt = attributes.find(varName);
	if (varIt != attributes.end()) {
		AttrVec::const_iterator attrIt = find_if(varIt->second.begin(), varIt->second.end(), CDMNameEqual(attrName));
		if (attrIt != (varIt->second).end()) {
			return *attrIt;
		} else {
			throw CDMException("Attribute " + attrName + " not found for variable: " + varName);
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

std::vector<CDMAttribute> CDM::getAttributes(const std::string& varName) const {
	std::vector<CDMAttribute> results;
	StrAttrVecMap::const_iterator varIt = attributes.find(varName);
	if (varIt != attributes.end()) {
		results.insert(results.begin(), varIt->second.begin(), varIt->second.end());
	}
	return results;
}

double CDM::getFillValue(const std::string& varName) const {
	try {
		const CDMAttribute& attr = getAttribute(varName, "_FillValue");
		return attr.getData()->asDouble()[0];
	} catch (CDMException& ex) {

	}
	return MIFI_UNDEFINED_F;
}

void CDM::toXMLStream(std::ostream& out) const
{
	out << "<cdm>" << std::endl;
	for (DimVec::const_iterator it = dimensions.begin(); it != dimensions.end(); ++it) {
		it->toXMLStream(out);
	}
	if (attributes.find(globalAttributeNS()) != attributes.end()) {
		const AttrVec& attrs = attributes.find(globalAttributeNS())->second;
		for (AttrVec::const_iterator it = attrs.begin(); it != attrs.end(); ++it) {
			it->toXMLStream(out);
		}
	}
	for (VarVec::const_iterator it = variables.begin(); it != variables.end(); ++it) {
		if (attributes.find(it->getName()) != attributes.end()) {
			it->toXMLStream(out, attributes.find(it->getName())->second);
		} else {
			it->toXMLStream(out);
		}
	}

	out << "</cdm>" << std::endl;
}

bool CDM::getProjectionAndAxesUnits(std::string& projectionName, std::string& xAxis, std::string& yAxis, std::string& xAxisUnits, std::string& yAxisUnits) const throw(CDMException) {
	bool retVal = true;
	projectionName = "latlong"; // default
	std::vector<std::string> projs = findVariables("grid_mapping_name", ".*");
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
		dims = findVariables("units", longUnits);
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
		dims = findVariables("units", latUnits);
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
		std::string orgProjName = getAttribute(projectionName, "grid_mapping_name").getStringValue();
		std::string xStandardName("projection_x_coordinate");
		std::string yStandardName("projection_y_coordinate");
		if (orgProjName == "rotated_latitude_longitude") {
			xStandardName = "grid_longitude";
			yStandardName = "grid_latitude";
		}

		dims = findVariables("standard_name", xStandardName);
		if (dims.empty()) {
			throw CDMException("couldn't find projection axis with standard_name "+ xStandardName + " for projection " + projectionName + ": " + orgProjName);
		} else {
			xAxis = dims[0];
			if (dims.size() > 1) {
				retVal = false;
				std::cerr << "found several dimensions with standard_name "
						<< xStandardName << ", using " << xAxis << std::endl;
			}
		}
		dims = findVariables("standard_name", yStandardName);
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
	// units and values
	xAxisUnits = getAttribute(xAxis, "units").getStringValue();
	yAxisUnits = getAttribute(yAxis, "units").getStringValue();

	return retVal;
}

void CDM::generateProjectionCoordinates(const std::string& projectionVariable, const std::string& xDim, const std::string& yDim, const std::string& lonDim, const std::string& latDim) throw(CDMException) {
	const CDMVariable& xVar = getVariable(xDim);
	const CDMVariable& yVar = getVariable(yDim);
	boost::shared_array<double> xData = xVar.getData()->asDouble();
	boost::shared_array<double> yData = yVar.getData()->asDouble();
	std::string xUnits = getAttribute(xDim, "units").getData()->asString();
	if (boost::regex_match(xUnits, boost::regex(".*degree.*"))) {
		// convert degrees to radians
		for (size_t i = 0; i < xVar.getData()->size(); ++i) {
			xData[i] *= DEG_TO_RAD;
		}
	}
	std::string yUnits = getAttribute(yDim, "units").getData()->asString();;
	if (boost::regex_match(yUnits, boost::regex(".*degree.*"))) {
		// convert degrees to radians
		for (size_t i = 0; i < yVar.getData()->size(); ++i) {
			yData[i] *= DEG_TO_RAD;
		}
	}
	size_t xDimLength = getDimension(xDim).getLength();
	size_t yDimLength = getDimension(yDim).getLength();
	size_t fieldSize = xDimLength * yDimLength;
	boost::shared_array<double> longVal(new double[fieldSize]);
	boost::shared_array<double> latVal(new double[fieldSize]);
	std::string lonLatProj("+elips=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0 +proj=latlong");
	std::string projStr = attributesToProjString(getAttributes(projectionVariable));
	if (MIFI_OK != mifi_project_axes(projStr.c_str(),lonLatProj.c_str(), xData.get(), yData.get(), xDimLength, yDimLength, longVal.get(), latVal.get())) {
		throw CDMException("unable to project axes from "+projStr+ " to " +lonLatProj);
	}
	// converting to Degree
	double* longPos = longVal.get();
	double* latPos = latVal.get();
	for (size_t i = 0; i < fieldSize; ++i, ++latPos, ++longPos) {
		*longPos *= RAD_TO_DEG;
		*latPos  *= RAD_TO_DEG;
	}
	std::vector<std::string> xyDims;
	xyDims.push_back(xDim);
	xyDims.push_back(yDim);
	CDMVariable lonVar(lonDim, CDM_DOUBLE, xyDims);
	lonVar.setData(createData(CDM_DOUBLE, fieldSize, &longVal[0], &longVal[fieldSize]));
	CDMVariable latVar(latDim, CDM_DOUBLE, xyDims);
	latVar.setData(createData(CDM_DOUBLE, fieldSize, &latVal[0], &latVal[fieldSize]));
	addVariable(lonVar);
	addVariable(latVar);
	// TODO: those values should be configurable
	addAttribute(lonVar.getName(),CDMAttribute("units", "degree_east"));
	addAttribute(lonVar.getName(),CDMAttribute("long_name", "longitude"));
	addAttribute(lonVar.getName(),CDMAttribute("standard_name", "longitude"));
	addAttribute(latVar.getName(),CDMAttribute("units", "degree_north"));
	addAttribute(latVar.getName(),CDMAttribute("long_name", "latitude"));
	addAttribute(latVar.getName(),CDMAttribute("standard_name", "latitude"));
}

class CDMCompatibleUnit : public std::unary_function<std::string, bool> {
	const CDM& cdm;
	const std::string& unitString;
	Units units;
public:
	CDMCompatibleUnit(const CDM& cdm, const std::string& unitString) : cdm(cdm), unitString(unitString) {}
	bool operator() (const std::string& varName) const
	{
		try {
			const CDMAttribute& unitAttr = cdm.getAttribute(varName, "units");
			std::string testUnit(unitAttr.getData()->asString());
			if (testUnit == "") return false;
			return units.areConvertible(unitString, testUnit);
		} catch (CDMException& ex) {
			// nothing to do
		}
		return false;
	}
};

class CDMCompatibleTime : public std::unary_function<std::string, bool> {
	const CDM& cdm;
	Units units;
public:
	CDMCompatibleTime(const CDM& cdm) : cdm(cdm) {}
	bool operator() (const std::string& varName) const
	{
		try {
			const CDMAttribute& unitAttr = cdm.getAttribute(varName, "units");
			std::string testUnit(unitAttr.getData()->asString());
			if (testUnit == "") return false;
			return units.isTime(testUnit);
		} catch (CDMException& ex) {
			// nothing to do
		}
		return false;
	}
};

class CDMAttributeEquals : public std::unary_function<std::string, bool> {
	const CDM& cdm;
	const std::string& attrName;
	boost::regex attrRegex;
public:
	CDMAttributeEquals(const CDM& cdm, const std::string& attrName, const std::string& attrValue) : cdm(cdm), attrName(attrName) {attrRegex = boost::regex("\\Q"+attrValue+"\\E");}
	CDMAttributeEquals(const CDM& cdm, const std::string& attrName, boost::regex attrValue) : cdm(cdm), attrName(attrName), attrRegex(attrValue) {}
	bool operator() (const std::string& varName) const
	{
		try {
			const CDMAttribute& unitAttr = cdm.getAttribute(varName, attrName);
			std::string testAttrVal(unitAttr.getData()->asString());
			return boost::regex_match(testAttrVal, attrRegex);
		} catch (CDMException& ex) {
			// nothing to do
		}
		return false;
	}
};


CDM::AttrVec CDM::getProjection(std::string varName) const
{
	CDM::AttrVec retVal;
	const AttrVec& attributes = getAttributes(varName);
	AttrVec::const_iterator ait = find_if(attributes.begin(), attributes.end(), CDMNameEqual("grid_mapping"));
	if (ait != attributes.end()) {
		std::string gridMapping = ait->getData()->asString();
		retVal = getAttributes(gridMapping);
	} else {
		// no projection, maybe longitude latitude?
		const std::vector<std::string>& shape = getVariable(varName).getShape();

		if (find_if(shape.begin(), shape.end(), CDMCompatibleUnit(*this, "degree_east")) != shape.end()) {
			if (find_if(shape.begin(), shape.end(), CDMCompatibleUnit(*this, "degree_north")) != shape.end()) {
				// longitude and latitude found
				// using CF-1.2 draft attribute for declaration of sphere
				retVal.push_back(CDMAttribute("grid_mapping_name", "latitude_longitude"));
				retVal.push_back(CDMAttribute("semi_major_axis", MIFI_EARTH_RADIUS_M));
				retVal.push_back(CDMAttribute("inverse_flattening", 0));
			}
		}
	}
	return retVal;
}


std::string CDM::getHorizontalXAxis(std::string varName) const
{
	std::string retVal;
	const std::vector<std::string>& shape = getVariable(varName).getShape();
	std::vector<std::string>::const_iterator shapeIt = find_if(shape.begin(), shape.end(), CDMAttributeEquals(*this, "standard_name", "projection_x_coordinate"));
	if (shapeIt != shape.end()) {
		retVal = *shapeIt;
	} else {
		shapeIt = find_if(shape.begin(), shape.end(), CDMAttributeEquals(*this, "standard_name", "grid_longitude"));
		if (shapeIt != shape.end()) {
			retVal = *shapeIt;
		} else {
			// longitude
			shapeIt = find_if(shape.begin(), shape.end(), CDMCompatibleUnit(*this, "degree_east"));
			if (shapeIt != shape.end()) {
				retVal = *shapeIt;
			}
		}
	}
	return retVal;
}
std::string CDM::getHorizontalYAxis(std::string varName) const
{
	std::string retVal;
	const std::vector<std::string>& shape = getVariable(varName).getShape();
	std::vector<std::string>::const_iterator shapeIt = find_if(shape.begin(), shape.end(), CDMAttributeEquals(*this, "standard_name", "projection_y_coordinate"));
	if (shapeIt != shape.end()) {
		retVal = *shapeIt;
	} else {
		shapeIt = find_if(shape.begin(), shape.end(), CDMAttributeEquals(*this, "standard_name", "grid_latitude"));
		if (shapeIt != shape.end()) {
			retVal = *shapeIt;
		} else {
			// longitude
			shapeIt = find_if(shape.begin(), shape.end(), CDMCompatibleUnit(*this, "degree_west"));
			if (shapeIt != shape.end()) {
				retVal = *shapeIt;
			}
		}
	}
	return retVal;
}

bool CDM::getLatitudeLongitude(std::string varName, std::string& latitude, std::string& longitude) const
{
	try {
		std::string coordinates = getAttribute(varName, "coordinates").getData()->asString();
		std::vector<std::string> tokens = tokenize(coordinates, " ");
		std::vector<std::string>::const_iterator latIt = find_if(tokens.begin(), tokens.end(), CDMCompatibleUnit(*this, "degree_north"));
		if (latIt != tokens.end()) {
			std::vector<std::string>::const_iterator lonIt = find_if(tokens.begin(), tokens.end(), CDMCompatibleUnit(*this, "degree_west"));
			if (lonIt != tokens.end()) {
				latitude = *latIt;
				longitude = *lonIt;
				return true;
			}
		}
	} catch (CDMException& e) {
		// coordinates not found for varName, doesn't matter
	}
	// try the shape axis
	const std::vector<std::string>& shape = getVariable(varName).getShape();
	std::vector<std::string>::const_iterator latIt = find_if(shape.begin(), shape.end(), CDMCompatibleUnit(*this, "degree_north"));
	if (latIt != shape.end()) {
		std::vector<std::string>::const_iterator lonIt = find_if(shape.begin(), shape.end(), CDMCompatibleUnit(*this, "degree_west"));
		if (lonIt != shape.end()) {
			latitude = *latIt;
			longitude = *lonIt;
			return true;
		}
	}
	return false;
}

std::string CDM::getTimeAxis(std::string varName) const
{
	std::string retVal;
	const std::vector<std::string>& shape = getVariable(varName).getShape();
	std::vector<std::string>::const_iterator shapeIt = find_if(shape.begin(), shape.end(), CDMCompatibleTime(*this));
	if (shapeIt != shape.end()) retVal = *shapeIt;
	return retVal;
}

std::string CDM::getVerticalAxis(std::string varName) const
{
	// detect attribute 'positive' = up/down (case insensitive)
	// detect pressure units
	std::string retVal;
	const std::vector<std::string>& shape = getVariable(varName).getShape();
	std::vector<std::string>::const_iterator shapeIt = find_if(shape.begin(), shape.end(), CDMAttributeEquals(*this, "positive", boost::regex("up", boost::regex::perl|boost::regex::icase)));
	if (shapeIt != shape.end()) {
		retVal = *shapeIt;
	} else {
		shapeIt = find_if(shape.begin(), shape.end(), CDMAttributeEquals(*this, "positive", boost::regex("down", boost::regex::perl|boost::regex::icase)));
		if (shapeIt != shape.end()) {
			retVal = *shapeIt;
		} else {
			shapeIt = find_if(shape.begin(), shape.end(), CDMCompatibleUnit(*this, "Pa"));
			if (shapeIt != shape.end()) {
				retVal = *shapeIt;
			}
		}
	}

	return retVal;
}


}
