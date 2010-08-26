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
#include "proj_api.h"
#include "fimex/DataImpl.h"
#include "fimex/Units.h"
#include "fimex/coordSys/Projection.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/Logger.h"
#include <boost/bind.hpp>
#include <boost/regex.hpp>
#include <functional>
#include <algorithm>
#include <set>

namespace MetNoFimex
{
static LoggerPtr logger = getLogger("fimex.CDM");

/** Comparator to check if units are comparable to the initialized one */

/**
 * Comparator to check if units is latitude or longitude as of CF-1.2
 * unfortunatley, udunits says degreesE == degreesN, so a string comparison
 * is required
 */
class CDMCompatibleLatLongUnit : public std::unary_function<std::string, bool>
{
	const CDM& cdm;
protected:
	std::set<std::string> compatibleDegrees;
	CDMCompatibleLatLongUnit(const CDM& cdm) : cdm(cdm) {}
public:
	virtual ~CDMCompatibleLatLongUnit() {}
	bool operator() (const std::string& varName) const {
		try {
			const CDMAttribute& unitAttr = cdm.getAttribute(varName, "units");
			std::string testUnit(unitAttr.getData()->asString());
			if (testUnit == "") return false;
			return compatibleDegrees.find(testUnit) != compatibleDegrees.end();
		} catch (CDMException& e) {
			// varName doesn't have units
		}
		return false;
	}
};

class CDMCompatibleLatitudeUnit : public CDMCompatibleLatLongUnit
{
public:
	CDMCompatibleLatitudeUnit(const CDM& cdm) : CDMCompatibleLatLongUnit(cdm) {
		compatibleDegrees.insert("degrees_north");
		compatibleDegrees.insert("degree_north");
		compatibleDegrees.insert("degree_N");
		compatibleDegrees.insert("degrees_N");
		compatibleDegrees.insert("degreeN");
		compatibleDegrees.insert("degreesN");
	}
};

class CDMCompatibleLongitudeUnit : public CDMCompatibleLatLongUnit
{
public:
	CDMCompatibleLongitudeUnit(const CDM& cdm) : CDMCompatibleLatLongUnit(cdm) {
		compatibleDegrees.insert("degrees_east");
		compatibleDegrees.insert("degree_east");
		compatibleDegrees.insert("degree_E");
		compatibleDegrees.insert("degrees_E");
		compatibleDegrees.insert("degreeE");
		compatibleDegrees.insert("degreesE");
	}
};

/** test if attributes string value is comparable to the initialized one */
class CDMAttributeEquals : public std::unary_function<std::string, bool>
{
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

typedef std::vector<boost::shared_ptr<const CoordinateSystem> > CoordSysList;

struct CDMImpl {
    CDM::StrAttrVecMap attributes;
    CDM::VarVec variables;
    CDM::DimVec dimensions;
    bool coordsInitialized;
    CoordSysList coordSystems;
};

static void enhance(CDMImpl* pimpl, const CDM& cdm)
{
    if (!pimpl->coordsInitialized) {
        pimpl->coordSystems = listCoordinateSystems(cdm);
        pimpl->coordsInitialized = true;
    }
}

CDM::CDM() : pimpl_(new CDMImpl())
{
    pimpl_->coordsInitialized = false;
}

CDM::CDM(const CDM& rhs) : pimpl_(new CDMImpl())
{
    *pimpl_ = *rhs.pimpl_;
}

CDM::~CDM()
{
    delete pimpl_;
}

CDM& CDM::operator=(const CDM& rhs)
{
    if (this == &rhs) return *this;
    delete pimpl_;
    pimpl_ = new CDMImpl(*rhs.pimpl_);
    return *this;
}

void CDM::addVariable(const CDMVariable& var) throw(CDMException)
{
    // TODO: check var.dims for existence!!!
    pimpl_->coordsInitialized = false;
	if (!hasVariable(var.getName())) {
		pimpl_->variables.push_back(var);
	} else {
		throw CDMException("cannot add variable: " + var.getName() + " already exists");
	}
}
bool CDM::hasVariable(const std::string& varName) const
{
	return (find_if(pimpl_->variables.begin(), pimpl_->variables.end(), CDMNameEqual(varName)) != pimpl_->variables.end());
}

const CDMVariable& CDM::getVariable(const std::string& varName) const throw(CDMException)
{
	VarVec::const_iterator varPos = find_if(pimpl_->variables.begin(), pimpl_->variables.end(), CDMNameEqual(varName));
	if (varPos != pimpl_->variables.end()) {
		return *varPos;
	} else {
		throw CDMException("cannot find variable: " + varName);
	}
}
CDMVariable& CDM::getVariable(const std::string& varName) throw(CDMException)
{
	// call constant version and cast
	return const_cast<CDMVariable&>(
			static_cast<const CDM&>(*this).getVariable(varName)
			);
}

std::vector<std::string> CDM::findVariables(const std::string& attrName, const std::string& attrValueRegExp) const
{
	std::map<std::string, std::string> findAttributes;
	findAttributes[attrName] = attrValueRegExp;
	std::vector<std::string> dims;
	return findVariables(findAttributes, dims);
}

bool CDM::renameVariable(const std::string& oldName, const std::string& newName)
{
    pimpl_->coordsInitialized = false;
    // change variable in VarVec variables and variableNames as keys in StrAttrVecMap attributes
    try {
        removeVariable(newName); // make sure none of the same name exists
        CDMVariable& var = getVariable(oldName);
        var.setName(newName);
        // working in places, addVariable(newName); not needed
        std::vector<CDMAttribute> attrs = getAttributes(oldName);
        for (std::vector<CDMAttribute>::iterator it = attrs.begin(); it != attrs.end(); ++it) {
            addAttribute(newName, *it);
        }
        removeVariable(oldName);
        return true;
    } catch (CDMException& ex) {
        // variable not found, return false
        return false;
    }
    return false;
}

bool CDM::checkVariableAttribute(const std::string& varName, const std::string& attribute, const boost::regex& attrValue) const
{
	StrAttrVecMap::const_iterator varIt = pimpl_->attributes.find(varName);
	if (varIt != pimpl_->attributes.end()) {
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
class VariableDimensionCheck : public std::unary_function<std::string, bool>
{
	const CDMVariable& variable;
public:
	VariableDimensionCheck(const CDMVariable& var) : variable(var) {}
	bool operator() (const std::string& dim) const { return variable.checkDimension(dim); }
};

/** object-function for checkVariableAttribute */
class VariableAttributeCheck : public std::unary_function<std::pair<std::string, boost::regex>, bool>
{
	const CDM& cdm;
	const std::string& varName;
public:
	VariableAttributeCheck(const CDM& cdm, const std::string& varName) : cdm(cdm), varName(varName) { };
	bool operator() (const std::pair<std::string, boost::regex>& attrRegex) const { return cdm.checkVariableAttribute(varName, attrRegex.first, attrRegex.second); }
};

std::vector<std::string> CDM::findVariables(const std::map<std::string, std::string>& findAttributes, const std::vector<std::string>& findDimensions) const
{
	std::vector<std::string> results;
	// precalc regexp
	std::map<std::string, boost::regex> attrRegExps;
	for (std::map<std::string, std::string>::const_iterator attrIt = findAttributes.begin(); attrIt != findAttributes.end(); ++attrIt) {
		attrRegExps[attrIt->first] = boost::regex(attrIt->second);
	}
	for (VarVec::const_iterator varIt = pimpl_->variables.begin(); varIt != pimpl_->variables.end(); ++varIt) {
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


void CDM::removeVariable(const std::string& variableName)
{
    pimpl_->coordsInitialized = false;
	pimpl_->variables.erase(remove_if(pimpl_->variables.begin(), pimpl_->variables.end(), CDMNameEqual(variableName)), pimpl_->variables.end());
    pimpl_->attributes.erase(variableName);
}


void CDM::addDimension(const CDMDimension& dim) throw(CDMException)
{
    pimpl_->coordsInitialized = false;
	if (!hasDimension(dim.getName())) {
		pimpl_->dimensions.push_back(dim);
	} else {
		throw CDMException("cannot add dimension: " + dim.getName() + " already exists");
	}
}

bool CDM::hasDimension(const std::string& dimName) const
{
	return (find_if(pimpl_->dimensions.begin(), pimpl_->dimensions.end(), CDMNameEqual(dimName)) != pimpl_->dimensions.end());
}

const CDMDimension& CDM::getDimension(const std::string& dimName) const throw(CDMException)
{
	DimVec::const_iterator dimIt = find_if(pimpl_->dimensions.begin(), pimpl_->dimensions.end(), CDMNameEqual(dimName));
	if (dimIt != pimpl_->dimensions.end()) {
		return *dimIt;
	} else {
		throw CDMException("cannot find dimension: " + dimName);
	}
}

CDMDimension& CDM::getDimension(const std::string& dimName) throw(CDMException)
{
	return const_cast<CDMDimension&>(
			static_cast<const CDM&>(*this).getDimension(dimName)
			);
}

bool CDM::testDimensionInUse(const std::string& name) const
{
    if (!hasDimension(name)) return false;
    bool inUse = false;
    for (VarVec::const_iterator it = pimpl_->variables.begin(); it != pimpl_->variables.end(); ++it) {
        if (it->checkDimension(name)) {
            inUse = true;
        }
    }
    return inUse;
}

bool CDM::renameDimension(const std::string& oldName, const std::string& newName) throw(CDMException)
{
    pimpl_->coordsInitialized = false;
    if (!hasDimension(oldName)) return false;
    if (hasDimension(newName)) {
        if (testDimensionInUse(newName)) {
            throw CDMException("Cannot rename dimension to "+newName+". Name already in use.");
        } else {
            removeDimension(newName);
        }
    }
    CDMDimension& dim = getDimension(oldName);
    dim.setName(newName);
    /* change the shape of all variables having the dimensions */
    for (VarVec::iterator it = pimpl_->variables.begin(); it != pimpl_->variables.end(); ++it) {
        if (it->checkDimension(oldName)) {
            std::vector<std::string> shape = it->getShape();
            for (size_t i = 0; i < shape.size(); i++) {
                if (shape[i] == oldName) {
                    shape[i] = newName;
                }
            }
            it->setShape(shape);
        }
    }
    return true;
}

bool CDM::removeDimension(const std::string& name) throw(CDMException)
{
    bool didErase = false;
    if (testDimensionInUse(name)) {
        throw CDMException("Cannot remove dimension "+name+". Dimension in use.");
    } else {
        DimVec::iterator it = remove_if(pimpl_->dimensions.begin(), pimpl_->dimensions.end(), CDMNameEqual(name));
        if (it != pimpl_->dimensions.end()) {
            pimpl_->dimensions.erase(it, pimpl_->dimensions.end());
            didErase = true;
        }
    }
    if (didErase) pimpl_->coordsInitialized = false;
    return didErase;
}


const CDMDimension* CDM::getUnlimitedDim() const
{
	DimVec::const_iterator it = find_if(pimpl_->dimensions.begin(), pimpl_->dimensions.end(), std::mem_fun_ref(&CDMDimension::isUnlimited));
	if (it == pimpl_->dimensions.end()) {
		return 0;
	} else {
		return &(*it);
	}
}

bool CDM::hasUnlimitedDim(const CDMVariable& var) const
{
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
    pimpl_->coordsInitialized = false;
	if ((varName != globalAttributeNS ()) && !hasVariable(varName)) {
		throw CDMException("cannot add attribute: variable " + varName + " does not exist");
	} else {
		AttrVec& attrVec = pimpl_->attributes[varName];
		if (find_if(attrVec.begin(), attrVec.end(), CDMNameEqual(attr.getName())) == attrVec.end()) {
			attrVec.push_back(attr);
		} else {
			throw CDMException("cannot add attribute: attribute " + varName + "." + attr.getName() + " already exists");
		}
	}
}

void CDM::addOrReplaceAttribute(const std::string& varName, const CDMAttribute& attr) throw(CDMException)
{
    pimpl_->coordsInitialized = false;
	if ((varName != globalAttributeNS ()) && !hasVariable(varName)) {
		throw CDMException("cannot add attribute: variable " + varName + " does not exist");
	} else {
		removeAttribute(varName, attr.getName());
		pimpl_->attributes[varName].push_back(attr);
	}
}

void CDM::removeAttribute(const std::string& varName, const std::string& attrName)
{
    pimpl_->coordsInitialized = false;
	StrAttrVecMap::iterator varIt = pimpl_->attributes.find(varName);
	if (varIt != pimpl_->attributes.end()) {
		AttrVec& attrVec = varIt->second;
		attrVec.erase(remove_if(attrVec.begin(), attrVec.end(), CDMNameEqual(attrName)), attrVec.end());
	}
}


const CDMAttribute& CDM::getAttribute(const std::string& varName, const std::string& attrName) const throw(CDMException)
{
	StrAttrVecMap::const_iterator varIt = pimpl_->attributes.find(varName);
	if (varIt != pimpl_->attributes.end()) {
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
CDMAttribute& CDM::getAttribute(const std::string& varName, const std::string& attrName) throw(CDMException)
{
	return const_cast<CDMAttribute&>(
			static_cast<const CDM&>(*this).getAttribute(varName, attrName)
			);
}

bool CDM::getAttribute(const std::string& varName, const std::string& attrName, CDMAttribute& retAttribute) const
{
	try {
		retAttribute = getAttribute(varName, attrName);
	} catch (CDMException e) {
		return false;
	}
	return true;
}

std::vector<CDMAttribute> CDM::getAttributes(const std::string& varName) const
{
	std::vector<CDMAttribute> results;
	StrAttrVecMap::const_iterator varIt = pimpl_->attributes.find(varName);
	if (varIt != pimpl_->attributes.end()) {
		results.insert(results.begin(), varIt->second.begin(), varIt->second.end());
	}
	return results;
}

double CDM::getFillValue(const std::string& varName) const
{
    CDMAttribute attr;
    if (getAttribute(varName, "_FillValue", attr)) {
        return attr.getData()->asDouble()[0];
    }
	return MIFI_UNDEFINED_F;
}

std::string CDM::getUnits(const std::string& varName) const
{
    CDMAttribute attr;
    getAttribute(varName, "units", attr); // no test, default attr-value is ""
    return attr.getStringValue();
}

void CDM::toXMLStream(std::ostream& out) const
{
	out << "<cdm>" << std::endl;
	for (DimVec::const_iterator it = pimpl_->dimensions.begin(); it != pimpl_->dimensions.end(); ++it) {
		it->toXMLStream(out);
	}
	if (pimpl_->attributes.find(globalAttributeNS()) != pimpl_->attributes.end()) {
		const AttrVec& attrs = pimpl_->attributes.find(globalAttributeNS())->second;
		for (AttrVec::const_iterator it = attrs.begin(); it != attrs.end(); ++it) {
			it->toXMLStream(out);
		}
	}
	for (VarVec::const_iterator it = pimpl_->variables.begin(); it != pimpl_->variables.end(); ++it) {
		if (pimpl_->attributes.find(it->getName()) != pimpl_->attributes.end()) {
			it->toXMLStream(out, pimpl_->attributes.find(it->getName())->second);
		} else {
			it->toXMLStream(out);
		}
	}

	out << "</cdm>" << std::endl;
}

const CDM::DimVec& CDM::getDimensions() const
{
    return pimpl_->dimensions;
}
const CDM::VarVec& CDM::getVariables() const
{
    return pimpl_->variables;
}
const CDM::StrAttrVecMap& CDM::getAttributes() const
{
    return pimpl_->attributes;
}


// TODO: in CF: projection belongs to variable, not to file!!
bool CDM::getProjectionAndAxesUnits(std::string& projectionName, std::string& xAxis, std::string& yAxis, std::string& xAxisUnits, std::string& yAxisUnits) const throw(CDMException)
{
	bool retVal = true;
	projectionName = "latitude_longitude"; // default
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
	if (projectionName == "latitude_longitude") {
		std::vector<std::string> shape;
		std::transform(getDimensions().begin(), getDimensions().end(), std::back_inserter(shape), std::mem_fun_ref(&CDMDimension::getName));
		std::vector<std::string>::iterator sit = find_if(shape.begin(), shape.end(), CDMCompatibleLongitudeUnit(*this));
		if (sit == shape.end()) {
			throw CDMException("couldn't find projection longitude axis for projection " + projectionName);
		} else {
			xAxis = *sit;
		}
		sit = find_if(shape.begin(), shape.end(), CDMCompatibleLatitudeUnit(*this));
		if (sit == shape.end()) {
			throw CDMException("couldn't find projection axis with latitue axis for projection " + projectionName);
		} else {
			yAxis = *sit;
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

void CDM::generateProjectionCoordinates(const std::string& projectionVariable, const std::string& xDim, const std::string& yDim, const std::string& lonDim, const std::string& latDim) throw(CDMException)
{
	const CDMVariable& xVar = getVariable(xDim);
	const CDMVariable& yVar = getVariable(yDim);
	boost::shared_array<double> xData = xVar.getData()->asDouble();
	boost::shared_array<double> yData = yVar.getData()->asDouble();
	size_t xDimLength = getDimension(xDim).getLength();
	size_t yDimLength = getDimension(yDim).getLength();
	assert(xDimLength == xVar.getData()->size());
	assert(yDimLength == yVar.getData()->size());
	std::string xUnits = getAttribute(xDim, "units").getData()->asString();
	if (boost::regex_match(xUnits, boost::regex(".*degree.*"))) {
		// convert degrees to radians
		for (size_t i = 0; i < xDimLength; ++i) {
			xData[i] *= DEG_TO_RAD;
		}
	}
	std::string yUnits = getAttribute(yDim, "units").getData()->asString();;
	if (boost::regex_match(yUnits, boost::regex(".*degree.*"))) {
		// convert degrees to radians
		for (size_t i = 0; i < yDimLength; ++i) {
			yData[i] *= DEG_TO_RAD;
		}
	}
	size_t fieldSize = xDimLength * yDimLength;
	boost::shared_array<double> longVal(new double[fieldSize]);
	boost::shared_array<double> latVal(new double[fieldSize]);
	std::string lonLatProj("+ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0 +proj=latlong");
	std::string projStr = Projection::create(getAttributes(projectionVariable))->getProj4String();
	LOG4FIMEX(logger, Logger::DEBUG, "generating lat(x,y),lon(x,y) using proj4: "+projStr);
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
	lonVar.setData(createData(CDM_DOUBLE, &longVal[0], &longVal[fieldSize]));
	CDMVariable latVar(latDim, CDM_DOUBLE, xyDims);
	latVar.setData(createData(CDM_DOUBLE, &latVal[0], &latVal[fieldSize]));
	addVariable(lonVar);
	addVariable(latVar);
	// add the coordinate attributes
	addAttribute(lonVar.getName(),CDMAttribute("units", "degree_east"));
	addAttribute(lonVar.getName(),CDMAttribute("long_name", "longitude"));
	addAttribute(lonVar.getName(),CDMAttribute("standard_name", "longitude"));
	addAttribute(latVar.getName(),CDMAttribute("units", "degree_north"));
	addAttribute(latVar.getName(),CDMAttribute("long_name", "latitude"));
	addAttribute(latVar.getName(),CDMAttribute("standard_name", "latitude"));
}


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

		if (find_if(shape.begin(), shape.end(), CDMCompatibleLongitudeUnit(*this)) != shape.end()) {
			if (find_if(shape.begin(), shape.end(), CDMCompatibleLatitudeUnit(*this)) != shape.end()) {
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

boost::shared_ptr<const Projection> CDM::getProjectionOf(std::string varName) const
{
    enhance(this->pimpl_, *this);
    const CoordSysList& csList = pimpl_->coordSystems;
    CoordSysList::const_iterator varSysIt = find_if(csList.begin(), csList.end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt == csList.end()) {
        return boost::shared_ptr<Projection>();
    }
    return (*varSysIt)->getProjection();
}

std::string CDM::getHorizontalXAxis(std::string varName) const
{
    enhance(this->pimpl_, *this);

    const CoordSysList& csList = pimpl_->coordSystems;
    CoordSysList::const_iterator varSysIt = find_if(csList.begin(), csList.end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt == csList.end()) {
        return "";
    }
    CoordinateSystem::ConstAxisPtr axis = (*varSysIt)->getGeoXAxis();
    return (axis.get() == 0) ? "" : axis->getName();
}
std::string CDM::getHorizontalYAxis(std::string varName) const
{
    enhance(this->pimpl_, *this);

    const CoordSysList& csList = pimpl_->coordSystems;
    CoordSysList::const_iterator varSysIt = find_if(csList.begin(), csList.end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt == csList.end()) {
        return "";
    }
    CoordinateSystem::ConstAxisPtr axis = (*varSysIt)->getGeoYAxis();
    return (axis.get() == 0) ? "" : axis->getName();
}

bool CDM::getLatitudeLongitude(std::string varName, std::string& latitude, std::string& longitude) const
{
    enhance(this->pimpl_, *this);

    const CoordSysList& csList = pimpl_->coordSystems;
    CoordSysList::const_iterator varSysIt = find_if(csList.begin(), csList.end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt == csList.end()) {
        return false;
    }
    CoordinateSystem::ConstAxisPtr latAxis = (*varSysIt)->findAxisOfType(CoordinateAxis::Lat);
    CoordinateSystem::ConstAxisPtr lonAxis = (*varSysIt)->findAxisOfType(CoordinateAxis::Lon);
    if (latAxis.get() != 0 && lonAxis.get() != 0) {
        latitude = latAxis->getName();
        longitude = lonAxis->getName();
        return true;
    }
    return false;
}

std::string CDM::getTimeAxis(std::string varName) const
{
    enhance(this->pimpl_, *this);

    const CoordSysList& csList = pimpl_->coordSystems;
    CoordSysList::const_iterator varSysIt = find_if(csList.begin(), csList.end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt == csList.end()) {
        return "";
    }
    CoordinateSystem::ConstAxisPtr axis = (*varSysIt)->getTimeAxis();
    return (axis.get() == 0) ? "" : axis->getName();
}

std::string CDM::getVerticalAxis(std::string varName) const
{
    enhance(this->pimpl_, *this);

    const CoordSysList& csList = pimpl_->coordSystems;
    CoordSysList::const_iterator varSysIt = find_if(csList.begin(), csList.end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt == csList.end()) {
        return "";
    }
    CoordinateSystem::ConstAxisPtr axis = (*varSysIt)->getGeoZAxis();
    return (axis.get() == 0) ? "" : axis->getName();
}


}
