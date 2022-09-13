/*
 * Fimex
 *
 * (C) Copyright 2008-2022, met.no
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

#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/Units.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/Projection.h"
#include "fimex/interpolation.h"

#include "reproject.h"

#include <algorithm>
#include <cassert>
#include <functional>
#include <regex>
#include <set>

namespace MetNoFimex
{
static Logger_p logger = getLogger("fimex.CDM");

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
            std::string testUnit = cdm.getUnits(varName);
            if (testUnit.empty())
                return false;
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

struct CDMImpl {
    CDM::StrAttrVecMap attributes;
    CDM::VarVec variables;
    CDM::DimVec dimensions;
    bool coordsInitialized;
    CoordinateSystem_cp_v coordSystems;

    CDMImpl()
        : coordsInitialized(false)
    {
    }
};

static void enhance(CDMImpl* pimpl, const CDM& cdm)
{
    if (!pimpl->coordsInitialized) {
        // TODO: all functions requiring the 'enhance' step should be deprecated
        //       since 'enhancing' cannot be done by the CDM alone and it is a source
        //       for race-conditions...
        pimpl->coordSystems = listCoordinateSystems(cdm);
        pimpl->coordsInitialized = true;
    }
}

CDM::CDM()
    : pimpl_(new CDMImpl())
{
}

CDM::CDM(const CDM& rhs)
    : pimpl_(new CDMImpl(*rhs.pimpl_))
{
}

CDM::~CDM()
{
}

CDM& CDM::operator=(const CDM& rhs)
{
    if (this != &rhs)
        pimpl_.reset(new CDMImpl(*rhs.pimpl_));
    return *this;
}

void CDM::addVariable(const CDMVariable& var)
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

const CDMVariable& CDM::getVariable(const std::string& varName) const
{
    VarVec::const_iterator varPos = find_if(pimpl_->variables.begin(), pimpl_->variables.end(), CDMNameEqual(varName));
    if (varPos != pimpl_->variables.end()) {
        return *varPos;
    } else {
        throw CDMException("cannot find variable: '" + varName + "'");
    }
}
CDMVariable& CDM::getVariable(const std::string& varName)
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
}

bool CDM::checkVariableAttribute(const std::string& varName, const std::string& attribute, const std::regex& attrValue) const
{
    StrAttrVecMap::const_iterator varIt = pimpl_->attributes.find(varName);
    if (varIt != pimpl_->attributes.end()) {
        AttrVec::const_iterator attrIt = find_if(varIt->second.begin(), varIt->second.end(), CDMNameEqual(attribute));
        if (attrIt != varIt->second.end()) {
            std::smatch what;
            const std::string val = attrIt->getStringValue();
            if (std::regex_match(val, what, attrValue))
                return true;
        }
    }
    return false;
}

namespace {

/** object function for CDMVariable::checkDimension */
class VariableDimensionCheck : public std::unary_function<std::string, bool>
{
    const CDMVariable& variable;
public:
    VariableDimensionCheck(const CDMVariable& var) : variable(var) {}
    bool operator() (const std::string& dim) const { return variable.checkDimension(dim); }
};

/** object-function for checkVariableAttribute */
class VariableAttributeCheck : public std::unary_function<std::pair<std::string, std::regex>, bool>
{
    const CDM& cdm;
    const std::string& varName;
public:
    VariableAttributeCheck(const CDM& cdm, const std::string& varName) : cdm(cdm), varName(varName) { }
    bool operator()(const std::pair<std::string, std::regex>& attrRegex) const
    {
        return cdm.checkVariableAttribute(varName, attrRegex.first, attrRegex.second);
    }
};

} // namespace

std::vector<std::string> CDM::findVariables(const std::map<std::string, std::string>& findAttributes, const std::vector<std::string>& findDimensions) const
{
    std::vector<std::string> results;
    // precalc regexp
    std::map<std::string, std::regex> attrRegExps;
    for (const auto fa : findAttributes) {
        attrRegExps[fa.first] = std::regex(fa.second);
    }
    for (const auto& var : pimpl_->variables) {
        // test if all attributes are found in variable (find_if finds the first not found)
        if (find_if(attrRegExps.begin(), attrRegExps.end(), std::not1(VariableAttributeCheck(*this, var.getName()))) == attrRegExps.end()) {
            // test if all dimensions are found in variable (find_if finds the first not found)
            if (find_if(findDimensions.begin(), findDimensions.end(), std::not1(VariableDimensionCheck(var))) == findDimensions.end()) {
                results.push_back(var.getName());
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


void CDM::addDimension(const CDMDimension& dim)
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

const CDMDimension& CDM::getDimension(const std::string& dimName) const
{
    DimVec::const_iterator dimIt = find_if(pimpl_->dimensions.begin(), pimpl_->dimensions.end(), CDMNameEqual(dimName));
    if (dimIt != pimpl_->dimensions.end()) {
        return *dimIt;
    } else {
        throw CDMException("cannot find dimension: " + dimName);
    }
}

CDMDimension& CDM::getDimension(const std::string& dimName)
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

bool CDM::renameDimension(const std::string& oldName, const std::string& newName)
{
    return renameDimension(oldName, newName, false);
}

bool CDM::renameDimension(const std::string& oldName, const std::string& newName, bool ignoreInUse)
{
    pimpl_->coordsInitialized = false;
    if (!hasDimension(oldName)) return false;
    if (hasDimension(newName)) {
        if (!ignoreInUse && testDimensionInUse(newName)) {
            throw CDMException("Cannot rename dimension to "+newName+". Name already in use and overwrite disabled.");
        } else {
            removeDimension(newName, ignoreInUse);
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

bool CDM::removeDimension(const std::string& name)
{
    return removeDimension(name, false);
}

bool CDM::removeDimension(const std::string& name, bool ignoreInUse)
{
    bool didErase = false;
    if (!ignoreInUse && testDimensionInUse(name)) {
        throw CDMException("Cannot remove dimension "+name+". Dimension in use and ignoring this is disabled.");
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
    const CDMDimension* unlimDim = getUnlimitedDim();
    if (!unlimDim)
        return false;

    const std::vector<std::string>& shape = var.getShape();
    return (find(shape.begin(), shape.end(), unlimDim->getName()) != shape.end());
}


void CDM::addAttribute(const std::string& varName, const CDMAttribute& attr)
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

void CDM::addOrReplaceAttribute(const std::string& varName, const CDMAttribute& attr)
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


const CDMAttribute& CDM::getAttribute(const std::string& varName, const std::string& attrName) const
{
    StrAttrVecMap::const_iterator varIt = pimpl_->attributes.find(varName);
    if (varIt == pimpl_->attributes.end())
        throw CDMException("Variable " + varName + " not found");

    AttrVec::const_iterator attrIt = find_if(varIt->second.begin(), varIt->second.end(), CDMNameEqual(attrName));
    if (attrIt == (varIt->second).end())
        throw CDMException("Attribute " + attrName + " not found for variable: " + varName);

    return *attrIt;
}

CDMAttribute& CDM::getAttribute(const std::string& varName, const std::string& attrName)
{
    return const_cast<CDMAttribute&>(
            static_cast<const CDM&>(*this).getAttribute(varName, attrName)
            );
}

bool CDM::getAttribute(const std::string& varName, const std::string& attrName, CDMAttribute& retAttribute) const
{
    StrAttrVecMap::const_iterator varIt = pimpl_->attributes.find(varName);
    if (varIt == pimpl_->attributes.end())
        return false;

    AttrVec::const_iterator attrIt = find_if(varIt->second.begin(), varIt->second.end(), CDMNameEqual(attrName));
    if (attrIt == (varIt->second).end())
        return false;

    retAttribute = *attrIt;
    return true;
}

bool CDM::hasAttribute(const std::string& varName, const std::string& attrName) const
{
    CDMAttribute attr;
    return getAttribute(varName, attrName, attr);
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

static double defaultFillValue_(CDMDataType dt)
{
    switch (dt) {
    case CDM_DOUBLE: return MIFI_FILL_DOUBLE;
    case CDM_FLOAT:  return MIFI_FILL_FLOAT;
    case CDM_INT64:  return MIFI_FILL_INT64;
    case CDM_INT:    return MIFI_FILL_INT;
    case CDM_SHORT:  return MIFI_FILL_SHORT;
    case CDM_CHAR:   return MIFI_FILL_CHAR;
    case CDM_UINT64: return MIFI_FILL_UINT64;
    case CDM_UINT:   return MIFI_FILL_UINT;
    case CDM_USHORT: return MIFI_FILL_USHORT;
    case CDM_UCHAR:  return MIFI_FILL_UCHAR;
    default:    return MIFI_UNDEFINED_D;
    }
}

double CDM::getFillValue(const std::string& varName) const
{
    CDMAttribute attr;
    if (getAttribute(varName, "_FillValue", attr) || getAttribute(varName, "missing_value", attr)) {
        CDMVariable var = getVariable(varName);
        if (var.getDataType() != attr.getDataType())
            throw CDMException("variable '" + varName + "' has type " + datatype2string(var.getDataType())
                               + " and FillValue type " + datatype2string(attr.getDataType()));
        return attr.getData()->asDouble()[0];
    }
    if (hasVariable(varName)) {
        return defaultFillValue_(getVariable(varName).getDataType());
    } else {
        return MIFI_UNDEFINED_D;
    }
}

double CDM::getValidMin(const std::string& varName) const
{
    CDMAttribute attr;
    if (getAttribute(varName, "valid_min", attr)) {
        return attr.getData()->asDouble()[0];
    } else if (getAttribute(varName, "valid_range", attr)) {
        return attr.getData()->asDouble()[0];
    }
    return MIFI_UNDEFINED_D;
}

double CDM::getValidMax(const std::string& varName) const
{
    CDMAttribute attr;
    if (getAttribute(varName, "valid_max", attr)) {
        return attr.getData()->asDouble()[0];
    } else if (getAttribute(varName, "valid_range", attr)) {
        DataPtr data = attr.getData();
        if (data->size() > 1) {
            return data->asDouble()[1];
        }
    }
    return MIFI_UNDEFINED_D;
}

double CDM::getAddOffset(const std::string& varName) const
{
    CDMAttribute attr;
    if (getAttribute(varName, "add_offset", attr)) {
        return attr.getData()->asDouble()[0];
    } else {
        return 0;
    }
}

double CDM::getScaleFactor(const std::string& varName) const
{
    CDMAttribute attr;
    if (getAttribute(varName, "scale_factor", attr)) {
        return attr.getData()->asDouble()[0];
    } else {
        return 1;
    }
}

std::string CDM::getUnits(const std::string& varName) const
{
    CDMAttribute attr;
    if (getAttribute(varName, "units", attr))
        return attr.getStringValue();
    else
        return "";
}

void CDM::toXMLStream(std::ostream& out) const
{
    out << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << std::endl <<
            "<netcdf xmlns=\"http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2\"" << std::endl <<
            "        xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" << std::endl <<
            "        xsi:schemaLocation=\"http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2 http://www.unidata.ucar.edu/schemas/netcdf/ncml-2.2.xsd\">" << std::endl;
    out << std::endl;
    for (const auto& dim : pimpl_->dimensions) {
        dim.toXMLStream(out);
    }
    out << std::endl;
    const auto it_global_attributes = pimpl_->attributes.find(globalAttributeNS());
    if (it_global_attributes != pimpl_->attributes.end()) {
        for (const auto& att : it_global_attributes->second) {
            att.toXMLStream(out);
        }
    }
    for (const auto& var : pimpl_->variables) {
        out << std::endl;
        const auto it_var_attributes = pimpl_->attributes.find(var.getName());
        if (it_var_attributes != pimpl_->attributes.end()) {
            var.toXMLStream(out, it_var_attributes->second);
        } else {
            var.toXMLStream(out);
        }
    }

    out << "</netcdf>" << std::endl;
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


bool CDM::getProjectionAndAxesUnits(std::string& projectionName, std::string& xAxis, std::string& yAxis, std::string& xAxisUnits, std::string& yAxisUnits) const
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
            LOG4FIMEX(logger, Logger::WARN, "found several projections, changing " << projs[0]);
        }
    }
    // detect original projection axes (x,y,lon,lat,rlat,rlon) (via projection_x/y_coordinate, degrees_east/north, grid_longitude/latitude)
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
                LOG4FIMEX(logger, Logger::WARN, "found several dimensions with standard_name " << xStandardName << ", using " << xAxis);
            }
        }
        dims = findVariables("standard_name", yStandardName);
        if (dims.empty()) {
            throw CDMException("couldn't find projection axis with standard_name "+ yStandardName + " for projection " + projectionName);
        } else {
            yAxis = dims[0];
            if (dims.size() > 1) {
                retVal = false;
                LOG4FIMEX(logger, Logger::WARN, "found several dimensions with standard_name " << yStandardName << " using " << yAxis);
            }
        }
    }
    // units and values
    xAxisUnits = getUnits(xAxis);
    yAxisUnits = getUnits(yAxis);

    return retVal;
}

void CDM::generateProjectionCoordinates(const std::string& projectionVariable, const std::string& xDim, const std::string& yDim, const std::string& lonDim, const std::string& latDim)
{
    generateProjectionCoordinates(Projection::create(getAttributes(projectionVariable)), xDim, yDim, lonDim, latDim);
}

void CDM::generateProjectionCoordinates(Projection_cp projection, const std::string& xDim, const std::string& yDim, const std::string& lonDim,
                                        const std::string& latDim)
{
    const CDMVariable& xVar = getVariable(xDim);
    const CDMVariable& yVar = getVariable(yDim);
    assert(xVar.hasData());
    assert(yVar.hasData());
    auto xData = xVar.getData()->asDouble();
    auto yData = yVar.getData()->asDouble();
    size_t xDimLength = getDimension(xDim).getLength();
    size_t yDimLength = getDimension(yDim).getLength();
    assert(xDimLength == xVar.getData()->size());
    assert(yDimLength == yVar.getData()->size());
    size_t fieldSize = xDimLength * yDimLength;
    auto longVal = make_shared_array<double>(fieldSize);
    auto latVal = make_shared_array<double>(fieldSize);
    std::string lonLatProj(MIFI_WGS84_LATLON_PROJ4);
    assert(projection.get() != 0);
    std::string projStr = projection->getProj4String();
    LOG4FIMEX(logger, Logger::DEBUG, "generating "<<latDim<<"("<<xDim<<","<<yDim<<"),"<<lonDim<<"("<<xDim<<","<<yDim<<") using proj4: "+projStr);
    reproject::reproject_axes(projStr, lonLatProj, xData.get(), yData.get(), xDimLength, yDimLength, longVal.get(), latVal.get());
    std::vector<std::string> xyDims;
    xyDims.push_back(xDim);
    xyDims.push_back(yDim);
    CDMVariable lonVar(lonDim, CDM_DOUBLE, xyDims);
    lonVar.setData(createData(fieldSize, longVal));
    CDMVariable latVar(latDim, CDM_DOUBLE, xyDims);
    latVar.setData(createData(fieldSize, latVal));
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

Projection_cp CDM::getProjectionOf(std::string varName) const
{
    enhance(pimpl_.get(), *this);
    if (CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(pimpl_->coordSystems, varName))
        return cs->getProjection();
    return nullptr;
}

std::string CDM::getHorizontalXAxis(std::string varName) const
{
    enhance(pimpl_.get(), *this);

    if (CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(pimpl_->coordSystems, varName)) {
        if (CoordinateAxis_cp axis = cs->getGeoXAxis())
            return axis->getName();
    }
    return std::string();
}

std::string CDM::getHorizontalYAxis(std::string varName) const
{
    enhance(pimpl_.get(), *this);

    if (CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(pimpl_->coordSystems, varName)) {
        if (CoordinateAxis_cp axis = cs->getGeoYAxis())
            return axis->getName();
    }
    return std::string();
}

bool CDM::getLatitudeLongitude(std::string varName, std::string& latitude, std::string& longitude) const
{
    enhance(pimpl_.get(), *this);

    if (CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(pimpl_->coordSystems, varName)) {
        CoordinateAxis_cp latAxis = cs->findAxisOfType(CoordinateAxis::Lat);
        CoordinateAxis_cp lonAxis = cs->findAxisOfType(CoordinateAxis::Lon);
        if (latAxis && lonAxis) {
            latitude = latAxis->getName();
            longitude = lonAxis->getName();
            return true;
        }
    }
    return false;
}

std::string CDM::getTimeAxis(std::string varName) const
{
    enhance(pimpl_.get(), *this);

    const CoordinateSystem_cp_v& csList = pimpl_->coordSystems;

    // check if variable is its own axis (coord-axis don't have coordinate system)
    for (CoordinateSystem_cp cs : csList) {
        if (CoordinateAxis_cp timeAxis = cs->getTimeAxis())
            if (timeAxis->getName() == varName)
                return varName;
    }

    // search for coordinate system for varName
    if (CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(csList, varName)) {
        if (CoordinateAxis_cp axis = cs->getTimeAxis())
            return axis->getName();
    }
    return std::string();
}

std::string CDM::getVerticalAxis(std::string varName) const
{
    enhance(pimpl_.get(), *this);

    const CoordinateSystem_cp_v& csList = pimpl_->coordSystems;

    // check if variable is its own axis (coord-axis don't have coordinate system)
    for (CoordinateSystem_cp cs : csList) {
        if (CoordinateAxis_cp vAxis = cs->getGeoZAxis())
            if (vAxis->getName() == varName)
                return varName;
    }

    if (CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(csList, varName)) {
        if (CoordinateAxis_cp axis = cs->getGeoZAxis())
            return axis->getName();
    }
    return std::string();
}

}
