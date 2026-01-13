/*
 * Fimex, NcmlCDMReader.cc
 *
 * (C) Copyright 2009-2024, met.no
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
 *  Created on: Apr 29, 2009
 *      Author: Heiko Klein
 */

#include "fimex/NcmlCDMReader.h"

#include "NcmlAggregationReader.h"

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/DataUtils.h"
#include "fimex/Logger.h"
#include "fimex/MutexLock.h"
#include "fimex/SliceBuilder.h"
#include "fimex/StringUtils.h"
#include "fimex/XMLDoc.h"
#include "fimex/XMLInput.h"
#include "fimex/XMLUtils.h"

#include "NcmlAggregationReader.h"
#include "NcmlUtils.h"

#include <memory>
#include <regex>

namespace MetNoFimex {

using namespace std;

namespace {

Logger_p logger = getLogger("fimex.NcmlCDMReader");

template <class OutputIterator>
void split_marker(OutputIterator out, const std::string& str, const std::string& marker)
{
    std::size_t previous = 0, current;
    while ((current = str.find(marker, previous)) != std::string::npos) {
        *(out++) = str.substr(previous, current - previous);
        previous = current + marker.size();
    }
    *(out++) = str.substr(previous);
}

std::vector<std::string> split_marker(const std::string& str, const std::string& marker)
{
    std::vector<std::string> out;
    split_marker(std::back_inserter(out), str, marker);
    return out;
}

std::vector<std::string> split_ncml_values(const std::string& values, const std::string& separator)
{
    if (separator.empty())
        return tokenize(values, " ,\t\r\n");
    else
        return split_marker(values, separator);
}

std::vector<std::string> fimexShapeFromXmlAtt(const std::string& ncml_att_shape)
{
    // ncml shape is slowest varying dimension first, fimex shape is fastest varying dimension first
    const std::vector<std::string> ncml_shape = tokenize(ncml_att_shape, " \t");
    const std::vector<std::string> fimex_shape(ncml_shape.rbegin(), ncml_shape.rend());
    return fimex_shape;
}

std::vector<std::string> fimexShapeFromXmlNode(xmlNodePtr node)
{
    return fimexShapeFromXmlAtt(getXmlProp(node, "shape"));
}

} // namespace

NcmlCDMReader::NcmlCDMReader(const XMLInput& configXML)
    : configId(configXML.id())
{
    setConfigDoc(configXML);

    dataReader = std::make_shared<NcmlAggregationReader>(configXML);
    init();
}

NcmlCDMReader::NcmlCDMReader(CDMReader_p dataReader, const XMLInput& configXML)
    : configId(configXML.id())
    , dataReader(dataReader)
{
    setConfigDoc(configXML);
    init();
}

NcmlCDMReader::~NcmlCDMReader()
{
}

void NcmlCDMReader::setConfigDoc(const XMLInput& configXML)
{
    doc = configXML.getXMLDoc();
    if (!configXML.isEmpty()) {
        doc->registerNamespace("nc", "http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2");
        XPathNodeSet nodes(doc, "/nc:netcdf");
        if (nodes.size() != 1) {
            throw CDMException("config "+configId+" is not a ncml document with root /nc:netcdf");
        }
    }
}

void NcmlCDMReader::init()
{
    XPathNodeSet nodes(doc, "/nc:netcdf");
    if (nodes.size() == 0) {
        LOG4FIMEX(logger, Logger::WARN, "config " << configId << " not a ncml-file, ignoring");
        return;
    }
    if (dataReader) {
        *cdm_.get() = dataReader->getCDM();
    }
    LOG4FIMEX(logger, Logger::DEBUG, "initializing");

    initRemove();
    initDimensionNameChange();
    initDimensionUnlimited();
    initVariableShapeChange();
    initVariableNameChange();
    initVariableTypeChange();
    initVariableDataChange();
    initVariableSpatialVector();
    initAttributeNameChange();
    initAddReassignAttribute();
    initWarnUnsupported();
}

void NcmlCDMReader::warnUnsupported(std::string xpath, std::string msg)
{
    const XPathNodeSet nodes(doc, xpath);
    if (nodes.size() > 0) {
        LOG4FIMEX(logger, Logger::DEBUG, msg);
    }
}

void NcmlCDMReader::initRemove()
{
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:remove")) {
        std::string name = getXmlProp(node, "name");
        if (name.empty())
            throw CDMException("name attribute required for /nc:netcdf/nc:remove element in " + configId);
        std::string type = getXmlProp(node, "type");
        if (type.empty())
            throw CDMException("type attribute required for /nc:netcdf/nc:remove element in " + configId);
        if (type == "variable") {
            LOG4FIMEX(logger, Logger::DEBUG, "removing variable " << name);
            cdm_->removeVariable(name);
        } else if (type == "dimension") {
            LOG4FIMEX(logger, Logger::DEBUG, "removing dimension " << name);
            cdm_->removeDimension(name);
        } else if (type == "attribute") {
            LOG4FIMEX(logger, Logger::DEBUG, "removing attribute " << name);
            cdm_->removeAttribute(CDM::globalAttributeNS(), name);
        } else if (type == "group") {
            LOG4FIMEX(logger, Logger::DEBUG, "request to remove group " << name << " ignored");
            // groups not supported
        } else {
            LOG4FIMEX(logger, Logger::FATAL, "unknown type to remove in "+configId);
            throw CDMException("unknown type to remove in "+configId);
        }
    }

    /* remove the variable attributes */
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:variable/nc:remove")) {
        std::string name = getXmlProp(node, "name");
        if (name.empty())
            throw CDMException("name attribute required for /nc:netcdf/nc:remove element in " + configId);
        std::string type = getXmlProp(node, "type");
        if (type != "attribute")
            throw CDMException("type attribute required = 'attribute' for /nc:netcdf/nc:variable/nc:remove element in " + configId);
        std::string varName = getXmlProp(node->parent, "orgName");
        if (varName.empty()) {
            varName = getXmlProp(node->parent, "name");
        }
        if (varName.empty())
            throw CDMException("name attribute required for all variable element in " + configId);
        cdm_->removeAttribute(varName, name);
    }
}

/* warn about features not supported in this class */
void NcmlCDMReader::initWarnUnsupported()
{
    warnUnsupported("/nc:netcdf/nc:group","groups not supported");
}

void NcmlCDMReader::initVariableShapeChange()
{
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:variable[@shape]")) {
        std::string name = getXmlProp(node, "orgName");
        if (name.empty()) {
            name = getXmlProp(node, "name");
        }
        if (name.empty())
            throw CDMException("ncml-file " + configId + " has no name or orgName for variable");
        if (cdm_->hasVariable(name)) {
            CDMVariable& var = cdm_->getVariable(name);
            const std::vector<std::string> old_shape = var.getShape(); // no reference as it will be replaced
            const std::vector<std::string> fimex_shape = fimexShapeFromXmlNode(node);
            var.setShape(fimex_shape);
            LOG4FIMEX(logger, Logger::DEBUG,
                      "changing shape of variable: " << name << " from " << join(old_shape.begin(), old_shape.end(), ",") << " to "
                                                     << join(fimex_shape.begin(), fimex_shape.end(), ","));
        }
    }
}

void NcmlCDMReader::initVariableTypeChange()
{
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:variable[@type]")) {
        std::string type = getXmlProp(node, "type");
        std::string name = getXmlProp(node, "name");
        if (name.empty())
            throw CDMException("ncml-file " + configId + " has no name for variable");

        const XPathNodeSet nodes_att_unsigned(doc, "/nc:attribute[@name='" + NCML_UNSIGNED + "']", node);
        bool att_unsigned = false;
        if (nodes_att_unsigned.size() == 1) {
            std::string att_unsigned_val = getXmlProp(nodes_att_unsigned.at(0), "value");
            if (att_unsigned_val == "true")
                att_unsigned = true;
            else if (att_unsigned_val != "false")
                throw CDMException("ncml-file " + configId + " has invalid _Unsigned attribute value for variable '" + name + "'");
        }
        if (cdm_->hasVariable(name)) {
            CDMVariable& var = cdm_->getVariable(name);
            const CDMDataType newType = datatype_ncml2cdm(type, att_unsigned);
            if (newType == CDM_NAT)
                throw CDMException("cannot create variable name='" + name + "' with type '" + type + "'");
            if (newType != var.getDataType()) {
                variableTypeChanges[name] = newType;
                LOG4FIMEX(logger, Logger::DEBUG, "changing datatype of variable: " << name);
                var.setDataType(newType);
            }
        }
    }
}

#define LOG_ERROR_THROW(info)                                                                                                                                  \
    do {                                                                                                                                                       \
        std::ostringstream msg;                                                                                                                                \
        msg << info;                                                                                                                                           \
        LOG4FIMEX(logger, Logger::ERROR, msg.str());                                                                                                           \
        throw CDMException(msg.str());                                                                                                                         \
    } while (0)

void NcmlCDMReader::initVariableDataChange()
{
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:variable/nc:values")) {
        std::string name = getXmlProp(node->parent, "name");
        if (name.empty())
            throw CDMException("ncml-file " + configId + " has no name for variable");
        if (cdm_->hasVariable(name)) {
            CDMVariable& var = cdm_->getVariable(name);

            size_t dataSize = 1;
            const vector<string>& dims = var.getShape();
            for (const string& d : dims) {
                dataSize *= cdm_->getDimension(d).getLength();
            }

            const auto start = getXmlProp(node, "start");
            const auto increment = getXmlProp(node, "increment");
            const auto values_content = getXmlContent(node);
            const bool have_start_inc = (!start.empty() && !increment.empty());
            const bool have_values = (!values_content.empty());
            if (start.empty() != increment.empty()) {
                LOG_ERROR_THROW("values element must have either both 'start' and 'increment' or none of the two");
            } else if (have_start_inc == have_values) {
                LOG_ERROR_THROW("values element must have either 'start' and 'increment' or a value list as content");
            } else if (have_start_inc) {
                shared_array<double> array(new double[dataSize]);
                array[0] = string2type<double>(start);
                const double step = string2type<double>(increment);
                for (size_t i = 1; i < dataSize; ++i) {
                    array[i] = array[0] + i * step;
                }
                var.setData(convertValues(*createData(dataSize, array), var.getDataType()));
            } else {
                const auto values_list = split_ncml_values(values_content, getXmlProp(node, "separator"));

                // check that no. of values == shape-size
                if ((dims.size() == 0 && values_list.size() > 1) // scalars
                    || (dims.size() > 0 && values_list.size() != dataSize)) {
                    LOG_ERROR_THROW("ncml for variable " << name << " has " << values_list.size() << " values while shape requires " << dataSize << " values");
                }

                var.setData(initDataByArray(var.getDataType(), values_list));
            }
        }
    }
}

void NcmlCDMReader::initVariableSpatialVector()
{
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:variable/nc:spatial_vector")) {
        std::string name = getXmlProp(node->parent, "name");
        if (name.empty())
            throw CDMException("ncml-file " + configId + " has no name for variable");
        if (cdm_->hasVariable(name)) {
            std::string direction = getXmlProp(node, "direction");
            std::string counterPart = getXmlProp(node, "counterpart");
            CDMVariable& var = cdm_->getVariable(name);
            var.setAsSpatialVector(counterPart, CDMVariable::vectorDirectionFromString(direction));
        }
    }
}

namespace {
std::string getAttributeValue(const xmlNodePtr node)
{
    std::string value = getXmlProp(node, "value");
    if (value.empty()) {
        // attribute value may also be specified as element content
        value = getXmlContent(node);
    }
    return value;
}

CDMAttribute createAttributeFromXML(const xmlNodePtr node, const std::string& value)
{
    std::string name = getXmlProp(node, "name");

    std::string type = getXmlProp(node, "type");
    if (type.empty()) {
        // default attribute type is "string"
        type = "string";
    }
    const CDMDataType datatype = datatype_ncml2cdm(type, false);
    if (datatype == CDM_NAT)
        throw CDMException("cannot create att name='" + name + "' with type '" + type + "'");

    vector<string> values;
    if (type == "string" || type == "String") {
        // string attributes always have a single value
        values = {value};
    } else {
        values = split_ncml_values(value, getXmlProp(node, "separator"));
    }

    return CDMAttribute(name, datatype, values);
}
} // namespace

void NcmlCDMReader::initAddReassignAttribute()
{
    // see https://docs.unidata.ucar.edu/netcdf-java/current/userguide/annotated_ncml_schema.html#attribute-element
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:attribute")) {
        const std::string value = getAttributeValue(node);
        if (value.empty())
            continue;

        const CDMAttribute att = createAttributeFromXML(node, value);

        LOG4FIMEX(logger, Logger::DEBUG, "adding global attribute: " + att.getName());
        cdm_->removeAttribute(CDM::globalAttributeNS(), att.getName());
        cdm_->addAttribute(CDM::globalAttributeNS(), att);
    }

    /* the variable attributes */
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:variable/nc:attribute")) {
        const std::string varName = getXmlProp(node->parent, "name");
        const std::string value = getAttributeValue(node);
        if (value.empty())
            continue;
        const CDMAttribute att = createAttributeFromXML(node, value);
        cdm_->removeAttribute(varName, att.getName());
        LOG4FIMEX(logger, Logger::DEBUG, "adding variable attribute to " << varName << ": " << att.getName());
        cdm_->addAttribute(varName, att);
    }
}

/* change the variable names */
void NcmlCDMReader::initVariableNameChange()
{
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:variable[@orgName]")) {
        std::string orgName = getXmlProp(node, "orgName");
        if (cdm_->hasVariable(orgName)) {
            std::string name = getXmlProp(node, "name");
            if (name.empty())
                throw CDMException("ncml-file " + configId + " has no name for variable with orgName: " + orgName);
            cdm_->renameVariable(orgName, name);
            variableNameChanges[name] = orgName;
        }
    }

    // add variables not existing yet
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:variable[not(@orgName)]")) {
        std::string name = getXmlProp(node, "name");
        std::string type = getXmlProp(node, "type");
        if (!type.empty() && !cdm_->hasVariable(name)) {
            CDMDataType dataType = string2datatype(type);
            CDMVariable var(name, dataType, fimexShapeFromXmlNode(node));
            var.setData(createData(dataType, 0)); // this data cannot get fetched from the reader!
            cdm_->addVariable(var);
        }
    }
}

/* change the dimension names */
void NcmlCDMReader::initDimensionNameChange()
{
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:dimension[@orgName]")) {
        std::string orgName = getXmlProp(node, "orgName");
        if (cdm_->hasDimension(orgName)) {
            std::string newName = getXmlProp(node, "name");
            if (newName.empty())
                throw CDMException("ncml-file "+ configId + " has no new name for dimension with orgName: '" + orgName + "'");
            bool overwrite = (getXmlProp(node, "overwriteName") == "true");
            cdm_->renameDimension(orgName, newName, overwrite);
            dimensionNameChanges[orgName] = newName;
        }
    }
    // add dimensions not existing
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:dimension[@length]")) {
        std::string name = getXmlProp(node, "name");
        int length = string2type<int>(getXmlProp(node, "length"));
        if (!cdm_->hasDimension(name)) {
            CDMDimension dim(name,length);
            cdm_->addDimension(dim);
        } else {
            // very dangerous to change existing length, might fail...
            // but might be used in some unlimited cases
            cdm_->getDimension(name).setLength(length);
        }
    }
}

/*
 * set isUnlimited for dimensions. No check is made if this
 * covers the netcdf3 requirements (i.e. just one unlimited dimension and it must be first)
 */
void NcmlCDMReader::initDimensionUnlimited()
{
    for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:dimension[@isUnlimited]")) {
        std::string name = getXmlProp(node, "name");
        std::string orgName = getXmlProp(node, "orgName");
        if (orgName.empty())
            orgName = name;
        std::string isUnlimited = getXmlProp(node, "isUnlimited");
        if (cdm_->hasDimension(name)) {
            CDMDimension& dim = cdm_->getDimension(name);
            if (isUnlimited == "true") {
                if (!dim.isUnlimited()) {
                    cdm_->getDimension(name).setUnlimited(1);
                    // prefetch the data of that variable
                    if (cdm_->hasVariable(name)) {
                        CDMVariable& var = cdm_->getVariable(name);
                        if (!var.hasData()) {
                            var.setData(dataReader->getData(orgName));
                        }
                    }
                }
            } else if (isUnlimited == "false") {
                cdm_->getDimension(name).setUnlimited(0);
            } else {
                string warning = "NcML: unlimited of dimension of " + name + " must be 'true' or 'false' but is: " + isUnlimited;
                LOG4FIMEX(logger, Logger::ERROR, warning);
                throw CDMException(warning);
            }
        }
    }
}


/* change the attribute names */
void NcmlCDMReader::initAttributeNameChange()
{
    { /* the global attributes */
        for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:attribute[@orgName]")) {
            std::string orgName = getXmlProp(node, "orgName");
            std::string name = getXmlProp(node, "name");
            if (name.empty())
                throw CDMException("ncml-file " + configId + " has no name for attribute with orgName: " + orgName);
            try {
                CDMAttribute& attr = cdm_->getAttribute(CDM::globalAttributeNS(), orgName);
                cdm_->removeAttribute(CDM::globalAttributeNS(), name); // remove other attributes with same name
                attr.setName(name);
            } catch (CDMException& ex) {
                // no such attribute, don't care
            }
        }
    }
    { /* the variable attributes */
        for (const auto node : XPathNodeSet(doc, "/nc:netcdf/nc:variable[@name]/nc:attribute[@orgName]")) {
            std::string varName = getXmlProp(node->parent, "name");
            std::string orgName = getXmlProp(node, "orgName");
            std::string name = getXmlProp(node, "name");
            if (name.empty())
                throw CDMException("ncml-file " + configId + " has no name for attribute with orgName: " + orgName);
            try {
                CDMAttribute& attr = cdm_->getAttribute(varName, orgName);
                cdm_->removeAttribute(varName, name); // remove other attributes with same name
                attr.setName(name);

            } catch (CDMException& ex) {
                // no such attribute, don't care
            }
        }
    }
}


DataPtr NcmlCDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    SliceBuilder sb(*cdm_, varName);
    if (const CDMDimension* unlimDim = cdm_->getUnlimitedDim()) {
        if (cdm_->getVariable(varName).checkDimension(unlimDim->getName()))
            sb.setStartAndSize(unlimDim->getName(), unLimDimPos, 1);
    }
    return getDataSlice(varName, sb);
}

DataPtr NcmlCDMReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getDataSlice(var,sb): (" << varName << ", " << sb << ")");

    // return unchanged data from this CDM
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        LOG4FIMEX(logger, Logger::DEBUG, "fetching data from memory");
        return getDataSliceFromMemory(variable, sb);
    }

    // find the original name, to fetch the data from the dataReader
    map<string, string>::const_iterator vit = variableNameChanges.find(varName);
    const std::string& orgVarName = (vit != variableNameChanges.end()) ? vit->second : varName;

    SliceBuilder orgSb(dataReader->getCDM(), orgVarName);
    {
        const vector<string> shape = sb.getDimensionNames();
        const vector<string> orgShape = orgSb.getDimensionNames();
        for (const std::string& orgDim : orgShape) {
            std::map<std::string, std::string>::const_iterator itChanged = dimensionNameChanges.find(orgDim);
            const std::string& newDim = (itChanged != dimensionNameChanges.end()) ? itChanged->second : orgDim;
            if (find(shape.begin(), shape.end(), newDim) != shape.end()) {
                size_t strt, sz;
                sb.getStartAndSize(newDim, strt, sz);
                if (strt + sz > dataReader->getCDM().getDimension(orgDim).getLength())
                    return createData(variable.getDataType(), 0);
                orgSb.setStartAndSize(orgDim, strt, sz);
            }
        }
        // set all other dims to 1
        vector<string> unsetDims = orgSb.getUnsetDimensionNames();
        for (size_t i = 0; i < unsetDims.size(); ++i) {
            // make minimal
            orgSb.setStartAndSize(unsetDims.at(i), 0, 1);
        }
    }

    // get data from original source with slicebuilder
    DataPtr data = dataReader->getDataSlice(orgVarName, orgSb);
    return applyVariableTypeChange(data, varName, orgVarName);
}

DataPtr NcmlCDMReader::applyVariableTypeChange(DataPtr data, const std::string& varName, const std::string& orgVarName)
{
    // eventually, change the type from the old type to the new type
    map<string, CDMDataType>::iterator dtIt = variableTypeChanges.find(varName);
    if (dtIt == variableTypeChanges.end())
        return data;

    const CDM& orgCDM = dataReader->getCDM();
    double orgFill = orgCDM.getFillValue(orgVarName);
    double orgScale = orgCDM.getScaleFactor(orgVarName);
    double orgOffset = orgCDM.getAddOffset(orgVarName);

    double newFill = cdm_->getFillValue(varName);
    double newScale = cdm_->getScaleFactor(varName);
    double newOffset = cdm_->getAddOffset(varName);

    return data->convertDataType(orgFill, orgScale, orgOffset, dtIt->second, newFill, newScale, newOffset);
}

} // namespace MetNoFimex
