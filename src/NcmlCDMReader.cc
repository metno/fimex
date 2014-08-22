/*
 * Fimex, NcmlCDMReader.cc
 *
 * (C) Copyright 2009, met.no
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
#include "fimex/XMLDoc.h"
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include "../config.h"
#ifdef HAVE_NETCDF_H
#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/NetCDF_CDMReader.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED
#endif
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/Data.h"
#include "fimex/CDM.h"
#include <boost/regex.hpp>

namespace MetNoFimex
{

using namespace std;

static LoggerPtr logger = getLogger("fimex/NcmlCDMReader");

NcmlCDMReader::NcmlCDMReader(const XMLInput& configXML)
    : configId(configXML.id())
{
#ifdef HAVE_NETCDF_H
    setConfigDoc(configXML);

    dataReader = boost::shared_ptr<CDMReader>(new NcmlAggregationReader(configXML));
#if 0
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf[@location]");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    if (nodes->nodeNr != 1) {
        LOG4FIMEX(logger, Logger::INFO, "config " << configId << " does not contain location-attribute, only ncml initialization");
    } else {
        string ncFile = getXmlProp(nodes->nodeTab[0], "location");
        // remove file: URL-prefix
        ncFile = boost::regex_replace(ncFile, boost::regex("^file:"), "", boost::format_first_only);
        // java-netcdf allows dods: prefix for dods-files while netcdf-C requires http:
        ncFile = boost::regex_replace(ncFile, boost::regex("^dods:"), "http:", boost::format_first_only);
        dataReader = boost::shared_ptr<CDMReader>(new NetCDF_CDMReader(ncFile));
    }
#endif
    init();
#else
    string msg("cannot read data through ncml - no netcdf-support compiled in fimex");
    LOG4FIMEX(logger, Logger::FATAL, msg);
    throw CDMException(msg);
#endif
}

NcmlCDMReader::NcmlCDMReader(const boost::shared_ptr<CDMReader> dataReader, const XMLInput& configXML)
    : configId(configXML.id()), dataReader(dataReader)
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
        XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        if (nodes->nodeNr != 1) {
            throw CDMException("config "+configId+" is not a ncml document with root /nc:netcdf");
        }
    }
}

void NcmlCDMReader::init()
{
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf");
    int size = xpathObj->nodesetval ? xpathObj->nodesetval->nodeNr : 0;
    if (size == 0) {
        LOG4FIMEX(logger, Logger::WARN, "config " << configId << " not a ncml-file, ignoring");
        return;
    }
    if (dataReader.get() != 0) {
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

void NcmlCDMReader::warnUnsupported(std::string xpath, std::string msg) {
    XPathObjPtr xpathObj = doc->getXPathObject(xpath);
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size > 0) {
        LOG4FIMEX(logger, Logger::DEBUG, msg);
    }
}

void NcmlCDMReader::initRemove() {
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:remove");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string name = getXmlProp(nodes->nodeTab[i], "name");
        if (name == "") throw CDMException("name attribute required for /nc:netcdf/nc:remove element in "+configId);
        std::string type = getXmlProp(nodes->nodeTab[i], "type");
        if (type == "") throw CDMException("type attribute required for /nc:netcdf/nc:remove element in "+configId);
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
    xpathObj = doc->getXPathObject("/nc:netcdf/nc:variable/nc:remove");
    nodes = xpathObj->nodesetval;
    size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string name = getXmlProp(nodes->nodeTab[i], "name");
        if (name == "") throw CDMException("name attribute required for /nc:netcdf/nc:remove element in "+configId);
        std::string type = getXmlProp(nodes->nodeTab[i], "type");
        if (type != "attribute") throw CDMException("type attribute required = 'attribute' for /nc:netcdf/nc:variable/nc:remove element in "+configId);
        std::string varName = getXmlProp(nodes->nodeTab[i]->parent, "orgName");
        if (varName == "") {
            varName = getXmlProp(nodes->nodeTab[i]->parent, "name");
        }
        if (varName == "") throw CDMException("name attribute required for all variable element in "+configId);
        cdm_->removeAttribute(varName, name);
    }
}

/* warn about features not supported in this class */
void NcmlCDMReader::initWarnUnsupported() {
    //warnUnsupported("/nc:netcdf/nc:aggregation", "aggregation not supported");
    warnUnsupported("/nc:netcdf/nc:group","groups not supported");
}

void NcmlCDMReader::initVariableShapeChange()
{
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:variable[@shape]");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string shape = getXmlProp(nodes->nodeTab[i], "shape");
        std::string name = getXmlProp(nodes->nodeTab[i], "orgName");
        if (name == "") {
            name = getXmlProp(nodes->nodeTab[i], "name");
        }
        if (name == "") throw CDMException("ncml-file "+ configId + " has no name or orgName for variable");
        if (cdm_->hasVariable(name)) {
            vector<string> vshape = tokenize(shape, " \t");
            CDMVariable& var = cdm_->getVariable(name);
            vector<string> oldShape = var.getShape();
            var.setShape(vshape);
            LOG4FIMEX(logger, Logger::DEBUG, "changing shape of variable: " << name << " from " << join(oldShape.begin(), oldShape.end(), ",") << " to " << shape);
        }
    }
}

void NcmlCDMReader::initVariableTypeChange()
{
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:variable[@type]");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string type = getXmlProp(nodes->nodeTab[i], "type");
        std::string name = getXmlProp(nodes->nodeTab[i], "name");
        if (name == "") throw CDMException("ncml-file "+ configId + " has no name for variable");
        if (cdm_->hasVariable(name)) {
            CDMVariable& var = cdm_->getVariable(name);
            CDMDataType oldType = var.getDataType();
            CDMDataType newType = string2datatype(type);
            if (oldType != newType) {
                variableTypeChanges[name] = newType;
                LOG4FIMEX(logger, Logger::DEBUG, "changing datatype of variable: " << name);
                var.setDataType(string2datatype(type));
            }
        }
    }
}

void NcmlCDMReader::initVariableDataChange()
{
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:variable/nc:values");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string name = getXmlProp(nodes->nodeTab[i]->parent, "name");
        if (name == "") throw CDMException("ncml-file "+ configId + " has no name for variable");
        if (cdm_->hasVariable(name)) {
            std::string values = getXmlContent(nodes->nodeTab[i]);
            std::vector<std::string> tokens = tokenize(values, " \t\r\n");
            std::vector<double> dvals;
            dvals.reserve(tokens.size());
            std::transform(tokens.begin(), tokens.end(),
                    std::back_inserter(dvals), string2type<double>);
            CDMVariable& var = cdm_->getVariable(name);
            // check that no. of values == shape-size
            vector<string> dims = var.getShape();
            size_t dataSize = 0;
            for (size_t i = 0; i < dims.size(); ++i) {
                size_t dimLen = cdm_->getDimension(dims[i]).getLength();
                if (dataSize == 0) {
                    dataSize = 1;
                }
                dataSize *= dimLen;
            }
            if ((dims.size() == 0 && dvals.size() > 1) // scalars
                || (dims.size() > 0 && dvals.size() != dataSize)) {
                LOG4FIMEX(logger, Logger::ERROR, "values from ncml: "<< dvals.size() <<" required for " << name << ": " << dataSize);
                throw CDMException("values from ncml does not match shape for variable "+name);
            }
            var.setData(createData(var.getDataType(), dvals.begin(), dvals.end()));
        }
    }
}

void NcmlCDMReader::initVariableSpatialVector()
{
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:variable/nc:spatial_vector");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string name = getXmlProp(nodes->nodeTab[i]->parent, "name");
        if (name == "") throw CDMException("ncml-file "+ configId + " has no name for variable");
        if (cdm_->hasVariable(name)) {
            std::string direction = getXmlProp(nodes->nodeTab[i], "direction");
            std::string counterPart = getXmlProp(nodes->nodeTab[i], "counterpart");
            CDMVariable& var = cdm_->getVariable(name);
            var.setAsSpatialVector(counterPart, direction);
        }
    }
}

void NcmlCDMReader::initAddReassignAttribute()
{
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:attribute[@value]");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string name = getXmlProp(nodes->nodeTab[i], "name");
        std::string value = getXmlProp(nodes->nodeTab[i], "value");
        std::string separator = getXmlProp(nodes->nodeTab[i], "separator");
        if (separator == "") separator = " ";
        vector<string> values = tokenize(value, separator);
        std::string type = getXmlProp(nodes->nodeTab[i], "type");
        if (type == "") type = "string";
        LOG4FIMEX(logger, Logger::DEBUG, "adding global attribute: " + name);
        cdm_->removeAttribute(CDM::globalAttributeNS(), name);
        cdm_->addAttribute(CDM::globalAttributeNS(), CDMAttribute(name, string2datatype(type), values));
    }

    /* the variable attributes */
    xpathObj = doc->getXPathObject("/nc:netcdf/nc:variable/nc:attribute[@value]");
    nodes = xpathObj->nodesetval;
    size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string varName = getXmlProp(nodes->nodeTab[i]->parent, "name");
        std::string name = getXmlProp(nodes->nodeTab[i], "name");
        std::string value = getXmlProp(nodes->nodeTab[i], "value");
        std::string separator = getXmlProp(nodes->nodeTab[i], "separator");
        if (separator == "") separator = " ";
        vector<string> values = tokenize(value, separator);
        std::string type = getXmlProp(nodes->nodeTab[i], "type");
        if (type == "") type = "string";
        cdm_->removeAttribute(varName, name);
        LOG4FIMEX(logger, Logger::DEBUG, "adding variable attribute to "<< varName << ": " << name);
        cdm_->addAttribute(varName, CDMAttribute(name, string2datatype(type), values));
    }
}

/* change the variable names */
void NcmlCDMReader::initVariableNameChange()
{
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:variable[@orgName]");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string orgName = getXmlProp(nodes->nodeTab[i], "orgName");
        if (cdm_->hasVariable(orgName)) {
            std::string name = getXmlProp(nodes->nodeTab[i], "name");
            if (name == "") throw CDMException("ncml-file "+ configId + " has no name for variable with orgName: "+ orgName);
            cdm_->renameVariable(orgName, name);
            variableNameChanges[name] = orgName;
        }
    }

    // add variables not existing yet
    xpathObj = doc->getXPathObject("/nc:netcdf/nc:variable[not(@orgName)]");
    nodes = xpathObj->nodesetval;
    size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string name = getXmlProp(nodes->nodeTab[i], "name");
        if (!cdm_->hasVariable(name)) {
            std::string shape = getXmlProp(nodes->nodeTab[i], "shape");
            vector<string> vshape = tokenize(shape, " \t");
            std::string type = getXmlProp(nodes->nodeTab[i], "type");
            CDMDataType dataType = string2datatype(type);
            CDMVariable var(name, dataType, vshape);
            var.setData(createData(dataType, 0)); // this data cannot get fetched from the reader!
            cdm_->addVariable(var);
        }
    }

}

/* change the dimension names */
void NcmlCDMReader::initDimensionNameChange()
{
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:dimension[@orgName]");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string orgName = getXmlProp(nodes->nodeTab[i], "orgName");
        if (cdm_->hasDimension(orgName)) {
            std::string name = getXmlProp(nodes->nodeTab[i], "name");
            if (name == "") throw CDMException("ncml-file "+ configId + " has no name for dimension with orgName: "+ orgName);
            cdm_->renameDimension(orgName, name);
        }
    }
    // add dimensions not existing
    xpathObj = doc->getXPathObject("/nc:netcdf/nc:dimension[@length]");
    nodes = xpathObj->nodesetval;
    size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string name = getXmlProp(nodes->nodeTab[i], "name");
        int length = string2type<int>(getXmlProp(nodes->nodeTab[i], "length"));
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
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:dimension[@isUnlimited]");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string name = getXmlProp(nodes->nodeTab[i], "name");
        std::string orgName = getXmlProp(nodes->nodeTab[i], "orgName");
        if (orgName == "") orgName = name;
        std::string isUnlimited = getXmlProp(nodes->nodeTab[i], "isUnlimited");
        if (cdm_->hasDimension(name)) {
            CDMDimension& dim = cdm_->getDimension(name);
            if (isUnlimited == "true") {
                if (!dim.isUnlimited()) {
                    cdm_->getDimension(name).setUnlimited(1);
                    unlimitedDimensionChanges[name] = orgName;
                    // prefetch the data of that variable
                    if (cdm_->hasVariable(name)) {
                        CDMVariable& var = cdm_->getVariable(name);
                        if (!var.hasData()) {
                            var.setData(dataReader->getData(orgName));
                        }
                    }
                }
            } else if (isUnlimited == "false") {
                unlimitedDimensionChanges[name] = orgName;
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
        XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:attribute[@orgName]");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        for (int i = 0; i < size; i++) {
            std::string orgName = getXmlProp(nodes->nodeTab[i], "orgName");
            std::string name = getXmlProp(nodes->nodeTab[i], "name");
            if (name == "") throw CDMException("ncml-file "+ configId + " has no name for attribute with orgName: "+ orgName);
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
        XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:variable[@name]/nc:attribute[@orgName]");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        for (int i = 0; i < size; i++) {
            std::string varName = getXmlProp(nodes->nodeTab[i]->parent, "name");
            std::string orgName = getXmlProp(nodes->nodeTab[i], "orgName");
            std::string name = getXmlProp(nodes->nodeTab[i], "name");
            if (name == "") throw CDMException("ncml-file "+ configId + " has no name for attribute with orgName: "+ orgName);
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
    ScopedCritical sc(mutex_);
    LOG4FIMEX(logger, Logger::DEBUG, "getDataSlice(var,unlimDimPos): (" << varName << ", " << unLimDimPos << ")");
    // return unchanged data from this CDM
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        LOG4FIMEX(logger, Logger::DEBUG, "fetching data from memory");
        return getDataSliceFromMemory(variable, unLimDimPos);
    }
    if (dataReader.get() == 0) {
        return createData(CDM_NAT, 0);
    }

    // find the original name, to fetch the data from the dataReader
    std::string orgVarName = varName;
    map<string, string>::iterator vit = variableNameChanges.find(varName);
    if (vit != variableNameChanges.end()) {
        orgVarName = vit->second;
    }

    DataPtr data;
    const CDM& orgCDM = dataReader->getCDM();
    const CDMDimension* orgUnlimDim = orgCDM.getUnlimitedDim();
    string orgUnlimDimNm = (orgUnlimDim != 0) ? orgUnlimDim->getName() : "";
    const CDMDimension* unlimDim = cdm_->getUnlimitedDim();
    string unlimDimNm = (unlimDim != 0) ? unlimDim->getName() : "";
    if ( (unlimitedDimensionChanges.size() > 0) &&
         find(cdm_->getVariable(varName).getShape().begin(),
              cdm_->getVariable(varName).getShape().end(),
              unlimitedDimensionChanges.begin()->first) != cdm_->getVariable(varName).getShape().end()
       ) {
        // not working for several unlimited dimensions yet
        LOG4FIMEX(logger, Logger::DEBUG, "getting data for var " << orgVarName << " with fake unlimDim " << unlimitedDimensionChanges[unlimDimNm]);
        SliceBuilder sb(dataReader->getCDM(), orgVarName);
        if (dataReader->getCDM().hasDimension(unlimitedDimensionChanges[unlimDimNm])) {
            sb.setStartAndSize(unlimitedDimensionChanges[unlimDimNm], unLimDimPos, 1);
        }
        //
        const vector<size_t>& sizes = sb.getDimensionSizes();
        if (find(sizes.begin(), sizes.end(), 0) == sizes.end()) {
            data = dataReader->getDataSlice(orgVarName, sb);
        } else {
            // one dimension has size 0 -> 0 length data
            data = createData(dataReader->getCDM().getVariable(orgVarName).getDataType(), 0, 0.0d);
        }
    } else {
        // check if extended unlimited dimension slice
        if ((unlimDim != 0) &&
                (orgCDM.getVariable(orgVarName).checkDimension(unlimDim->getName())) &&
                (unlimDim->getLength() <= unLimDimPos)) {
            LOG4FIMEX(logger, Logger::DEBUG, "getting data for var " << varName << " and slice " << unLimDimPos << " outside original data, using undef");
            data = createData(cdm_->getVariable(orgVarName).getDataType(), 0);
        } else {
            // get data as usual
            data = dataReader->getDataSlice(orgVarName, unLimDimPos);
        }
    }
    // eventually, change the type from the old type to the new type
    map<string, CDMDataType>::iterator dtIt = variableTypeChanges.find(varName);
    if (dtIt != variableTypeChanges.end()) {
        double orgFill = orgCDM.getFillValue(orgVarName);
        CDMAttribute attr;
        double orgScale = 1.;
        double orgOffset = 0.;
        if (orgCDM.getAttribute(varName, "scale_factor", attr)) {
            orgScale = attr.getData()->asDouble()[0];
        }
        if (orgCDM.getAttribute(varName, "add_offset", attr)) {
            orgOffset = attr.getData()->asDouble()[0];
        }
        double newFill = cdm_->getFillValue(varName);
        double newScale = 1.;
        double newOffset = 0.;
        if (cdm_->getAttribute(varName, "scale_factor", attr)) {
            newScale = attr.getData()->asDouble()[0];
        }
        if (cdm_->getAttribute(varName, "add_offset", attr)) {
            newOffset = attr.getData()->asDouble()[0];
        }

        data = data->convertDataType(orgFill, orgScale, orgOffset, dtIt->second, newFill, newScale, newOffset);
    }
    return data;
}

DataPtr NcmlCDMReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    ScopedCritical sc(mutex_);
    LOG4FIMEX(logger, Logger::DEBUG, "getDataSlice(var,sb): (" << varName << ", sb)");
    // return unchanged data from this CDM
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        LOG4FIMEX(logger, Logger::DEBUG, "fetching data from memory");
        DataPtr data = variable.getData();
        if (data->size() == 0) {
            return data;
        } else {
            return variable.getData()->slice(sb.getMaxDimensionSizes(),
                                             sb.getDimensionStartPositions(),
                                             sb.getDimensionSizes());
        }
    }

    // find the original name, to fetch the data from the dataReader
    std::string orgVarName = varName;
    map<string, string>::iterator vit = variableNameChanges.find(varName);
    if (vit != variableNameChanges.end()) {
        orgVarName = vit->second;
    }

    // get data from original source with slicebuilder
    DataPtr data = dataReader->getDataSlice(orgVarName, sb);

    // eventually, change the type from the old type to the new type
    map<string, CDMDataType>::iterator dtIt = variableTypeChanges.find(varName);
    if (dtIt != variableTypeChanges.end()) {
        const CDM& orgCDM = dataReader->getCDM();
        double orgFill = orgCDM.getFillValue(orgVarName);
        CDMAttribute attr;
        double orgScale = 1.;
        double orgOffset = 0.;
        if (orgCDM.getAttribute(varName, "scale_factor", attr)) {
            orgScale = attr.getData()->asDouble()[0];
        }
        if (orgCDM.getAttribute(varName, "add_offset", attr)) {
            orgOffset = attr.getData()->asDouble()[0];
        }
        double newFill = cdm_->getFillValue(varName);
        double newScale = 1.;
        double newOffset = 0.;
        if (cdm_->getAttribute(varName, "scale_factor", attr)) {
            newScale = attr.getData()->asDouble()[0];
        }
        if (cdm_->getAttribute(varName, "add_offset", attr)) {
            newOffset = attr.getData()->asDouble()[0];
        }

        data = data->convertDataType(orgFill, orgScale, orgOffset, dtIt->second, newFill, newScale, newOffset);
    }

    return data;
}

}
