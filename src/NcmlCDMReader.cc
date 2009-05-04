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
#include "fimex/XMLDoc.h"
#include "fimex/NetCDF_CF10_CDMReader.h"
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/Data.h"

namespace MetNoFimex
{

using namespace std;

static LoggerPtr logger = getLogger("fimex/NcmlCDMReader");

NcmlCDMReader::NcmlCDMReader(std::string configFile) throw(CDMException)
    : configFile(configFile)
{
    doc = new XMLDoc(configFile);
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf[@location]");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    if (nodes->nodeNr != 1) {
        throw CDMException("config-file "+configFile+" does not contain location-attribute");
    }
    string ncFile = getXmlProp(nodes->nodeTab[0], "location");
    dataReader = boost::shared_ptr<CDMReader>(new NetCDF_CF10_CDMReader(ncFile));
    init();
}

NcmlCDMReader::NcmlCDMReader(const boost::shared_ptr<CDMReader> dataReader, std::string configFile) throw(CDMException)
    : configFile(configFile), dataReader(dataReader)
{
    doc = new XMLDoc(configFile);
    init();
}

NcmlCDMReader::~NcmlCDMReader()
{
    // doc cannot get copied, just created or deleted
    delete doc;
}

void NcmlCDMReader::init() throw(CDMException)
{
    doc->registerNamespace("nc", "http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2");
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf");
    int size = xpathObj->nodesetval ? xpathObj->nodesetval->nodeNr : 0;
    if (size == 0) {
        LOG4FIMEX(logger, Logger::WARN, "config-file " << configFile << " not a ncml-file, ignoring");
        return;
    }
    cdm = dataReader->getCDM();
    LOG4FIMEX(logger, Logger::DEBUG, "initializing");

    initRemove();
    initVariableNameChange();
    initVariableTypeChange();
    initDimensionNameChange();
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
        if (name == "") throw CDMException("name attribute required for /nc:netcdf/nc:remove element in "+configFile);
        std::string type = getXmlProp(nodes->nodeTab[i], "type");
        if (type == "") throw CDMException("type attribute required for /nc:netcdf/nc:remove element in "+configFile);
        if (type == "variable") {
            LOG4FIMEX(logger, Logger::DEBUG, "removing variable " << name);
            cdm.removeVariable(name);
        } else if (type == "dimension") {
            LOG4FIMEX(logger, Logger::DEBUG, "removing dimension " << name);
            cdm.removeDimension(name);
        } else if (type == "attribute") {
            LOG4FIMEX(logger, Logger::DEBUG, "removing attribute " << name);
            cdm.removeAttribute(CDM::globalAttributeNS(), name);
        } else if (type == "group") {
            LOG4FIMEX(logger, Logger::DEBUG, "request to remove group " << name << " ignored");
            // groups not supported
        } else {
            LOG4FIMEX(logger, Logger::FATAL, "unknown type to remove in "+configFile);
            throw CDMException("unknown type to remove in "+configFile);
        }
    }


    /* remove the variable attributes */
    xpathObj = doc->getXPathObject("/nc:netcdf/nc:variable/nc:remove");
    nodes = xpathObj->nodesetval;
    size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string name = getXmlProp(nodes->nodeTab[i], "name");
        if (name == "") throw CDMException("name attribute required for /nc:netcdf/nc:remove element in "+configFile);
        std::string type = getXmlProp(nodes->nodeTab[i], "type");
        if (type != "attribute") throw CDMException("type attribute required = 'attribute' for /nc:netcdf/nc:variable/nc:remove element in "+configFile);
        std::string varName = getXmlProp(nodes->nodeTab[i]->parent, "name");
        if (varName == "") throw CDMException("name attribute required for all variable element in "+configFile);
        cdm.removeAttribute(varName, name);
    }
}

/* warn about features not supported in this class */
void NcmlCDMReader::initWarnUnsupported() {
    warnUnsupported("/nc:netcdf/nc:aggregation", "aggregation not supported");
    warnUnsupported("/nc:netcdf/nc:dimension[isUnlimited]", "setting of unlimited dimension not supported");
    warnUnsupported("/nc:netcdf/nc:group","groups not supported");
}

void NcmlCDMReader::initVariableTypeChange()
{
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:variable[@type]");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        std::string type = getXmlProp(nodes->nodeTab[i], "type");
        std::string name = getXmlProp(nodes->nodeTab[i], "name");
        if (name == "") throw CDMException("ncml-file "+ configFile + " has no name for variable");
        if (cdm.hasVariable(name)) {
            variableTypeChanges[name] = string2datatype(type);
            LOG4FIMEX(logger, Logger::DEBUG, "changing datatype of variable: " << name);
            cdm.getVariable(name).setDataType(string2datatype(type));
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
        cdm.removeAttribute(CDM::globalAttributeNS(), name);
        cdm.addAttribute(CDM::globalAttributeNS(), CDMAttribute(name, string2datatype(type), values));
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
        cdm.removeAttribute(varName, name);
        LOG4FIMEX(logger, Logger::DEBUG, "adding variable attribute to "<< varName << ": " << name);
        cdm.addAttribute(varName, CDMAttribute(name, string2datatype(type), values));
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
        if (cdm.hasVariable(orgName)) {
            std::string name = getXmlProp(nodes->nodeTab[i], "name");
            if (name == "") throw CDMException("ncml-file "+ configFile + " has no name for variable with orgName: "+ orgName);
            cdm.renameVariable(orgName, name);
            variableNameChanges[name] = orgName;
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
        if (cdm.hasVariable(orgName)) {
            std::string name = getXmlProp(nodes->nodeTab[i], "name");
            if (name == "") throw CDMException("ncml-file "+ configFile + " has no name for dimension with orgName: "+ orgName);
            cdm.renameDimension(orgName, name);
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
            if (name == "") throw CDMException("ncml-file "+ configFile + " has no name for attribute with orgName: "+ orgName);
            try {
                CDMAttribute& attr = cdm.getAttribute(CDM::globalAttributeNS(), orgName);
                cdm.removeAttribute(CDM::globalAttributeNS(), name); // remove other attributes with same name
                attr.setName(name);
            } catch (CDMException ex) {
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
            if (name == "") throw CDMException("ncml-file "+ configFile + " has no name for attribute with orgName: "+ orgName);
            try {
                CDMAttribute& attr = cdm.getAttribute(varName, orgName);
                cdm.removeAttribute(varName, name); // remove other attributes with same name
                attr.setName(name);
            } catch (CDMException ex) {
                // no such attribute, don't care
            }
        }
    }
}


const boost::shared_ptr<Data> NcmlCDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
    // return unchanged data from this CDM
    const CDMVariable& variable = cdm.getVariable(varName);
    if (variable.hasData()) {
        return getDataSliceFromMemory(variable, unLimDimPos);
    }

    // find the original name, to fetch the data from the dataReader
    std::string orgVarName = varName;
    map<string, string>::iterator vit = variableNameChanges.find(varName);
    if (vit != variableNameChanges.end()) {
        orgVarName = vit->second;
    }
    boost::shared_ptr<Data> data = dataReader->getDataSlice(orgVarName, unLimDimPos);

    // eventually, change the type from the old type to the new type
    map<string, CDMDataType>::iterator dtIt = variableTypeChanges.find(varName);
    if (dtIt != variableTypeChanges.end()) {
        const CDM& orgCDM = dataReader->getCDM();
        double orgFill = orgCDM.getFillValue(orgVarName);
        CDMAttribute attr;
        double orgScale = 1.;
        double orgOffset = 0.;
        if (orgCDM.getAttribute(varName, "scale_factor", attr)) {
            orgScale = attr.getData()->asConstDouble()[0];
        }
        if (orgCDM.getAttribute(varName, "add_offset", attr)) {
            orgOffset = attr.getData()->asConstDouble()[0];
        }
        double newFill = cdm.getFillValue(varName);
        double newScale = 1.;
        double newOffset = 0.;
        if (cdm.getAttribute(varName, "scale_factor", attr)) {
            newScale = attr.getData()->asConstDouble()[0];
        }
        if (cdm.getAttribute(varName, "add_offset", attr)) {
            newOffset = attr.getData()->asConstDouble()[0];
        }

        data = data->convertDataType(orgFill, orgScale, orgOffset, dtIt->second, newFill, newScale, newOffset);
    }

    return data;
}


}
