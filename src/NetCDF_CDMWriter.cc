/*
 * Fimex
 *
 * (C) Copyright 2008-2019, met.no
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

#include "fimex/NetCDF_CDMWriter.h"

#include "fimex/CDMDataType.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/NcmlCDMReader.h"
#include "fimex/String2Type.h"
#include "fimex/StringUtils.h"
#include "fimex/Units.h"
#include "fimex/UnitsException.h"
#include "fimex/XMLDoc.h"
#include "fimex/XMLInputFile.h"
#include "fimex/mifi_constants.h"

#include "NetCDF_Utils.h"

#include <functional>
#include <memory>
#include <numeric>

#include <libxml/tree.h>
#include <libxml/xpath.h>

namespace MetNoFimex {

namespace {

Logger_p logger = getLogger("fimex.NetCDF_CDMWriter");

int getNcVersion(int version, std::unique_ptr<XMLDoc>& doc)
{
    int retVal = NC_CLOBBER;
    switch (version) {
        case 3: retVal = NC_CLOBBER; break;
#ifdef NC_NETCDF4
        case 4: retVal = NC_CLOBBER | NC_CLASSIC_MODEL | NC_NETCDF4; break;
#endif
        default: LOG4FIMEX(logger, Logger::ERROR, "unknown netcdf-version "<< version << " using 3 instead"); break;
    }
    if (doc) {
        // set the default filetype
        xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/default[@filetype]");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        if (nodes->nodeNr) {
            const std::string filetype = string2lowerCase(getXmlProp(nodes->nodeTab[0], "filetype"));
            retVal = NC_CLOBBER;
            if (filetype == "netcdf3") {
                //retVal = NC_CLOBBER;
            } else if (filetype == "netcdf3_64bit") {
                retVal |= NC_64BIT_OFFSET;
            }
#ifdef NC_NETCDF4
            else if (filetype == "netcdf4") {
                retVal |= NC_NETCDF4;
            } else if (filetype == "netcdf4classic") {
                retVal |= NC_NETCDF4 | NC_CLASSIC_MODEL;
            }
#endif
            else {
                throw CDMException("unknown netcdf-filetype: " + filetype);
            }
        }
    }
    return retVal;
}

int ncDimId(int ncId, const CDMDimension* unLimDim)
{
    int unLimDimId = -1;
    if (unLimDim)
        NCMUTEX_LOCKED(ncCheck(nc_inq_dimid(ncId, unLimDim->getName().c_str(), &unLimDimId)));
    return unLimDimId;
}

void checkDoc(std::unique_ptr<XMLDoc>& doc, const std::string& filename)
{
    xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    const int size = (nodes) ? nodes->nodeNr : 0;
    if (size != 1)
        throw CDMException("no root element (/cdm_ncwriter_config) in " + filename);
}

} // namespace

NetCDF_CDMWriter::NetCDF_CDMWriter(CDMReader_p reader, const std::string& outputFile, std::string configFile, int version)
    : CDMWriter(reader, outputFile)
    , ncFile(new Nc())
{
    std::unique_ptr<XMLDoc> doc;
    if (!configFile.empty()) {
        doc.reset(new XMLDoc(configFile));
        checkDoc(doc, configFile);
    }
    const int ncVersion = getNcVersion(version, doc);
    ncFile->filename = outputFile;
#ifdef HAVE_MPI
    if (mifi_mpi_initialized() && (mifi_mpi_size > 1)) {
        LOG4FIMEX(logger, Logger::DEBUG, "opening parallel nc-file: " << ncFile->filename);
        NCMUTEX_LOCKED(ncCheck(nc_create_par(ncFile->filename.c_str(), ncVersion | NC_MPIIO, mifi_mpi_comm, mifi_mpi_info, &ncFile->ncId),
                               "creating " + ncFile->filename));
    } else
#endif
    {
        NCMUTEX_LOCKED(ncCheck(nc_create(ncFile->filename.c_str(), ncVersion, &ncFile->ncId), "creating " + ncFile->filename));
    }
    ncFile->isOpen = true;

    NCMUTEX_LOCKED(ncCheck(nc_inq_format(ncFile->ncId, &ncFile->format)));
#ifdef NC_NETCDF4
    if ((ncFile->format == NC_FORMAT_NETCDF4) || (ncFile->format == NC_FORMAT_NETCDF4_CLASSIC)) {
        if ((ncVersion & NC_CLASSIC_MODEL) != 0)
            LOG4FIMEX(logger, Logger::DEBUG, "netcdf4 format, classic mode");
        else
            LOG4FIMEX(logger, Logger::DEBUG, "netcdf4 format");
    } else
#endif
    {
        LOG4FIMEX(logger, Logger::DEBUG, "netcdf3 format");
    }
    if (ncFile->format < 3) {
        // using nofill for netcdf3 (2times io) -- does not affect netcdf4
        int oldFill;
        NCMUTEX_LOCKED(nc_set_fill(ncFile->ncId, NC_NOFILL, &oldFill));
    }
    initNcmlReader(doc);
    cdm = cdmReader->getCDM();
    initRemove(doc);
    // variable needs to be called before dimension!!!
    initFillRenameVariable(doc);
    initFillRenameDimension(doc);
    initFillRenameAttribute(doc);

    init();
}

void NetCDF_CDMWriter::initNcmlReader(std::unique_ptr<XMLDoc>& doc)
{
    if (doc) {
        xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/ncmlConfig");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        const int size = (nodes) ? nodes->nodeNr : 0;
        if (size == 1) {
            std::string configFile = getXmlProp(nodes->nodeTab[0], "filename");
            LOG4FIMEX(logger, Logger::DEBUG, "configuring CDMWriter with ncml config file: " << configFile);
            cdmReader = std::make_shared<NcmlCDMReader>(cdmReader, XMLInputFile(configFile));
        } else if (size > 1) {
            throw CDMException("multiple ncmlConfig elements");
        }
    }
}

void NetCDF_CDMWriter::initRemove(std::unique_ptr<XMLDoc>& doc)
{
    bool autoRemoveDims = true;
    if (doc) {
        {
            // remove global attributes
            xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/remove[@type='attribute']");
            xmlNodeSetPtr nodes = xpathObj->nodesetval;
            const int size = (nodes) ? nodes->nodeNr : 0;
            for (int i = 0; i < size; i++) {
                const std::string name = getXmlProp(nodes->nodeTab[i], "name");
                cdm.removeAttribute(CDM::globalAttributeNS(), name);
            }
        }
        {
            // remove variable attributes
            xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/variable/remove[@type='attribute']");
            xmlNodeSetPtr nodes = xpathObj->nodesetval;
            const int size = (nodes) ? nodes->nodeNr : 0;
            for (int i = 0; i < size; i++) {
                const std::string name = getXmlProp(nodes->nodeTab[i], "name");
                const std::string varName = getXmlProp(nodes->nodeTab[i]->parent, "name");
                cdm.removeAttribute(varName, name);
            }
        }
        {
            // remove variables
            xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/remove[@type='variable']");
            xmlNodeSetPtr nodes = xpathObj->nodesetval;
            const int size = (nodes) ? nodes->nodeNr : 0;
            for (int i = 0; i < size; i++) {
                const std::string name = getXmlProp(nodes->nodeTab[i], "name");
                LOG4FIMEX(logger, Logger::DEBUG, "Removing variables '" << name << "'");
                cdm.removeVariable(name);
            }
        }
        {
            // remove dimensions
            xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/remove[@type='dimension']");
            xmlNodeSetPtr nodes = xpathObj->nodesetval;
            const int size = (nodes) ? nodes->nodeNr : 0;
            for (int i = 0; i < size; i++) {
                const std::string name = getXmlProp(nodes->nodeTab[i], "name");
                if (cdm.testDimensionInUse(name)) {
                    LOG4FIMEX(logger, Logger::WARN, "Cannot remove dimension in use: '" << name << "'");
                } else {
                    LOG4FIMEX(logger, Logger::DEBUG, "Removing dimension '" << name << "'");
                    cdm.removeDimension(name);
                }
            }
        }
        {
            // check for autoRemoveUnusedDimensions
            xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/default[@autoRemoveUnusedDimension='false']");
            xmlNodeSetPtr nodes = xpathObj->nodesetval;
            const int size = (nodes) ? nodes->nodeNr : 0;
            if (size > 0)
                autoRemoveDims = false;
        }
    }
    if (autoRemoveDims) {
        const CDM::DimVec dims = cdm.getDimensions(); // copy
        for (const CDMDimension& dim : dims) {
            if (!cdm.testDimensionInUse(dim.getName())) {
                LOG4FIMEX(logger, Logger::DEBUG, "Auto-removing dimension '" << dim.getName() << "'");
                cdm.removeDimension(dim.getName());
            }
        }
    }
}

void NetCDF_CDMWriter::initFillRenameDimension(std::unique_ptr<XMLDoc>& doc)
{
    if (doc) {
        xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/dimension[@newname]");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        const int size = (nodes) ? nodes->nodeNr : 0;
        for (int i = 0; i < size; i++) {
            LOG4FIMEX(logger, Logger::WARN, "Changing renaming dimensions in cdmWriterConfig.xml is deprecated, use ncmlCDMConfig instead!");
            const std::string name = getXmlProp(nodes->nodeTab[i], "name");
            const std::string newname = getXmlProp(nodes->nodeTab[i], "newname");
            cdm.getDimension(name); // check existence, throw exception
            dimensionNameChanges[name] = newname;
            // change dimension variable unless it has been changed
            if (variableNameChanges.find(name) == variableNameChanges.end()) {
                variableNameChanges[name] = newname;
            }
        }
    }
}

void NetCDF_CDMWriter::testVariableExists(const std::string& varName)
{
    try {
        cdm.getVariable(varName);
    } catch (CDMException& e) {
        throw CDMException(std::string("error modifying variable in writer: ") + e.what());
    }
}

void NetCDF_CDMWriter::initFillRenameVariable(std::unique_ptr<XMLDoc>& doc)
{
    if (doc) {
        xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/variable[@newname]");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        for (int i = 0; i < size; i++) {
            LOG4FIMEX(logger, Logger::WARN, "Changing variable-names in cdmWriterConfig.xml is deprecated, use ncmlCDMConfig instead!");
            const std::string name = getXmlProp(nodes->nodeTab[i], "name");
            testVariableExists(name);
            const std::string newname = getXmlProp(nodes->nodeTab[i], "newname");
            variableNameChanges[name] = newname;
        }
    }
    if (doc) {
        // read 'type' attribute and enable re-typeing of data
        xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/variable[@type]");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        for (int i = 0; i < size; i++) {
            const std::string name = getXmlProp(nodes->nodeTab[i], "name");
            const CDMDataType type = string2datatype(getXmlProp(nodes->nodeTab[i], "type"));
            CDMVariable& v = cdm.getVariable(name);
            v.setDataType(type);
            variableTypeChanges[name] = type;
        }
    }
    unsigned int defaultCompression = 13; // shuffle + comp-level 3
    if (doc) {
        // set the default compression level for all variables
        xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/default[@compressionLevel]");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        if (nodes->nodeNr) {
            defaultCompression = string2type<unsigned int>(getXmlProp(nodes->nodeTab[0], "compressionLevel"));
        }
    }
    for (const CDMVariable& var : cdm.getVariables()) {
        variableCompression[var.getName()] = defaultCompression;
    }
    if (doc) {
        // set the compression level for all variables
        xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/variable[@compressionLevel]");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        const int size = (nodes) ? nodes->nodeNr : 0;
        for (int i = 0; i < size; i++) {
            const std::string name = getXmlProp(nodes->nodeTab[i], "name");
            const unsigned int compression = string2type<unsigned int>(getXmlProp(nodes->nodeTab[i], "compressionLevel"));
            variableCompression[name] = compression;
        }
    }
    // chunking
    if (doc) {
        // set the compression level for all variables
        xmlXPathObject_p xpathObj = doc->getXPathObject("/cdm_ncwriter_config/dimension[@chunkSize]");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        const int size = (nodes) ? nodes->nodeNr : 0;
        for (int i = 0; i < size; i++) {
            const std::string name = getXmlProp(nodes->nodeTab[i], "name");
            const unsigned int chunkSize = string2type<unsigned int>(getXmlProp(nodes->nodeTab[i], "chunkSize"));
            dimensionChunkSize[name] = chunkSize;
        }
    }
}

void NetCDF_CDMWriter::initFillRenameAttribute(std::unique_ptr<XMLDoc>& doc)
{
    // make a complete copy of the original attributes
    if (!doc)
        return;

    xmlXPathObject_p xpathObj = doc->getXPathObject("//attribute");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    const int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        xmlNodePtr node = nodes->nodeTab[i];
        const std::string attName = getXmlProp(node, "name");
        if (attName != "units")
            LOG4FIMEX(logger, Logger::WARN, "Changing attributes in cdmWriterConfig.xml is deprecated, use ncmlCDMConfig instead!");
        std::string varName = CDM::globalAttributeNS();
        xmlNodePtr parent = node->parent;
        const std::string parentName = getXmlName(parent);
        if (parentName == "cdm_ncwriter_config") {
            // default
        } else if (parentName == "variable") {
            varName = getXmlProp(parent, "name");
            testVariableExists(varName);
        } else {
            throw CDMException("unknown parent of attribute " + attName + ": " + parentName);
        }

        const std::string attValue = getXmlProp(node, "value");
        const std::string attType = getXmlProp(node, "type");
        const std::string attNewName = getXmlProp(node, "newname");
        const std::string newAttrName = attNewName != "" ? attNewName : attName;
        CDMAttribute attr;
        if (!attType.empty()) {
            attr = CDMAttribute(newAttrName, attType, attValue);
        } else {
            const CDMAttribute& oldAttr = cdm.getAttribute(varName, attName);
            attr = CDMAttribute(newAttrName, oldAttr.getData());
        }
        cdm.removeAttribute(varName, attName); // remove the attribute with the old name
        cdm.addAttribute(varName, attr);       // set the attribute with the new name and data
    }
}

NetCDF_CDMWriter::NcDimIdMap NetCDF_CDMWriter::defineDimensions()
{
    NcDimIdMap ncDimMap;
    for (const CDMDimension& dim : cdm.getDimensions()) {
        int length;
#ifdef HAVE_MPI
        if (mifi_mpi_initialized() && (mifi_mpi_size > 1)) {
            // netcdf-MPI does not work with unlimited variables
            length = dim.getLength();
            if (length == 0)
                length = 1;
        } else
#endif
        {
            length = dim.isUnlimited() ? NC_UNLIMITED : dim.getLength();
        }
        if (!dim.isUnlimited()) {
            // length = 0 means unlimited in netcdf, so I create a dummy
            if (length == 0)
                length = 1;
        }
        // NcDim is organized by NcFile, no need to clean
        // change the name written to the file according to getDimensionName
        int dimId;
        NCMUTEX_LOCKED(ncCheck(nc_def_dim(ncFile->ncId, getDimensionName(dim.getName()).c_str(), length, &dimId)));
        ncDimMap[dim.getName()] = dimId;
        LOG4FIMEX(logger, Logger::DEBUG, "DimId of " << dim.getName() << " = " << dimId);
    }
    return ncDimMap;
}

NetCDF_CDMWriter::NcVarIdMap NetCDF_CDMWriter::defineVariables(const NcDimIdMap& ncDimIdMap)
{
    NcVarIdMap ncVarMap;
    for (const CDMVariable& var : cdm.getVariables()) {
        const std::vector<std::string>& shape = var.getShape();
        std::unique_ptr<int[]> ncshape(new int[shape.size()]);
        for (size_t i = 0; i < shape.size(); i++) {
            // revert order, cdm requires fastest moving first, netcdf requires fastest moving first
            ncshape[i] = ncDimIdMap.find(shape[(shape.size()-1-i)])->second;
        }
        CDMDataType datatype = var.getDataType();
        if (variableTypeChanges.find(var.getName()) != variableTypeChanges.end()) {
            CDMDataType& newType = variableTypeChanges[var.getName()];
            datatype = newType != CDM_NAT ? newType : datatype;
        }
        if (datatype == CDM_NAT && shape.empty()) {
            // empty variable, use int datatype
            datatype = CDM_INT;
        }
        int varId;
        LOG4FIMEX(logger, Logger::DEBUG, "defining variable " << var.getName() << " with shape '" << join(shape.begin(), shape.end()) << "' = " <<join(&ncshape[0], &ncshape[0]+shape.size()));
        NCMUTEX_LOCKED(
            ncCheck(nc_def_var(ncFile->ncId, getVariableName(var.getName()).c_str(), cdmDataType2ncType(datatype), shape.size(), &ncshape[0], &varId)));
        ncVarMap[var.getName()] = varId;
#ifdef NC_NETCDF4
        // set compression
        if ((ncFile->format == NC_FORMAT_NETCDF4) || (ncFile->format == NC_FORMAT_NETCDF4_CLASSIC)) {
#ifdef HAVE_MPI
            if (mifi_mpi_initialized() && (mifi_mpi_size > 1)) {
                LOG4FIMEX(logger, Logger::INFO, "disabling all compression, not possible with parallel HDF5");
            } else
#endif //HAVE_MPI
            {
                int compression = 0;
                int shuffle = 0;
                if (variableCompression.find(var.getName()) != variableCompression.end()) {
                    compression = variableCompression[var.getName()];
                    if (compression > 10) {
                        compression -= 10;
                        shuffle = 1;
                    }
                }
                if (compression > 0 && !shape.empty()) { // non-scalar variables
                    // create a chunk-strategy: continuous in last dimensions, max MAX_CHUNK
                    const size_t DEFAULT_CHUNK = 2 << 20; // good chunk up to 1M *sizeof(type)
                    const size_t MIN_CHUNK = 2 << 16;     // chunks should be at least reasonably sized, e.g. 64k*sizeof(type)
                    std::unique_ptr<size_t[]> ncChunk(new size_t[shape.size()]);
                    size_t chunkSize = 1;
                    for (size_t i = 0; i < shape.size(); i++) {
                        // revert order, cdm requires fastest moving first, netcdf-c requires fastest moving last
                        const CDMDimension& dim = cdm.getDimension(shape[i]);
                        const unsigned int dimSize = dim.isUnlimited() ? 1 : dim.getLength();
                        std::map<std::string, unsigned int>::const_iterator defaultChunk = dimensionChunkSize.find(shape[i]);
                        if (defaultChunk != dimensionChunkSize.end()) {
                            unsigned int chunkDim = clamp(1u, dimSize, defaultChunk->second);
                            chunkSize *= chunkDim;
                            ncChunk[shape.size() - 1 - i] = chunkDim;
                        } else {
                            const size_t lastChunkSize = chunkSize;
                            chunkSize *= dimSize;
                            if (chunkSize < DEFAULT_CHUNK) {
                                ncChunk[shape.size() - 1 - i] = dimSize;
                            } else {
                                size_t thisChunk = 1;
                                if (dimSize > 1 && (lastChunkSize < (MIN_CHUNK))) {
                                    // create a chunk-size which makes the total chunk ~= MIN_CHUNK
                                    thisChunk =
                                        clamp(1u, (unsigned int)floor(chunkSize / MIN_CHUNK), dimSize); // a number > 2^4 since chunkSize > DEFAULT_CHUNK
                                }
                                ncChunk[shape.size() - 1 - i] = thisChunk;
                            }
                        }
                    }
                    if (chunkSize > 0) {
                        LOG4FIMEX(logger, Logger::DEBUG, "chunk variable " << var.getName() << " to " << join(&ncChunk[0], &ncChunk[0] + shape.size(), "x"));
                        NCMUTEX_LOCKED(ncCheck(nc_def_var_chunking(ncFile->ncId, varId, NC_CHUNKED, ncChunk.get())));
                    }
                    // start compression
                    LOG4FIMEX(logger, Logger::DEBUG, "compressing variable " << var.getName() << " with level " << compression << " and shuffle=" << shuffle);
                    NCMUTEX_LOCKED(ncCheck(nc_def_var_deflate(ncFile->ncId, varId, shuffle, 1, compression)));
                }
            }
        }
#endif // NC_NETCDF4
    }
    return ncVarMap;
}

void NetCDF_CDMWriter::writeAttributes(const NcVarIdMap& ncVarMap)
{
    OmpScopedLock lock(Nc::getMutex());
    for (const auto& nmsp_att : cdm.getAttributes()) {
        int varId;
        if (nmsp_att.first == CDM::globalAttributeNS()) {
            varId = NC_GLOBAL;
        } else {
            varId = ncVarMap.find(nmsp_att.first)->second;
        }
        for (const CDMAttribute& attr : nmsp_att.second) {
            const CDMDataType dt = attr.getDataType();
            const nc_type nc_dt = cdmDataType2ncType(dt);
            DataPtr attrData = attr.getData();
            const size_t attrLen = attrData->size();
            const char* attrName = attr.getName().c_str();
            switch (dt) {
            case CDM_STRING: {
                const std::string text = attrData->asString();
                ncCheck(nc_put_att_text(ncFile->ncId, varId, attrName, text.size(), text.c_str()));
                break;
            }
            case CDM_CHAR:
                ncCheck(nc_put_att_schar(ncFile->ncId, varId, attrName, nc_dt, attrLen, reinterpret_cast<const signed char*>(attrData->asChar().get())));
                break;
            case CDM_SHORT:
                ncCheck(nc_put_att_short(ncFile->ncId, varId, attrName, nc_dt, attrLen, attrData->asShort().get()));
                break;
            case CDM_INT:
                ncCheck(nc_put_att_int(ncFile->ncId, varId, attrName, nc_dt, attrLen, attrData->asInt().get()));
                break;
            case CDM_FLOAT:
                ncCheck(nc_put_att_float(ncFile->ncId, varId, attrName, nc_dt, attrLen, attrData->asFloat().get()));
                break;
            case CDM_DOUBLE:
                ncCheck(nc_put_att_double(ncFile->ncId, varId, attrName, nc_dt, attrLen, attrData->asDouble().get()));
                break;
#ifdef NC_NETCDF4
            case CDM_STRINGS: {
                shared_array<std::string> svals = attrData->asStrings();
                if (ncFile->supports_nc_string()) {
                    std::unique_ptr<const char* []> cvals(new const char*[attrLen]);
                    for (size_t i=0; i<attrLen; ++i)
                        cvals[i] = svals[i].c_str();
                    ncCheck(nc_put_att(ncFile->ncId, varId, attrName, nc_dt, attrLen, &cvals[0]));
                } else if (attrLen == 1) {
                    // convert 1 string to char attribute
                    const std::string& s0 = svals[0];
                    ncCheck(nc_put_att_text(ncFile->ncId, varId, attrName, s0.size(), s0.c_str()));
                } else {
                    throw CDMException("cannot write CDM_STRINGS with more than 1 entry to this NetCDF format for attribute '" + attr.getName() + "'");
                }
                break;
            }
            case CDM_UCHAR:
                ncCheck(nc_put_att_uchar(ncFile->ncId, varId, attrName, nc_dt, attrLen, attrData->asUChar().get()));
                break;
            case CDM_USHORT:
                ncCheck(nc_put_att_ushort(ncFile->ncId, varId, attrName, nc_dt, attrLen, attrData->asUShort().get()));
                break;
            case CDM_UINT:
                ncCheck(nc_put_att_uint(ncFile->ncId, varId, attrName, nc_dt, attrLen, attrData->asUInt().get()));
                break;
            case CDM_INT64:
                ncCheck(nc_put_att_longlong(ncFile->ncId, varId, attrName, nc_dt, attrLen, attrData->asInt64().get()));
                break;
            case CDM_UINT64:
                ncCheck(nc_put_att_ulonglong(ncFile->ncId, varId, attrName, nc_dt, attrLen, attrData->asUInt64().get()));
                break;
#endif
            case CDM_NAT:
            default:
                throw CDMException("unknown datatype for attribute " + attr.getName());
            }
        }
    }
}

DataPtr NetCDF_CDMWriter::convertData(const CDMVariable& var, DataPtr data)
{
    const std::string& varName = var.getName();

    const std::string oldUnit = cdmReader->getCDM().getUnits(varName);
    const std::string newUnit = cdm.getUnits(varName);

    const CDMDataType oldType = var.getDataType();
    const std::map<std::string, CDMDataType>::const_iterator it = variableTypeChanges.find(varName);
    const CDMDataType newType = (it != variableTypeChanges.end()) ? it->second : oldType;

    const double oldFill = cdmReader->getCDM().getFillValue(varName);
    const double oldScale = cdmReader->getCDM().getScaleFactor(varName);
    const double oldOffset = cdmReader->getCDM().getAddOffset(varName);
    const double newFill = cdm.getFillValue(varName);
    const double newScale = cdm.getScaleFactor(varName);
    const double newOffset = cdm.getAddOffset(varName);

    if ((newType != CDM_NAT && oldType != CDM_NAT) &&
        (newType != oldType || newUnit != oldUnit || oldFill != newFill || oldScale != newScale || oldOffset != newOffset)) {
        try {
            UnitsConverter_p uc;
            if (oldUnit != newUnit) { // changes of the units
                Units units;
                uc = units.getConverter(oldUnit, newUnit);
            }
            data = data->convertDataType(oldFill, oldScale, oldOffset, uc, newType, newFill, newScale, newOffset);
        } catch (UnitException& e) {
            LOG4FIMEX(logger, Logger::WARN, "unable to convert data-units for variable " << var.getName() << ": " << e.what());
        } catch (CDMException& e) {
            // units not defined, do nothing
        }
    }
    return data;
}

void NetCDF_CDMWriter::writeData(const NcVarIdMap& ncVarMap)
{
    const CDMDimension* unLimDim = cdm.getUnlimitedDim();
    const int unLimDimId = ncDimId(ncFile->ncId, unLimDim);
    const long long maxUnLim = (unLimDim == 0) ? 0 : unLimDim->getLength();
    const CDM::VarVec& cdmVars = cdm.getVariables();

#ifdef HAVE_MPI
    const bool sliceAlongUnlimited = (maxUnLim > 3);
    const bool using_mpi = (mifi_mpi_initialized() && mifi_mpi_size > 1);
#endif

    // read data along unLimDim and then variables, otherwise netcdf3 reading might get very slow
    // see http://www.unidata.ucar.edu/support/help/MailArchives/netcdf/msg10905.html
    // use unLimDimPos = -1 for variables without unlimited dimension

    bool exceptions = false;
#ifdef _OPENMP
#if (defined(__GNUC__) && __GNUC__ >= 9) || defined(__clang__)
#pragma omp parallel for default(none) shared(logger, cdmVars, ncVarMap, maxUnLim, unLimDimId, exceptions)
#elif !defined(__INTEL_COMPILER) || (__INTEL_COMPILER >= 1800)
#pragma omp parallel for default(none) shared(logger, cdmVars, ncVarMap, exceptions)
#endif // __INTEL_COMPILER
#endif // _OPENMP
    for (long long unLimDimPos = -1; unLimDimPos < maxUnLim; ++unLimDimPos) {
#ifdef HAVE_MPI
        if (using_mpi) {
            if (sliceAlongUnlimited) { // MPI-slices along unlimited dimension
                // only work on variables which belong to this mpi-process (modulo-base)
                if ((unLimDimPos % mifi_mpi_size) != mifi_mpi_rank) {
                    LOG4FIMEX(logger, Logger::DEBUG, "processor " << mifi_mpi_rank << " skipping unLimDimPos " << unLimDimPos);
                    continue;
                } else {
                    LOG4FIMEX(logger, Logger::DEBUG, "processor " << mifi_mpi_rank << " working on unLimDimPos " << unLimDimPos);
                }
            }
        }
#endif
        for (size_t vi = 0; vi < cdmVars.size(); ++vi) {
            if (exceptions)
                continue;
            const CDMVariable& cdmVar = cdmVars[vi];
            const std::string& varName = cdmVar.getName();
            const int varId = ncVarMap.find(varName)->second;
#ifdef HAVE_MPI
            if (using_mpi) {
                NCMUTEX_LOCKED(ncCheck(nc_var_par_access(ncFile->ncId, varId, NC_INDEPENDENT)));
                if (!sliceAlongUnlimited) { // MPI-slices along unlimited dimension
                    // only work on variables which belong to this mpi-process (modulo-base along variable-ids)
                    if ((vi % mifi_mpi_size) != mifi_mpi_rank) {
                        LOG4FIMEX(logger, Logger::DEBUG, "processor " << mifi_mpi_rank << " skipping variable " << varName << "'");
                        continue;
                    } else {
                        LOG4FIMEX(logger, Logger::DEBUG, "processor " << mifi_mpi_rank << " working on variable '" << varName << "'");
                    }
                }
            }
#endif
            int n_dims;
            std::unique_ptr<int[]> dim_ids;
            std::unique_ptr<size_t[]> count;
            std::unique_ptr<size_t[]> start;
            int unLimDimIdx = -1;
            {
                OmpScopedLock ncLock(Nc::getMutex());

                ncCheck(nc_inq_varndims(ncFile->ncId, varId, &n_dims));

                dim_ids.reset(new int[n_dims]);
                ncCheck(nc_inq_vardimid(ncFile->ncId, varId, dim_ids.get()));

                start.reset(new size_t[n_dims]);
                count.reset(new size_t[n_dims]);
                for (int i = 0; i < n_dims; ++i) {
                    if (dim_ids[i] == unLimDimId)
                        unLimDimIdx = i;

                    size_t dim_len;
                    ncCheck(nc_inq_dimlen(ncFile->ncId, dim_ids[i], &dim_len));
                    start[i] = 0;
                    count[i] = dim_len;
                }
            }
            LOG4FIMEX(logger, Logger::DEBUG, "dimids of " << varName << ": " << join(&dim_ids[0], &dim_ids[0] + n_dims));

            const bool no_unlim = (unLimDimPos == -1 && unLimDimIdx == -1 && !cdm.hasUnlimitedDim(cdmVar));
            const bool with_unlim = (unLimDimPos != -1 && unLimDimIdx >= 0 && cdm.hasUnlimitedDim(cdmVar));
            DataPtr data;
            try {
                if (no_unlim) {
                    data = cdmReader->getData(varName);
                } else if (with_unlim) {
                    data = cdmReader->getDataSlice(varName, unLimDimPos);
                } else {
                    continue; // FIXME
                }
                if (data)
                    data = convertData(cdmVar, data);
            } catch (std::exception& ex) {
                std::ostringstream msg;
                msg << "exception while reading variable '" << varName << "'";
                if (!no_unlim)
                    msg << " at unlimited dim position " << unLimDimPos;
                msg << "; will stop writing data";
                msg << "; message: " << ex.what();
                LOG4FIMEX(logger, Logger::ERROR, msg.str());
                data = nullptr;
                exceptions = true;
            } catch (...) {
                std::ostringstream msg;
                msg << "exception while reading variable '" << varName << "'";
                if (!no_unlim)
                    msg << " at unlimited dim position " << unLimDimPos;
                msg << "; will stop writing data";
                LOG4FIMEX(logger, Logger::ERROR, msg.str());
                data = nullptr;
                exceptions = true;
            }
            if (exceptions)
                continue;

            if ((!data || data->size() == 0) && ncFile->format < 3) {
                // need to write data with _FillValue,
                // since we are using NC_NOFILL for nc3 format files = NC_FORMAT_CLASSIC(1) NC_FORMAT_64BIT(2))
                if (with_unlim)
                    count[unLimDimIdx] = 1; // just one slice
                size_t size = (n_dims > 0) ? std::accumulate(count.get(), count.get() + n_dims, 1, std::multiplies<size_t>()) : 1;
                data = createData(cdmVar.getDataType(), size, cdm.getFillValue(varName));
            }
            if (data && data->size() > 0) {
                if (with_unlim) {
                    count[unLimDimIdx] = 1;
                    start[unLimDimIdx] = unLimDimPos;
                }
                LOG4FIMEX(logger, Logger::DEBUG,
                          "writing variable " << varName << " dimLen= " << n_dims << " start=" << join(&start[0], &start[0] + n_dims)
                                              << " count=" << join(&count[0], &count[0] + n_dims));
                OmpScopedLock ncLock(Nc::getMutex());
                try {
                    ncPutValues(data, ncFile->ncId, varId, cdmDataType2ncType(cdmVar.getDataType()), n_dims, start.get(), count.get());
                } catch (std::exception& ex) {
                    OmpScopedUnlock ncUnlock(Nc::getMutex());
                    LOG4FIMEX(logger, Logger::ERROR, "exception " << ex.what() << " while writing variable " << varName);
                } catch (...) {
                    OmpScopedUnlock ncUnlock(Nc::getMutex());
                    LOG4FIMEX(logger, Logger::ERROR, "unknown exception while writing variable " << varName);
                }
            }
        }
#ifndef HAVE_MPI
        if (unLimDimPos >= 0) {
            NCMUTEX_LOCKED(ncCheck(nc_sync(ncFile->ncId))); // sync every 'time/unlimited' step (does not work with MPI)
        }
#endif
    }
    if (exceptions)
        throw CDMException("netcdf writing failed with ERRORs");
}

void NetCDF_CDMWriter::init()
{
    // write metadata
    const NcDimIdMap ncDimIdMap = defineDimensions();
    const NcVarIdMap ncVarIdMap = defineVariables(ncDimIdMap);
    writeAttributes(ncVarIdMap);
    NCMUTEX_LOCKED(ncCheck(nc_enddef(ncFile->ncId)));

    // write data
    writeData(ncVarIdMap);
}

NetCDF_CDMWriter::~NetCDF_CDMWriter()
{
}

const CDMAttribute& NetCDF_CDMWriter::getAttribute(const std::string& varName, const std::string& attName) const
{
    return cdm.getAttribute(varName, attName);
}

const std::string& NetCDF_CDMWriter::getVariableName(const std::string& varName) const
{
    std::map<std::string, std::string>::const_iterator it = variableNameChanges.find(varName);
    return (it != variableNameChanges.end()) ? it->second : varName;
}

const std::string& NetCDF_CDMWriter::getDimensionName(const std::string& dimName) const
{
    std::map<std::string, std::string>::const_iterator it = dimensionNameChanges.find(dimName);
    return (it != dimensionNameChanges.end()) ? it->second : dimName;
}

} // namespace MetNoFimex
