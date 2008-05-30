#include "NetCDF_CDMWriter.h"
#include "config.h"
extern "C" {
#include NETCDF_C_INCLUDE             // the C interface
}
#include <boost/shared_array.hpp>
#include "interpolation.h"
#include "CDMDataType.h"
#include "DataTypeChanger.h"
#include "NetCDF_Utils.h"
#include "Utils.h"
#include "XMLDoc.h"

namespace MetNoFimex
{

static NcBool putRecData(NcVar* var, CDMDataType dt, boost::shared_ptr<Data> data, size_t recNum) {
	if (data->size() == 0) return true;
	
	NcDim* dim = var->get_dim(0); // 0 dimension must be record dimension (unlimited if any)
	var->set_rec(dim, recNum);
	switch (dt) {
	case CDM_NAT: return false;
	case CDM_CHAR:
	case CDM_STRING: return var->put_rec(dim, data->asConstChar().get());
	case CDM_SHORT:  return var->put_rec(dim, data->asConstShort().get());
	case CDM_INT: return var->put_rec(dim, data->asConstInt().get());
	case CDM_FLOAT: return var->put_rec(dim, data->asConstFloat().get());
	case CDM_DOUBLE: return var->put_rec(dim, data->asConstDouble().get());
	default: return false;
	}
	
}


static NcBool putVarData(NcVar* var, CDMDataType dt, boost::shared_ptr<Data> data) {
	size_t size = data->size();
	if (size == 0) return true;
	
	boost::shared_array<long> edges(var->edges());
	int dims = var->num_dims();
	int dim_size = 1;
	for (int i = 0; i < dims; i++) {
		dim_size *= edges[i];
	}
	if (size != static_cast<size_t>(dim_size)) {
		return false;
	}
	
	switch (dt) {
	case CDM_NAT: return false;
	case CDM_CHAR:
	case CDM_STRING: return var->put(data->asChar().get(),  edges.get());
	case CDM_SHORT:  return var->put(data->asShort().get(), edges.get());
	case CDM_INT:    return var->put(data->asInt().get(),   edges.get());
	case CDM_FLOAT:  return var->put(data->asFloat().get(), edges.get());
	case CDM_DOUBLE: return var->put(data->asDouble().get(),edges.get());
	default: return false;
	}
	
}

NetCDF_CDMWriter::NetCDF_CDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile)
: CDMWriter(cdmReader, outputFile), ncErr(NcError::verbose_nonfatal), ncFile(outputFile.c_str(), NcFile::Replace)
{
	// make a local copy of attributes (required for config)
	attributes = cdmReader->getCDM().getAttributes();
	init();
}

NetCDF_CDMWriter::NetCDF_CDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile, const std::string& configFile)
: CDMWriter(cdmReader, outputFile), ncErr(NcError::verbose_nonfatal), ncFile(outputFile.c_str(), NcFile::Replace)
{
	std::auto_ptr<XMLDoc> doc(new XMLDoc(configFile));
	// variable needs to be called before dimension!!!
	initFillRenameVariable(doc);
	initFillRenameDimension(doc);
	initFillRenameAttribute(doc);
	init();
}

void NetCDF_CDMWriter::initFillRenameDimension(const std::auto_ptr<XMLDoc>& doc) throw(CDMException)
{
	XPathObjPtr xpathObj = doc->getXPathObject("/cdm_ncwriter_config/dimension[@newname]");
	xmlNodeSetPtr nodes = xpathObj->nodesetval;
	int size = (nodes) ? nodes->nodeNr : 0;
	for (int i = 0; i < size; i++) {
		std::string name = getXmlProp(nodes->nodeTab[i], "name");
		std::string newname = getXmlProp(nodes->nodeTab[i], "newname");
		dimensionNameChanges[name] = newname;
		// change dimension variable unless it has been changed
		if (variableNameChanges.find(name) == variableNameChanges.end()) {
			variableNameChanges[name] = newname;
		}
	}
}

void NetCDF_CDMWriter::initFillRenameVariable(const std::auto_ptr<XMLDoc>& doc) throw(CDMException)
{
	XPathObjPtr xpathObj = doc->getXPathObject("/cdm_ncwriter_config/variable[@newname]");
	xmlNodeSetPtr nodes = xpathObj->nodesetval;
	int size = (nodes) ? nodes->nodeNr : 0;
	for (int i = 0; i < size; i++) {
		std::string name = getXmlProp(nodes->nodeTab[i], "name");
		std::string newname = getXmlProp(nodes->nodeTab[i], "newname");
		variableNameChanges[name] = newname;
	}
	// read 'type' attribute and enable re-typeing of data
	xpathObj = doc->getXPathObject("/cdm_ncwriter_config/variable[@type]");
	nodes = xpathObj->nodesetval;
	size = (nodes) ? nodes->nodeNr : 0;
	for (int i = 0; i < size; i++) {
		std::string name = getXmlProp(nodes->nodeTab[i], "name");
		CDMDataType type = string2datatype(getXmlProp(nodes->nodeTab[i], "type"));
		variableTypeChanges[name] = type;
	}
}

void NetCDF_CDMWriter::initFillRenameAttribute(const std::auto_ptr<XMLDoc>& doc) throw(CDMException)
{
	// make a complete copy of the original attributes
	attributes = cdmReader->getCDM().getAttributes();
	XPathObjPtr xpathObj = doc->getXPathObject("//attribute");
	xmlNodeSetPtr nodes = xpathObj->nodesetval;
	int size = (nodes) ? nodes->nodeNr : 0;
	for (int i = 0; i < size; i++) {
		xmlNodePtr node = nodes->nodeTab[i];
		std::string attName = getXmlProp(node, "name");
		std::string varName = CDM::globalAttributeNS();
		xmlNodePtr parent = node->parent;
		std::string parentName = getXmlName(parent);
		if (parentName == "cdm_ncwriter_config") {
			// default
		} else if (parentName == "variable") {
			varName = getXmlProp(parent, "name");
		} else {
			throw CDMException("unknown parent of attribute "+attName+": "+parentName);
		}
		
		std::string attValue = getXmlProp(node, "value");
		std::string attType = getXmlProp(node, "type");
		std::string attNewName = getXmlProp(node, "newname");
		if (attNewName != "") {
			attributeNameChanges[varName][attName] = attNewName;
		}
		if (attType != "") {
			attributes[varName][attName] = CDMAttribute(attName, attType, attValue);
		}	
	}
}


NetCDF_CDMWriter::NcDimMap NetCDF_CDMWriter::defineDimensions() {
	const CDM& cdm = cdmReader->getCDM();
	const CDM::StrDimMap& cdmDims = cdm.getDimensions();
	NcDimMap ncDimMap;
	for (CDM::StrDimMap::const_iterator it = cdmDims.begin(); it != cdmDims.end(); ++it) {
		int length = it->second.isUnlimited() ? NC_UNLIMITED : it->second.getLength();
		// NcDim is organized by NcFile, no need to clean
		// change the name written to the file according to getDimensionName
		NcDim* dim = ncFile.add_dim(getDimensionName(it->first).c_str(), length); 
		if (dim == 0) throw CDMException(nc_strerror(ncErr.get_err()));
		ncDimMap[it->first] = dim;
	}
	return ncDimMap;
}

NetCDF_CDMWriter::NcVarMap NetCDF_CDMWriter::defineVariables(const NcDimMap& ncDimMap) {
	const CDM& cdm = cdmReader->getCDM();
	const CDM::StrVarMap& cdmVars = cdm.getVariables();
	NcVarMap ncVarMap;
	for (CDM::StrVarMap::const_iterator it = cdmVars.begin(); it != cdmVars.end(); ++it) {
		const CDMVariable& var = it->second;
		const std::vector<std::string>& shape = var.getShape();
		// the const-ness required by the library
		typedef const NcDim* NcDimPtr;
		boost::shared_array<NcDimPtr> ncshape(new NcDimPtr[shape.size()]);
		for (size_t i = 0; i < shape.size(); i++) {
			// revert order, cdm requires fastest moving first, netcdf-cplusplus requires fastest moving first
			ncshape[i] = ncDimMap.find(shape[(shape.size()-1-i)])->second;
		}
		CDMDataType datatype = var.getDataType();
		if (variableTypeChanges.find(var.getName()) != variableTypeChanges.end()) {
			CDMDataType& newType = variableTypeChanges[var.getName()];
			datatype = newType != CDM_NAT ? newType : datatype;
		}
		if (datatype == CDM_NAT && shape.size() == 0) {
			// empty variable, use int datatype
			datatype = CDM_INT;
		}
		NcVar *ncVar = ncFile.add_var(getVariableName(var.getName()).c_str(), cdmDataType2ncType(datatype), shape.size(), ncshape.get());
		if (! ncVar) {
			throw CDMException(nc_strerror(ncErr.get_err()));
		}
		ncVarMap[it->first] = ncVar;
	}
	return ncVarMap;
}

void NetCDF_CDMWriter::writeAttributes(const NcVarMap& ncVarMap) {
	// using C interface since it offers a combined interface to global and var attributes
	for (CDM::StrStrAttrMap::const_iterator it = attributes.begin(); it != attributes.end(); ++it) {
		int varId;
		if (it->first == CDM::globalAttributeNS()) {
			varId = NC_GLOBAL;
		} else {
			varId = ncVarMap.find(it->first)->second->id();
		}
		for (CDM::StrAttrMap::const_iterator ait = it->second.begin(); ait != it->second.end(); ++ait) {
			int errCode = NC_NOERR;
			const CDMAttribute& attr = ait->second;
			CDMDataType dt = attr.getDataType();
			switch (dt) {
			case CDM_STRING: ;
			case CDM_CHAR:
				errCode = nc_put_att_text(ncFile.id(), varId, getAttributeName(it->first, attr.getName()).c_str(), attr.getData()->size(), attr.getData()->asChar().get() );
				break;
			case CDM_SHORT:
				errCode = nc_put_att_short(ncFile.id(), varId, getAttributeName(it->first, attr.getName()).c_str(), static_cast<nc_type>(cdmDataType2ncType(dt)), attr.getData()->size(), attr.getData()->asShort().get() );
				break;
			case CDM_INT:
				errCode = nc_put_att_int(ncFile.id(), varId, getAttributeName(it->first, attr.getName()).c_str(), static_cast<nc_type>(cdmDataType2ncType(dt)), attr.getData()->size(), attr.getData()->asInt().get() );
				break;
			case CDM_FLOAT:
				errCode = nc_put_att_float(ncFile.id(), varId, getAttributeName(it->first, attr.getName()).c_str(), static_cast<nc_type>(cdmDataType2ncType(dt)), attr.getData()->size(), attr.getData()->asFloat().get() );
				break;
			case CDM_DOUBLE:
				errCode = nc_put_att_double(ncFile.id(), varId, getAttributeName(it->first, attr.getName()).c_str(), static_cast<nc_type>(cdmDataType2ncType(dt)), attr.getData()->size(), attr.getData()->asDouble().get() );
				break;
			case CDM_NAT:
			default: throw CDMException("unknown datatype for attribute " + attr.getName());
			}
			if (errCode != NC_NOERR) {
				throw CDMException(nc_strerror(ncErr.get_err()));
			}
		}
	}	
}

void NetCDF_CDMWriter::writeData(const NcVarMap& ncVarMap) {
	const CDM& cdm = cdmReader->getCDM();
	const CDM::StrVarMap& cdmVars = cdm.getVariables();
	for (CDM::StrVarMap::const_iterator it = cdmVars.begin(); it != cdmVars.end(); ++it) {
		const std::string& varName = it->first;
		const CDMVariable& cdmVar = it->second;
		DataTypeChanger dtc(cdmVar.getDataType());
		if ((variableTypeChanges.find(varName) != variableTypeChanges.end()) &&
			(variableTypeChanges[varName] != CDM_NAT)) {
			double oldFill = MIFI_UNDEFINED_D;
			try {
				const CDMAttribute& attr = cdm.getAttribute(varName, "_FillValue");
				oldFill = attr.getData()->asDouble()[0];
			} catch (CDMException& e) {
			}
			double oldScale = 1.;
			try {
				const CDMAttribute& attr = cdm.getAttribute(varName, "scale_factor");
				oldScale = attr.getData()->asDouble()[0];
			} catch (CDMException& e) {
			}
			double oldOffset = 0.;
			try {
				const CDMAttribute& attr = cdm.getAttribute(varName, "add_offset");
				oldOffset = attr.getData()->asDouble()[0];
			} catch (CDMException& e) {
			}
			double newFill = MIFI_UNDEFINED_D;
			try {
				const CDMAttribute& attr = getAttribute(varName, "_FillValue");
				newFill = attr.getData()->asDouble()[0];
			} catch (CDMException& e) {
			}
			double newScale = 1.;
			try {
				const CDMAttribute& attr = getAttribute(varName, "scale_factor");
				newScale = attr.getData()->asDouble()[0];
			} catch (CDMException& e) {
			}
			double newOffset = 0.;
			try {
				const CDMAttribute& attr = cdm.getAttribute(varName, "add_offset");
				newOffset = attr.getData()->asDouble()[0];
			} catch (CDMException& e) {
			}
			std::cerr << cdmVar.getName() << " " << oldFill << " " << oldScale << " " << oldOffset << " " << " " << newFill << " " << newScale << " " << newOffset << std::endl;
			dtc = DataTypeChanger(cdmVar.getDataType(), oldFill, oldScale, oldOffset, variableTypeChanges[cdmVar.getName()], newFill, newScale, newOffset);
		}
		NcVar* ncVar = ncVarMap.find(cdmVar.getName())->second;
		if (!cdm.hasUnlimitedDim(cdmVar)) {
			boost::shared_ptr<Data> data = cdmReader->getDataSlice(cdmVar.getName());
			try {
				data = dtc.convertData(data);
			} catch (CDMException& e) {
				throw CDMException("problems writing data to var " + cdmVar.getName() + ": " + e.what());
			}
			if (!putVarData(ncVar, dtc.getDataType(), data)) {
				throw CDMException("problems writing data to var " + cdmVar.getName() + ": " + nc_strerror(ncErr.get_err()) + ", datalength: " + type2string(data->size()));
			}
		} else {
			// iterate over each unlimited dim (usually time)
			const CDMDimension* unLimDim = cdm.getUnlimitedDim();
			for (size_t i = 0; i < unLimDim->getLength(); ++i) {
				boost::shared_ptr<Data> data = cdmReader->getDataSlice(cdmVar.getName(), i);
				try {
					data = dtc.convertData(data);
				} catch (CDMException& e) {
					throw CDMException("problems writing data to var " + cdmVar.getName() + ": " + e.what());
				}
				if (!putRecData(ncVar, dtc.getDataType(), data, i)) {
					throw CDMException("problems writing datarecord " + type2string(i) + " to var " + cdmVar.getName() + ": " + nc_strerror(ncErr.get_err()) + ", datalength: " + type2string(data->size()));
				}
			}
		}
	}
}
	
void NetCDF_CDMWriter::init() throw(CDMException)
{
	// write metadata
	if (! ncFile.is_valid()) {
		throw CDMException(nc_strerror(ncErr.get_err()));
	}
	NcDimMap ncDimMap = defineDimensions();
	NcVarMap ncVarMap = defineVariables(ncDimMap);
	writeAttributes(ncVarMap);
	writeData(ncVarMap);	
}

NetCDF_CDMWriter::~NetCDF_CDMWriter()
{
}

const CDMAttribute& NetCDF_CDMWriter::getAttribute(const std::string& varName, const std::string& attName) const throw(CDMException)
{
	if (attributes.find(varName) == attributes.end()) {
		throw CDMException("could not find variable "+varName+" in NetcdfWriter attribute list");
	}
	if (attributes.find(varName)->second.find(attName) == attributes.find(varName)->second.end()) {
		throw CDMException("could not find attribute "+attName+" for variable "+varName+" in NetcdfWriter attribute list");		
	}
	return attributes.find(varName)->second.find(attName)->second;
}

const std::string& NetCDF_CDMWriter::getAttributeName(const std::string& varName, const std::string& attName) const
{
	if (attributeNameChanges.find(varName) != attributeNameChanges.end()) {
		if (attributeNameChanges.find(varName)->second.find(attName) != attributeNameChanges.find(varName)->second.end()) {
			return attributeNameChanges.find(varName)->second.find(attName)->second;
		}
	}
	return attName;
}

const std::string& NetCDF_CDMWriter::getVariableName(const std::string& varName) const
{
	if (variableNameChanges.find(varName) == variableNameChanges.end()) {
		return varName;
	} else {
		return variableNameChanges.find(varName)->second;
	}
}
const std::string& NetCDF_CDMWriter::getDimensionName(const std::string& dimName) const
{
	if (dimensionNameChanges.find(dimName) == dimensionNameChanges.end()) {
		return dimName;
	} else {
		return dimensionNameChanges.find(dimName)->second;
	}
}

}
