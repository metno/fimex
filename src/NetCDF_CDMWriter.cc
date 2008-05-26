#include "NetCDF_CDMWriter.h"
#include "config.h"
extern "C" {
#include NETCDF_C_INCLUDE             // the C interface
}
#include <boost/shared_array.hpp>
#include "CDMDataType.h"
#include "NetCDF_Utils.h"
#include "Utils.h"

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
	init();
}

NetCDF_CDMWriter::NetCDF_CDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile, const std::string& configFile)
: CDMWriter(cdmReader, outputFile), ncErr(NcError::verbose_nonfatal), ncFile(outputFile.c_str(), NcFile::Replace)
{
		
	init();
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
	const CDM& cdm = cdmReader->getCDM();
	const CDM::StrStrAttrMap& cdmAttrs = cdm.getAttributes();
	// using C interface since it offers a combined interface to global and var attributes
	for (CDM::StrStrAttrMap::const_iterator it = cdmAttrs.begin(); it != cdmAttrs.end(); ++it) {
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
		const CDMVariable& cdmVar = it->second;
		NcVar* ncVar = ncVarMap.find(cdmVar.getName())->second;
		if (!cdm.hasUnlimitedDim(cdmVar)) {
			boost::shared_ptr<Data> data = cdmReader->getDataSlice(cdmVar.getName());
			if (!putVarData(ncVar, cdmVar.getDataType(), data)) {
				throw CDMException("problems writing data to var " + cdmVar.getName() + ": " + nc_strerror(ncErr.get_err()) + ", datalength: " + type2string(data->size()));
			}
		} else {
			// iterate over each unlimited dim (usually time)
			const CDMDimension* unLimDim = cdm.getUnlimitedDim();
			for (size_t i = 0; i < unLimDim->getLength(); ++i) {
				boost::shared_ptr<Data> data = cdmReader->getDataSlice(cdmVar.getName(), i);
				if (!putRecData(ncVar, cdmVar.getDataType(), data, i)) {
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

const std::string& NetCDF_CDMWriter::getAttributeName(const std::string& varName, const std::string& attName) const
{
	if ((attributeNameChanges.find(varName) == attributeNameChanges.end()) ||
		(attributeNameChanges.find(varName)->second.find(attName) == attributeNameChanges.find(varName)->second.end())) {
		return attName;
	}
	return attributeNameChanges.find(varName)->second.find(attName)->second;
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
