#include "NetCDF_CF10_CDMReader.h"
#include "NetCDF_Utils.h"

namespace MetNoUtplukk
{

NetCDF_CF10_CDMReader::NetCDF_CF10_CDMReader(const std::string& filename)
: filename(filename), ncFile(filename.c_str(), NcFile::ReadOnly)
{
	NcError ncErr(NcError::verbose_nonfatal);
	if (!ncFile.is_valid()) {
		throw CDMException(nc_strerror(ncErr.get_err()));
	}
	
	// read metadata to cdm
	// define dimensions
	for (int i = 0; i < ncFile.num_dims(); ++i) {
		NcDim* dim = ncFile.get_dim(i);
		CDMDimension d(dim->name(), dim->size());
		d.setUnlimited(dim->is_unlimited());
		cdm.addDimension(d);
	}
	// define variables
	for (int i = 0; i < ncFile.num_vars(); ++i) {
		NcVar* var = ncFile.get_var(i);
		CDMDataType type = ncType2cdmDataType(var->type());
		std::vector<std::string> shape;
		// reverse dimensions
		for (int j = var->num_dims()-1; j >= 0; --j) {
			NcDim* dim = var->get_dim(j);
			shape.push_back(dim->name());
		}
		cdm.addVariable(CDMVariable(var->name(), type, shape));
		// define the attributes of the variable
		for (int j = 0; j < var->num_atts(); ++j) {
			addAttribute(var->name(), var->get_att(j));
		}
	}
	// define global attributes
	for (int i = 0; i < ncFile.num_atts(); ++i) {
		addAttribute(cdm.globalAttributeNS(), ncFile.get_att(i));
	}
}

NetCDF_CF10_CDMReader::~NetCDF_CF10_CDMReader()
{
}

const boost::shared_ptr<Data> NetCDF_CF10_CDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
	const CDMVariable& var = cdm.getVariable(varName);
	if (var.hasData()) {
		return getDataFromMemory(var, unLimDimPos);
	}
	
	NcVar* ncVar = ncFile.get_var(var.getName().c_str());
	if (cdm.hasUnlimitedDim(var)) {
		return ncValues2Data(ncVar->get_rec(unLimDimPos), ncVar->type(), ncVar->rec_size());
	} else {
		return ncValues2Data(ncVar->values(), ncVar->type(), ncVar->num_vals());
	}
}

void NetCDF_CF10_CDMReader::addAttribute(const std::string& varName, NcAtt* ncAtt)
{
	CDMDataType dt(ncType2cdmDataType(ncAtt->type()));
	boost::shared_ptr<Data> attrData = ncValues2Data(ncAtt->values(), ncAtt->type(), ncAtt->num_vals());
	cdm.addAttribute(varName, CDMAttribute(ncAtt->name(), dt, attrData));
}


}
