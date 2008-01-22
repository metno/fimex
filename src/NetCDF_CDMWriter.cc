#include "NetCDF_CDMWriter.h"
extern "C" {
#include "netcdf.h"             // the C interface
}
#include <boost/shared_array.hpp>

namespace MetNoUtplukk
{

static void dontDelete(void *) {} // used as non-destructor of shared_ptr

NetCDF_CDMWriter::NetCDF_CDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile)
: CDMWriter(cdmReader, outputFile), ncFile(outputFile.c_str(), NcFile::Replace)
{
	NcError ncErr(NcError::verbose_nonfatal);
	// write metadata
	if (! ncFile.is_valid()) {
		throw CDMException(nc_strerror(ncErr.get_err()));
	}
	const CDM& cdm = cdmReader->getCDM();
	
	// define dims
	typedef std::map<std::string, boost::shared_ptr<NcDim> > NcDimMap;
	const CDM::StrDimMap& cdmDims = cdm.getDimensions();
	NcDimMap ncDimMap;
	for (CDM::StrDimMap::const_iterator it = cdmDims.begin(); it != cdmDims.end(); ++it) {
		int length = it->second.isUnlimited() ? NC_UNLIMITED : it->second.getLength();
		boost::shared_ptr<NcDim> dim(ncFile.add_dim(it->first.c_str(), length), dontDelete); // NcDim is organized by NcFile
		if (dim.get() == 0) throw CDMException(nc_strerror(ncErr.get_err()));
		ncDimMap[it->first] = dim;
	}

	// define vars
	typedef std::map<std::string, boost::shared_ptr<NcVar> > NcVarMap;
	const CDM::StrVarMap& cdmVars = cdm.getVariables();
	for (CDM::StrVarMap::const_iterator it = cdmVars.begin(); it != cdmVars.end(); ++it) {
		const CDMVariable& var = it->second;
		const std::vector<CDMDimension>& shape = var.getShape();
		// the const-ness required by the library
		typedef const NcDim* NcDimPtr;
		boost::shared_array<NcDimPtr> ncshape(new NcDimPtr[shape.size()]);
		for (size_t i = 0; i < shape.size(); i++) {
			// TODO: maybe the shape should be converted in the reader?
			// revert shape to get Time from last to first index
			std::cerr << shape[i].getName() << " " << ncDimMap[shape[i].getName()]->name() << " " << ncDimMap[shape[i].getName()]->size() << std::endl;
			//ncshape[shape.size() - 1 + i] = ncDimMap[shape[i].getName()].get();
			ncshape[shape.size() - i - 1] = ncDimMap[shape[i].getName()].get();
			std::cerr << "shape " << (i) << ": " << shape[i].getName() << " id: " << ncshape[shape.size() - i -1]->name() << std::endl;
		}
		// TODO: fixe data-type
		std::cerr << "add_var(" << var.getName() << ", ncFloat, " << shape.size() << ", ...)" << std::endl; 
		boost::shared_ptr<NcVar> ncVar(ncFile.add_var(var.getName().c_str(), ncFloat, shape.size(), ncshape.get()), dontDelete);
	}
	// write data
	
}

NetCDF_CDMWriter::~NetCDF_CDMWriter()
{
}

}
