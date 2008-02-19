#include "CDMExtractor.h"

namespace MetNoUtplukk
{

CDMExtractor::CDMExtractor(boost::shared_ptr<CDMReader> datareader)
: dataReader(datareader)
{
	cdm = dataReader->getCDM();
}

CDMExtractor::~CDMExtractor()
{
 }

const boost::shared_ptr<Data> CDMExtractor::getDataSlice(const CDMVariable& variable, size_t unLimDimPos) throw(CDMException)
{
	boost::shared_ptr<Data> data; 
	if (dimChanges.empty()) {
		// simple read
		data = dataReader->getDataSlice(variable, unLimDimPos);
	} else {
		// translate slice-variable size where dimensions have been transformed, (via data.slice)
		bool hasChangedDim = false;
		const std::vector<std::string>& dims = variable.getShape();
		std::vector<size_t> orgDimSize, newDimSize, newDimStart;
		const CDM& orgCDM = dataReader->getCDM();
		for (std::vector<std::string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
			const CDMDimension& dim = orgCDM.getDimension(*it);
			if (! dim.isUnlimited()) { // this is the slice-dim
				orgDimSize.push_back(dim.getLength());
				DimChangeMap::iterator foundDim = dimChanges.find(dim.getName());
				if (foundDim == dimChanges.end()) {
					newDimStart.push_back(0);
					newDimSize.push_back(dim.getLength());
				} else {
					hasChangedDim = true;
					newDimStart.push_back((foundDim->second)[0]);
					newDimSize.push_back((foundDim->second)[1]);
				}
			} else {
				// just changing unLimDimPos
				DimChangeMap::iterator foundDim = dimChanges.find(dim.getName());
				if (foundDim != dimChanges.end()) {
					unLimDimPos += (foundDim->second)[0];
				}								
			}
		}
		// read
		data = dataReader->getDataSlice(variable, unLimDimPos);
		if (hasChangedDim && (data->size() > 0)) { // datasize might be 0, i.e. if time doesn't exist
			data = data->slice(orgDimSize, newDimStart, newDimSize);
		}
	}
	// TODO: translate datatype where required
	return data;
}

void CDMExtractor::removeVariable(std::string variable) throw(CDMException)
{
	cdm.removeVariable(variable);
}

void CDMExtractor::reduceDimension(std::string dimName, size_t start, size_t length) throw(CDMException)
{
	CDMDimension& dim = cdm.getDimension(dimName);
	if (start+length >= dim.getLength()) {
		throw CDMException("can't enlarge dimension " + dimName + ": start+length out of bounds");
	}
	// keep track of changes
	dim.setLength(length);
	boost::array<size_t, 2> changes = { {start, length} };
	dimChanges[dimName] = changes; 
	
	
	// removing data of this dimension, just to be sure it's read from the dataReader
	try {
		CDMVariable& var = cdm.getVariable(dim.getName());
		//TODO fix: this deletes all data
		//var.setData(boost::shared_ptr<Data>());
	} catch (CDMException& ex) {
		// var doesn't exists, don't care
	}
}

void CDMExtractor::changeDataType(std::string variable, CDMDataType datatype) throw(CDMException)
{
	// TODO
	throw CDMException("not implemented yet");
}

} // end of namespace
