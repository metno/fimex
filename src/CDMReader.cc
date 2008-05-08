#include "CDMReader.h"
#include <boost/bind.hpp>

namespace MetNoFimex {

static void addSizeUnlessUnlimited(const CDM& cdm, const std::string& dimName, size_t& size) {
	const CDMDimension& dim = cdm.getDimension(dimName);
	if (!dim.isUnlimited()) {
		size *= dim.getLength();
	}	
}

// retrieve size of data slice
static size_t getSliceSize(const CDM& cdm, const CDMVariable& variable) {
	size_t sliceSize = 1;
	std::vector<std::string> shape = variable.getShape();
	for_each(shape.begin(), shape.end(), boost::bind(addSizeUnlessUnlimited, cdm, _1, sliceSize));
	return sliceSize;
}

const boost::shared_ptr<Data> CDMReader::getDataFromMemory(const CDMVariable& variable, size_t unLimDimPos) throw(CDMException)
{
	if (variable.hasData()) {
		if (cdm.hasUnlimitedDim(variable)) {
			// cut out the unlimited dim data
			size_t sliceSize = getSliceSize(cdm, variable);
			return createDataSlice(variable.getDataType(), *(variable.getData()), unLimDimPos*sliceSize, sliceSize);
		} else {
			return variable.getData();
		}
	} else {
		return boost::shared_ptr<Data>();
	}
}

}
