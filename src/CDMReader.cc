#include "CDMReader.h"

namespace MetNoUtplukk {

const boost::shared_ptr<Data> CDMReader::getDataFromMemory(const CDMVariable& variable, size_t unLimDimPos) throw(CDMException)
{
	if (variable.hasData()) {
		if (cdm.hasUnlimitedDim(variable)) {
			// cut out the unlimited dim data
			size_t sliceSize = 1;
			std::vector<std::string> shape = variable.getShape();
			for (std::vector<std::string>::const_iterator it = shape.begin(); it
					!= shape.end(); ++it) {
				CDMDimension& dim = cdm.getDimension(*it);
				if (!dim.isUnlimited()) {
					sliceSize *= dim.getLength();
				}
			}
			return createDataSlice(variable.getDataType(),
					*(variable.getData()), unLimDimPos*sliceSize, sliceSize);
		} else {
			return variable.getData();
		}
	} else {
		return boost::shared_ptr<Data>();
	}
}

}
