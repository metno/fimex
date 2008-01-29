#include "Data.h"

namespace MetNoUtplukk
{

boost::shared_ptr<Data> createData(CDMDataType datatype, size_t length) throw(CDMException) {
	int i = 0; // used as 0 InputIterator for first and last
	return createData(datatype, length, &i, &i);
}



}
