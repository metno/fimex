#include "Data.h"

namespace MetNoUtplukk
{

boost::shared_ptr<Data> createData(CDMDataType datatype, long length) {
	switch (datatype) {
		case CDM_DOUBLE: return boost::shared_ptr<Data>(new DataImpl<double>(length));
		case CDM_FLOAT: return boost::shared_ptr<Data>(new DataImpl<float>(length));
		case CDM_INT: return boost::shared_ptr<Data>(new DataImpl<int>(length));
		case CDM_SHORT: return boost::shared_ptr<Data>(new DataImpl<short>(length));
		case CDM_CHAR: return boost::shared_ptr<Data>(new DataImpl<char>(length));
		case CDM_NAT: ;
		default: ;
	}
	return boost::shared_ptr<Data>();
}



}
