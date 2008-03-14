#include "NetCDF_Utils.h"
#include <boost/shared_array.hpp>

namespace MetNoUtplukk
{

NcType cdmDataType2ncType(CDMDataType dt) {
	switch (dt) {
	case CDM_NAT: return ncNoType;
	case CDM_CHAR: return ncChar;
	case CDM_STRING: return ncChar;
	case CDM_SHORT: return ncShort;
	case CDM_INT: return ncInt;
	case CDM_FLOAT: return ncFloat;
	case CDM_DOUBLE: return ncDouble;
	default: return ncNoType;
	}
}

CDMDataType ncType2cdmDataType(NcType dt) {
	switch (dt) {
	case ncNoType: return CDM_NAT;
	case ncByte: return CDM_CHAR;
	case ncChar: return CDM_STRING;
	case ncShort: return CDM_SHORT;
	case ncInt: return CDM_INT;
//	case ncLong: return CDM_INT; // ncLong is deprecated, and identical to ncInt
	case ncFloat: return CDM_FLOAT;
	case ncDouble: return CDM_DOUBLE;
	default: return CDM_NAT;
	}
}

boost::shared_ptr<Data> ncValues2Data(NcValues* values, NcType dt, size_t length) {
	switch (dt) {
	case ncByte: return boost::shared_ptr<Data>(new DataImpl<char>(boost::shared_array<char>(reinterpret_cast<char*>(values->base())), length));
	case ncChar: return boost::shared_ptr<Data>(new DataImpl<char>(boost::shared_array<char>(reinterpret_cast<char*>(values->base())), length));
	case ncShort: return boost::shared_ptr<Data>(new DataImpl<short>(boost::shared_array<short>(reinterpret_cast<short*>(values->base())), length));
	case ncInt: return boost::shared_ptr<Data>(new DataImpl<int>(boost::shared_array<int>(reinterpret_cast<int*>(values->base())), length));
//	case ncLong: return CDM_INT; // ncLong is deprecated, and identical to ncInt
	case ncFloat: return boost::shared_ptr<Data>(new DataImpl<float>(boost::shared_array<float>(reinterpret_cast<float*>(values->base())), length));
	case ncDouble: return boost::shared_ptr<Data>(new DataImpl<double>(boost::shared_array<double>(reinterpret_cast<double*>(values->base())), length));
	case ncNoType: delete values; return boost::shared_ptr<DataImpl<int> >(new DataImpl<int>(boost::shared_array<int>(new int[0]), 0));
	}
}


}
