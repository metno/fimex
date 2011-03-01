/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

#include "NetCDF_Utils.h"
#include <boost/shared_array.hpp>
#include "fimex/DataImpl.h"

namespace MetNoFimex
{

NcType cdmDataType2ncType(CDMDataType dt) {
	switch (dt) {
	case CDM_NAT: return ncNoType;
	case CDM_CHAR: return ncByte;
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
	case ncNoType:
	default: delete values; return boost::shared_ptr<DataImpl<int> >(new DataImpl<int>(boost::shared_array<int>(new int[0]), 0));
	}
}


}
