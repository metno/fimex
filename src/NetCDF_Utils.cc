/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/
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
#include "fimex/Data.h"
#include "fimex/CDMException.h"
#include <boost/scoped_array.hpp>
#include <functional>
#include <numeric>

namespace MetNoFimex
{

void ncCheck(int status) {
    if (status != NC_NOERR)
        throw CDMException(nc_strerror(status));
}


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

CDMDataType ncType2cdmDataType(nc_type dt) {
    switch (dt) {
    case NC_NAT: return CDM_NAT;
    case NC_BYTE: return CDM_CHAR;
    case NC_CHAR: return CDM_STRING;
    case NC_SHORT: return CDM_SHORT;
    case NC_INT: return CDM_INT;
    case NC_FLOAT: return CDM_FLOAT;
    case NC_DOUBLE: return CDM_DOUBLE;
    case NC_STRING: return CDM_STRING;
    case NC_UBYTE: return CDM_UCHAR;
    case NC_USHORT: return CDM_USHORT;
    case NC_UINT: return CDM_UINT;
    case NC_INT64: return CDM_INT64;
    case NC_UINT64: return CDM_UINT64;
    default: return CDM_NAT;
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

boost::shared_ptr<Data> ncGetAttValues(int ncId, int varId, const std::string& attName, nc_type dt)
{
    size_t attrLen;
    ncCheck(nc_inq_attlen (ncId, varId, attName.c_str(), &attrLen));
    switch (dt) {
    case NC_BYTE:
    case NC_STRING:
    case NC_CHAR: {
        boost::shared_array<char> vals(new char[attrLen]);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_SHORT: {
        boost::shared_array<short> vals(new short[attrLen]);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_INT: {
        boost::shared_array<int> vals(new int[attrLen]);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_FLOAT: {
        boost::shared_array<float> vals(new float[attrLen]);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_DOUBLE: {
        boost::shared_array<double> vals(new double[attrLen]);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_UBYTE: {
        boost::shared_array<unsigned char> vals(new unsigned char[attrLen]);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_USHORT: {
        boost::shared_array<unsigned short> vals(new unsigned short[attrLen]);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_UINT: {
        boost::shared_array<unsigned int> vals(new unsigned int[attrLen]);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_INT64: {
        boost::shared_array<long long> vals(new long long[attrLen]);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_UINT64: {
        boost::shared_array<unsigned long long> vals(new unsigned long long[attrLen]);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_NAT:
    default: return createData(0, boost::shared_array<int>(new int[0]));
    }
}

boost::shared_ptr<Data> ncGetValues(int ncId, int varId, nc_type dt, size_t dimLen, const size_t* start, const size_t* count)
{
    size_t sliceLen;
    boost::scoped_array<size_t> mstart(new size_t[(dimLen == 0) ? 1 : dimLen]);
    boost::scoped_array<size_t> mcount(new size_t[(dimLen == 0) ? 1 : dimLen]);
    if (dimLen == 0) {
        // scalar
        sliceLen = 1;
        mstart[0] = 0;
        mcount[0] = 1;
    } else {
        sliceLen = std::accumulate(count, count + dimLen, 1, std::multiplies<size_t>());
        std::copy(&start[0], &start[0]+dimLen, &mstart[0]);
        std::copy(&count[0], &count[0]+dimLen, &mcount[0]);
    }

    switch (dt) {
    case NC_BYTE:
    case NC_STRING:
    case NC_CHAR: {
        boost::shared_array<char> vals(new char[sliceLen]);
        ncCheck(nc_get_vara(ncId, varId, &mstart[0], &mcount[0], reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_SHORT: {
        boost::shared_array<short> vals(new short[sliceLen]);
        ncCheck(nc_get_vara(ncId, varId, &mstart[0], &mcount[0], reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_INT: {
        boost::shared_array<int> vals(new int[sliceLen]);
        ncCheck(nc_get_vara(ncId, varId, &mstart[0], &mcount[0], reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_FLOAT: {
        boost::shared_array<float> vals(new float[sliceLen]);
        ncCheck(nc_get_vara(ncId, varId, &mstart[0], &mcount[0], reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_DOUBLE: {
        boost::shared_array<double> vals(new double[sliceLen]);
        ncCheck(nc_get_vara(ncId, varId, &mstart[0], &mcount[0], reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_UBYTE: {
        boost::shared_array<unsigned char> vals(new unsigned char[sliceLen]);
        ncCheck(nc_get_vara(ncId, varId, &mstart[0], &mcount[0], reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_USHORT: {
        boost::shared_array<unsigned short> vals(new unsigned short[sliceLen]);
        ncCheck(nc_get_vara(ncId, varId, &mstart[0], &mcount[0], reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_UINT: {
        boost::shared_array<unsigned int> vals(new unsigned int[sliceLen]);
        ncCheck(nc_get_vara(ncId, varId, &mstart[0], &mcount[0], reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_INT64: {
        boost::shared_array<long long> vals(new long long[sliceLen]);
        ncCheck(nc_get_vara(ncId, varId, &mstart[0], &mcount[0], reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_UINT64: {
        boost::shared_array<unsigned long long> vals(new unsigned long long[sliceLen]);
        ncCheck(nc_get_vara(ncId, varId, &mstart[0], &mcount[0], reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_NAT:
    default: return createData(0, boost::shared_array<int>(new int[0]));
    }
}

boost::shared_ptr<Data> ncValues2Data(NcValues* values, NcType dt, size_t length) {
	switch (dt) {
	case ncByte: return createData(length, boost::shared_array<char>(reinterpret_cast<char*>(values->base())));
	case ncChar: return createData(length, boost::shared_array<char>(reinterpret_cast<char*>(values->base())));
	case ncShort: return createData(length, boost::shared_array<short>(reinterpret_cast<short*>(values->base())));
	case ncInt: return createData(length, boost::shared_array<int>(reinterpret_cast<int*>(values->base())));
//	case ncLong: return CDM_INT; // ncLong is deprecated, and identical to ncInt
	case ncFloat: return createData(length, boost::shared_array<float>(reinterpret_cast<float*>(values->base())));
	case ncDouble: return createData(length, boost::shared_array<double>(reinterpret_cast<double*>(values->base())));
	case ncNoType:
	default: delete values; return createData(0, boost::shared_array<int>(new int[0]));
	}
}


}
