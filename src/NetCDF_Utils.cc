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
extern "C" {
#include "netcdf.h"
}
#include "../config.h"
#ifndef HAVE_NETCDF_HDF5_LIB
#undef NC_NETCDF4 /* netcdf4.1.2-4.2 define NC_NETCDF4 even when functions are not in library */
#endif

namespace MetNoFimex
{

void ncCheck(int status) {
    if (status != NC_NOERR)
        throw CDMException(nc_strerror(status));
}

Nc::~Nc()
{
   if (isOpen) {
       ncCheck(nc_close(ncId));
   }
}

nc_type cdmDataType2ncType(CDMDataType dt) {
    switch (dt) {
    case CDM_CHAR: return NC_BYTE;
    case CDM_STRING: return NC_CHAR;
    case CDM_SHORT: return NC_SHORT;
    case CDM_INT: return NC_INT;
    case CDM_FLOAT: return NC_FLOAT;
    case CDM_DOUBLE: return NC_DOUBLE;
#ifdef NC_NETCDF4
    case CDM_UCHAR: return NC_UBYTE;
    case CDM_USHORT: return NC_USHORT;
    case CDM_UINT: return NC_UINT;
    case CDM_INT64: return NC_INT64;
    case CDM_UINT64: return NC_UINT64;
#endif
    case CDM_NAT:
    default: return NC_NAT;
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
#ifdef NC_NETCDF4
    case NC_STRING: return CDM_STRING;
    case NC_UBYTE: return CDM_UCHAR;
    case NC_USHORT: return CDM_USHORT;
    case NC_UINT: return CDM_UINT;
    case NC_INT64: return CDM_INT64;
    case NC_UINT64: return CDM_UINT64;
#endif
    default: return CDM_NAT;
    }
}

boost::shared_ptr<Data> ncGetAttValues(int ncId, int varId, const std::string& attName, nc_type dt)
{
    size_t attrLen;
    ncCheck(nc_inq_attlen (ncId, varId, attName.c_str(), &attrLen));
    switch (dt) {
    case NC_BYTE:
#ifdef NC_NETCDF4
    case NC_STRING:
#endif
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
#ifdef NC_NETCDF4
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
#endif
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
#ifdef NC_NETCDF4
    case NC_STRING:
#endif
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
#ifdef NC_NETCDF4
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
#endif
    case NC_NAT:
    default: return createData(0, boost::shared_array<int>(new int[0]));
    }
}

void ncPutValues(boost::shared_ptr<Data> data, int ncId, int varId, nc_type type, size_t dimLen, const size_t* start, const size_t* count)
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
    if (sliceLen != data->size())
    {
        std::ostringstream msg;
        msg << "sliceLength (" << sliceLen << ") != dataSize (" << data->size() << ")";
        throw CDMException(msg.str());
    }

    switch (type) {
    case NC_BYTE:
#ifdef NC_NETCDF4
    case NC_STRING:
#endif
    case NC_CHAR: {
        // use general nc_put_vara here; nc_put_vara_schar will give problems with nc-internal data-checks
        // in case of real characters != bytes
        ncCheck(nc_put_vara(ncId, varId, &mstart[0], &mcount[0], data->asChar().get()));
        break;
    }
    case NC_SHORT: {
        ncCheck(nc_put_vara_short(ncId, varId, &mstart[0], &mcount[0], data->asShort().get()));
        break;
    }
    case NC_INT: {
        ncCheck(nc_put_vara_int(ncId, varId, &mstart[0], &mcount[0], data->asInt().get()));
        break;
    }
    case NC_FLOAT: {
        ncCheck(nc_put_vara_float(ncId, varId, &mstart[0], &mcount[0], data->asFloat().get()));
        break;
    }
    case NC_DOUBLE: {
        ncCheck(nc_put_vara_double(ncId, varId, &mstart[0], &mcount[0], data->asDouble().get()));
        break;
    }
#if NC_NETCDF4
    case NC_UBYTE: {
        ncCheck(nc_put_vara_uchar(ncId, varId, &mstart[0], &mcount[0], data->asUChar().get()));
        break;
    }
    case NC_USHORT: {
        ncCheck(nc_put_vara_ushort(ncId, varId, &mstart[0], &mcount[0], data->asUShort().get()));
        break;
    }
    case NC_UINT: {
        ncCheck(nc_put_vara_uint(ncId, varId, &mstart[0], &mcount[0], data->asUInt().get()));
        break;
    }
    case NC_INT64: {
        ncCheck(nc_put_vara_longlong(ncId, varId, &mstart[0], &mcount[0], data->asInt64().get()));
        break;
    }
    case NC_UINT64: {
        ncCheck(nc_put_vara_ulonglong(ncId, varId, &mstart[0], &mcount[0], data->asUInt64().get()));
        break;
    }
#endif
    case NC_NAT:
    default: break;
    }

}


}
