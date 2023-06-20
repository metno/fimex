/*
 * Fimex
 *
 * (C) Copyright 2008-2023, met.no
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

#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/MutexLock.h"

#include <functional>
#include <numeric>

extern "C" {
#include "netcdf.h"
}
#include "fimex_netcdf_config.h"
#ifndef HAVE_NETCDF_HDF5_LIB
#undef NC_NETCDF4 /* netcdf4.1.2-4.2 define NC_NETCDF4 even when functions are not in library */
#endif

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.NetCDF_Utils");
// hdf5 lib is usually not thread-safe, so reading from one file and writing to another fails
static OmpMutex ncMutex;

namespace {

DataPtr createDataStringsFromNc(shared_array<char*> cvals, size_t len)
{
    auto vals = make_shared_array<std::string>(len);
    for (size_t i = 0; i < len; ++i) {
        vals[i] = cvals[i];
    }
    nc_free_string(len, &cvals[0]);
    return createData(len, vals);
}

} // namespace

void ncCheck(int status)
{
    if (status != NC_NOERR) {
        throw CDMException(nc_strerror(status));
    }
}

void ncCheck(int status, const std::string& msg)
{
    if (status != NC_NOERR) {
        if (msg.empty()) {
            throw CDMException(nc_strerror(status));
        } else {
            std::ostringstream out;
            out << nc_strerror(status);
            out << " : " << msg;
            throw CDMException(out.str());
        }
    }
}

Nc::Nc()
    : isOpen(false)
    , pid(getpid())
{
}

Nc::~Nc()
{
    if (isOpen) {
        int status;
        NCMUTEX_LOCKED(status = nc_close(ncId));
        try {
            if (status != NC_NOERR) {
                LOG4FIMEX(logger, Logger::ERROR, "error while closing NetCDF file '" << filename << "': " << nc_strerror(status));
            } else {
                LOG4FIMEX(logger, Logger::DEBUG, "close NetCDF file '" << filename << "'");
            }
        } catch (...) {
            // ignore exceptions from logging
        }
    }
}

void Nc::reopen_if_forked()
{
    pid_t this_pid = getpid();
    if (pid != this_pid) {
        pid = this_pid;
        LOG4FIMEX(logger, Logger::DEBUG, "reopening file " << filename << " after fork to " << pid << " '" << ncId << "' ");

        OmpScopedLock lock(ncMutex);
        // reopen file so file descriptions (e.g. offset) are not shared
        ncCheck(nc_close(ncId), "closing parent filehandle");
        ncCheck(nc_open(filename.c_str(), NC_NOWRITE, &ncId), "re-opening '" + filename + "' after fork");
    }
}

OmpMutex& Nc::getMutex()
{
    return ncMutex;
}

nc_type cdmDataType2ncType(CDMDataType dt)
{
    switch (dt) {
    case CDM_CHAR:
        return NC_BYTE;
    case CDM_STRING:
        return NC_CHAR;
    case CDM_SHORT:
        return NC_SHORT;
    case CDM_INT:
        return NC_INT;
    case CDM_FLOAT:
        return NC_FLOAT;
    case CDM_DOUBLE:
        return NC_DOUBLE;
#ifdef NC_NETCDF4
    case CDM_STRINGS:
        return NC_STRING;
    case CDM_UCHAR:
        return NC_UBYTE;
    case CDM_USHORT:
        return NC_USHORT;
    case CDM_UINT:
        return NC_UINT;
    case CDM_INT64:
        return NC_INT64;
    case CDM_UINT64:
        return NC_UINT64;
#endif
    case CDM_NAT:
    default:
        return NC_NAT;
    }
}

CDMDataType ncType2cdmDataType(nc_type dt)
{
    switch (dt) {
    case NC_NAT:
        return CDM_NAT;
    case NC_BYTE:
        return CDM_CHAR;
    case NC_CHAR:
        return CDM_STRING;
    case NC_SHORT:
        return CDM_SHORT;
    case NC_INT:
        return CDM_INT;
    case NC_FLOAT:
        return CDM_FLOAT;
    case NC_DOUBLE:
        return CDM_DOUBLE;
#ifdef NC_NETCDF4
    case NC_STRING:
        return CDM_STRINGS;
    case NC_UBYTE:
        return CDM_UCHAR;
    case NC_USHORT:
        return CDM_USHORT;
    case NC_UINT:
        return CDM_UINT;
    case NC_INT64:
        return CDM_INT64;
    case NC_UINT64:
        return CDM_UINT64;
#endif
    default:
        return CDM_NAT;
    }
}

DataPtr ncGetAttValues(int ncId, int varId, const std::string& attName, nc_type dt)
{
    size_t attrLen;
    ncCheck(nc_inq_attlen(ncId, varId, attName.c_str(), &attrLen));
    switch (dt) {
    case NC_BYTE: {
        auto vals = make_shared_array<char>(attrLen);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_CHAR: {
        std::string vals(attrLen, '\0');
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        if (attrLen > 0 && vals[attrLen - 1] == 0) {
            // remove terminating 0 character
            vals.erase(vals.size() - 1);
        }
        return createData(vals);
    }
    case NC_SHORT: {
        auto vals = make_shared_array<short>(attrLen);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_INT: {
        auto vals = make_shared_array<int>(attrLen);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_FLOAT: {
        auto vals = make_shared_array<float>(attrLen);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_DOUBLE: {
        auto vals = make_shared_array<double>(attrLen);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
#ifdef NC_NETCDF4
    case NC_UBYTE: {
        auto vals = make_shared_array<unsigned char>(attrLen);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_USHORT: {
        auto vals = make_shared_array<unsigned short>(attrLen);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_UINT: {
        auto vals = make_shared_array<unsigned int>(attrLen);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_INT64: {
        auto vals = make_shared_array<long long>(attrLen);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_UINT64: {
        auto vals = make_shared_array<unsigned long long>(attrLen);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&vals[0])));
        return createData(attrLen, vals);
    }
    case NC_STRING: {
        auto cvals = make_shared_array<char*>(attrLen);
        ncCheck(nc_get_att(ncId, varId, attName.c_str(), reinterpret_cast<void*>(&cvals[0])));
        return createDataStringsFromNc(cvals, attrLen);
    }
#endif
    case NC_NAT:
    default:
        return createData(0, make_shared_array<int>(0));
    }
}

static const size_t zero = 0, one = 1;

DataPtr ncGetValues(int ncId, int varId, nc_type dt, size_t dimLen, const size_t* start, const size_t* count)
{
    size_t sliceLen;
    if (dimLen == 0) {
        // scalar
        sliceLen = 1;
        start = &zero;
        count = &one;
    } else {
        sliceLen = product(count, dimLen);
    }

    switch (dt) {
    case NC_CHAR: {
        std::string vals(sliceLen, '\0');
        ncCheck(nc_get_vara(ncId, varId, start, count, reinterpret_cast<void*>(&vals[0])));
        return createData(vals);
    }
    case NC_BYTE: {
        auto vals = make_shared_array<char>(sliceLen);
        ncCheck(nc_get_vara(ncId, varId, start, count, reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_SHORT: {
        auto vals = make_shared_array<short>(sliceLen);
        ncCheck(nc_get_vara(ncId, varId, start, count, reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_INT: {
        auto vals = make_shared_array<int>(sliceLen);
        ncCheck(nc_get_vara(ncId, varId, start, count, reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_FLOAT: {
        auto vals = make_shared_array<float>(sliceLen);
        ncCheck(nc_get_vara(ncId, varId, start, count, reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_DOUBLE: {
        auto vals = make_shared_array<double>(sliceLen);
        ncCheck(nc_get_vara(ncId, varId, start, count, reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
#ifdef NC_NETCDF4
    case NC_UBYTE: {
        auto vals = make_shared_array<unsigned char>(sliceLen);
        ncCheck(nc_get_vara(ncId, varId, start, count, reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_USHORT: {
        auto vals = make_shared_array<unsigned short>(sliceLen);
        ncCheck(nc_get_vara(ncId, varId, start, count, reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_UINT: {
        auto vals = make_shared_array<unsigned int>(sliceLen);
        ncCheck(nc_get_vara(ncId, varId, start, count, reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_INT64: {
        auto vals = make_shared_array<long long>(sliceLen);
        ncCheck(nc_get_vara(ncId, varId, start, count, reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_UINT64: {
        auto vals = make_shared_array<unsigned long long>(sliceLen);
        ncCheck(nc_get_vara(ncId, varId, start, count, reinterpret_cast<void*>(&vals[0])));
        return createData(sliceLen, vals);
    }
    case NC_STRING: {
        // this will fail if NetCDF file does not support NC_STRING, e.g. for NetCDF 3 or NetCDF 4 classic
        auto cvals = make_shared_array<char*>(sliceLen);
        ncCheck(nc_get_vara(ncId, varId, start, count, reinterpret_cast<void*>(&cvals[0])));
        return createDataStringsFromNc(cvals, sliceLen);
    }
#endif
    case NC_NAT:
    default:
        return createData(0, make_shared_array<int>(0));
    }
}

void ncPutValues(DataPtr data, int ncId, int varId, nc_type type, size_t dimLen, const size_t* start, const size_t* count)
{
    if (data->size() == 0)
        return;

    size_t sliceLen;
    if (dimLen == 0) {
        // scalar
        sliceLen = 1;
        start = &zero;
        count = &one;
    } else {
        sliceLen = product(count, dimLen);
    }
    if (sliceLen != data->size()) {
        std::ostringstream msg;
        msg << "sliceLength (" << sliceLen << ") != dataSize (" << data->size() << ")";
        throw CDMException(msg.str());
    }

    switch (type) {
    case NC_CHAR: {
        // use general nc_put_vara here; nc_put_vara_schar will give problems with nc-internal data-checks
        // in case of real characters != bytes
        const std::string text = data->asString();
        ncCheck(nc_put_vara(ncId, varId, start, count, text.c_str()));
        break;
    }
    case NC_BYTE: {
        // use general nc_put_vara here; nc_put_vara_schar will give problems with nc-internal data-checks
        // in case of real characters != bytes
        ncCheck(nc_put_vara(ncId, varId, start, count, data->asChar().get()));
        break;
    }
    case NC_SHORT: {
        ncCheck(nc_put_vara_short(ncId, varId, start, count, data->asShort().get()));
        break;
    }
    case NC_INT: {
        ncCheck(nc_put_vara_int(ncId, varId, start, count, data->asInt().get()));
        break;
    }
    case NC_FLOAT: {
        ncCheck(nc_put_vara_float(ncId, varId, start, count, data->asFloat().get()));
        break;
    }
    case NC_DOUBLE: {
        ncCheck(nc_put_vara_double(ncId, varId, start, count, data->asDouble().get()));
        break;
    }
#if NC_NETCDF4
    case NC_STRING: {
        const size_t dataLen = data->size();
        auto svals = data->asStrings();
        auto cvals = make_shared_array<const char*>(dataLen);
        for (size_t i = 0; i < dataLen; ++i)
            cvals[i] = svals[i].c_str();
        ncCheck(nc_put_vara(ncId, varId, start, count, &cvals[0]));
        break;
    }
    case NC_UBYTE: {
        ncCheck(nc_put_vara_uchar(ncId, varId, start, count, data->asUChar().get()));
        break;
    }
    case NC_USHORT: {
        ncCheck(nc_put_vara_ushort(ncId, varId, start, count, data->asUShort().get()));
        break;
    }
    case NC_UINT: {
        ncCheck(nc_put_vara_uint(ncId, varId, start, count, data->asUInt().get()));
        break;
    }
    case NC_INT64: {
        ncCheck(nc_put_vara_longlong(ncId, varId, start, count, data->asInt64().get()));
        break;
    }
    case NC_UINT64: {
        ncCheck(nc_put_vara_ulonglong(ncId, varId, start, count, data->asUInt64().get()));
        break;
    }
#endif
    case NC_NAT:
    default:
        break;
    }
}

} // namespace MetNoFimex
