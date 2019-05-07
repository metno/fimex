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

#include "fimex/Data.h"

#include "DataImpl.h"
#include "StringData.h"

#include <boost/make_shared.hpp>

namespace MetNoFimex {

// pure abstract class, impl. required for linker
Data::~Data() {}

namespace {

template<typename T>
DataPtr createDataT(size_t length, boost::shared_array<T> array)
{
    typedef DataImpl<T> Impl;
    return boost::make_shared<Impl>(array, length);
}

template <typename T>
DataPtr createDataT(size_t length)
{
    typedef DataImpl<T> Impl;
    return boost::make_shared<Impl>(length);
}

DataPtr createDataPtr_(CDMDataType datatype, size_t length)
{
    // clang-format off
    switch (datatype) {
    case CDM_DOUBLE:  return createDataT<double>(length);
    case CDM_FLOAT:   return createDataT<float>(length);
    case CDM_INT64:   return createDataT<long long>(length);
    case CDM_INT:     return createDataT<int>(length);
    case CDM_SHORT:   return createDataT<short>(length);
    case CDM_STRING:  return createData(std::string(length, ' '));
    case CDM_CHAR:    return createDataT<char>(length);
    case CDM_UINT64:  return createDataT<unsigned long long>(length);
    case CDM_UINT:    return createDataT<unsigned int>(length);
    case CDM_USHORT:  return createDataT<unsigned short>(length);
    case CDM_UCHAR:   return createDataT<unsigned char>(length);
    case CDM_STRINGS: return createDataT<std::string>(length);
    case CDM_NAT:     return createDataT<char>(length);
    default: break;
    }
    // clang-format on
    throw CDMException("cannot create dataslice of CDMDataType: " + type2string(datatype));
}

} // namespace

DataPtr createData(size_t length, boost::shared_array<double> array)
{ return createDataT(length, array); }

DataPtr createData(size_t length, boost::shared_array<float> array)
{ return createDataT(length, array); }

DataPtr createData(size_t length, boost::shared_array<int> array)
{ return createDataT(length, array); }

DataPtr createData(size_t length, boost::shared_array<short> array)
{ return createDataT(length, array); }

DataPtr createData(size_t length, boost::shared_array<char> array)
{ return createDataT(length, array); }

DataPtr createData(size_t length, boost::shared_array<unsigned int> array)
{ return createDataT(length, array); }

DataPtr createData(size_t length, boost::shared_array<unsigned short> array)
{ return createDataT(length, array); }

DataPtr createData(size_t length, boost::shared_array<unsigned char> array)
{ return createDataT(length, array); }

DataPtr createData(size_t length, boost::shared_array<long long> array)
{ return createDataT(length, array); }

DataPtr createData(size_t length, boost::shared_array<unsigned long long> array)
{ return createDataT(length, array); }

DataPtr createData(size_t length, boost::shared_array<std::string> array)
{ return createDataT(length, array); }

DataPtr createData(CDMDataType datatype, size_t length, double val)
{
    DataPtr data = createDataPtr_(datatype, length);
    data->setAllValues(val);
    return data;
}

DataPtr createData(const std::string& value)
{
    return boost::make_shared<StringData>(value);
}

DataPtr createDataSlice(CDMDataType datatype, const Data& data, size_t dataStartPos, size_t length)
{
    DataPtr d;
    // clang-format off
    switch (datatype) {
    case CDM_DOUBLE:  d = createDataT<double>            (length); break;
    case CDM_FLOAT:   d = createDataT<float>             (length); break;
    case CDM_INT64:   d = createDataT<long long>         (length); break;
    case CDM_INT:     d = createDataT<int>               (length); break;
    case CDM_SHORT:   d = createDataT<short>             (length); break;
    case CDM_CHAR:    d = createDataT<char>              (length); break;
    case CDM_UINT64:  d = createDataT<unsigned long long>(length); break;
    case CDM_UINT:    d = createDataT<unsigned int>      (length); break;
    case CDM_USHORT:  d = createDataT<unsigned short>    (length); break;
    case CDM_UCHAR:   d = createDataT<unsigned char>     (length); break;
    case CDM_STRINGS: d = createDataT<std::string>       (length); break;
    case CDM_NAT:     return createDataT<char>(0);
    case CDM_STRING:
    default: throw CDMException("cannot create dataslice of CDMDataType: " + type2string(datatype));
    }
    // clang-format on
    d->setValues(0, data, dataStartPos, dataStartPos + length);
    return d;
}

template<>
void DataImpl<char>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asChar(), data.size(), first, last);
}
template<>
void DataImpl<short>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asShort(), data.size(), first, last);
}
template<>
void DataImpl<int>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asInt(), data.size(), first, last);
}
template<>
void DataImpl<unsigned char>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asUChar(), data.size(), first, last);
}
template<>
void DataImpl<unsigned short>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asUShort(), data.size(), first, last);
}
template<>
void DataImpl<unsigned int>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asUInt(), data.size(), first, last);
}
template<>
void DataImpl<long long>::setValues(size_t startPos, const Data& data, size_t first, size_t last)  {
    copyData(startPos, data.asInt64(), data.size(), first, last);
}
template<>
void DataImpl<unsigned long long>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asUInt64(), data.size(), first, last);
}
template<>
void DataImpl<float>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asFloat(), data.size(), first, last);
}
template<>
void DataImpl<double>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asDouble(), data.size(), first, last);
}
template<>
void DataImpl<std::string>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asStrings(), data.size(), first, last);
}

// specializations of getDataType
// clang-format off
template<>
CDMDataType DataImpl<char>::getDataType() const { return CDM_CHAR; } // wrong, might also be CDM_STRING
template<>
CDMDataType DataImpl<short>::getDataType() const {return CDM_SHORT;}
template<>
CDMDataType DataImpl<int>::getDataType() const {return CDM_INT;}
template<>
CDMDataType DataImpl<long long>::getDataType() const {return CDM_INT64;}
template<>
CDMDataType DataImpl<unsigned char>::getDataType() const {return CDM_UCHAR;}
template<>
CDMDataType DataImpl<unsigned short>::getDataType() const {return CDM_USHORT;}
template<>
CDMDataType DataImpl<unsigned int>::getDataType() const {return CDM_UINT;}
template<>
CDMDataType DataImpl<unsigned long long>::getDataType() const {return CDM_UINT64;}
template<>
CDMDataType DataImpl<float>::getDataType() const {return CDM_FLOAT;}
template<>
CDMDataType DataImpl<double>::getDataType() const {return CDM_DOUBLE;}
template<>
CDMDataType DataImpl<std::string>::getDataType() const {return CDM_STRINGS;}
// clang-format on
}
