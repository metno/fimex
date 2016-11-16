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

#include <boost/make_shared.hpp>

namespace MetNoFimex
{

// pure abstract class, impl. required for linker
Data::~Data() {}

template<typename T>
DataPtr createDataT(size_t length, boost::shared_array<T> array)
{
    return DataPtr(new DataImpl<T>(array, length));
}

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

DataPtr createDataPtr_(CDMDataType datatype, size_t length)
{
    switch (datatype) {
        case CDM_DOUBLE: return boost::shared_ptr<DataImpl<double> >(new DataImpl<double>(length));
        case CDM_FLOAT:  return boost::shared_ptr<DataImpl<float> >(new DataImpl<float>(length));
        case CDM_INT64:  return boost::shared_ptr<DataImpl<long long> >(new DataImpl<long long>(length));
        case CDM_INT:    return boost::shared_ptr<DataImpl<int> >(new DataImpl<int>(length));
        case CDM_SHORT:  return boost::shared_ptr<DataImpl<short> >(new DataImpl<short>(length));
        case CDM_STRING:
        case CDM_CHAR:   return boost::shared_ptr<DataImpl<char> >(new DataImpl<char>(length));
        case CDM_UINT64: return boost::shared_ptr<DataImpl<unsigned long long> >(new DataImpl<unsigned long long>(length));
        case CDM_UINT:   return boost::shared_ptr<DataImpl<unsigned int> >(new DataImpl<unsigned int>(length));
        case CDM_USHORT: return boost::shared_ptr<DataImpl<unsigned short> >(new DataImpl<unsigned short>(length));
        case CDM_UCHAR:  return boost::shared_ptr<DataImpl<unsigned char> >(new DataImpl<unsigned char>(length));
        case CDM_STRINGS: return boost::make_shared< DataImpl<std::string> >(length);
        case CDM_NAT: return DataPtr(new DataImpl<char>(length));
        default: break;
    }
    throw(CDMException("cannot create dataslice of CDMDataType: " + type2string(datatype)));

}

DataPtr createData(CDMDataType datatype, size_t length, double val) {
    DataPtr data = createDataPtr_(datatype, length);
    data->setAllValues(val);
    return data;
}

DataPtr createDataSlice(CDMDataType datatype, const Data& data, size_t dataStartPos, size_t length)  {
    switch (datatype) {
        case CDM_DOUBLE: { boost::shared_ptr<DataImpl<double> > mydata(new DataImpl<double>(length)); mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_FLOAT:  { boost::shared_ptr<DataImpl<float> > mydata(new DataImpl<float>(length));   mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_INT64:    { boost::shared_ptr<DataImpl<long long> > mydata(new DataImpl<long long>(length));       mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_INT:    { boost::shared_ptr<DataImpl<int> > mydata(new DataImpl<int>(length));       mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_SHORT:  { boost::shared_ptr<DataImpl<short> > mydata(new DataImpl<short>(length));   mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_STRING:
        case CDM_CHAR:   { boost::shared_ptr<DataImpl<char> > mydata(new DataImpl<char>(length));     mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_UINT64:    { boost::shared_ptr<DataImpl<unsigned long long> > mydata(new DataImpl<unsigned long long>(length));       mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_UINT:    { boost::shared_ptr<DataImpl<unsigned int> > mydata(new DataImpl<unsigned int>(length));       mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_USHORT:  { boost::shared_ptr<DataImpl<unsigned short> > mydata(new DataImpl<unsigned short>(length));   mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_UCHAR:   { boost::shared_ptr<DataImpl<unsigned char> > mydata(new DataImpl<unsigned char>(length));     mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_STRINGS: { boost::shared_ptr<DataImpl<std::string> > mydata(new DataImpl<std::string>(length)); mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_NAT: return DataPtr(new DataImpl<char>(0));
        default: break;
    }
    throw(CDMException("cannot create dataslice of CDMDataType: " + type2string(datatype)));
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
template<>
CDMDataType DataImpl<char>::getDataType() const {return CDM_CHAR;}
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

}
