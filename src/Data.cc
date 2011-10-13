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

namespace MetNoFimex
{

// pure abstract class, impl. required for linker
Data::~Data() {}

boost::shared_ptr<Data> createData(CDMDataType datatype, size_t length, double val) {
	std::vector<char> v(length);
	boost::shared_ptr<Data> data = createData(datatype, v.begin(), v.end());
	data->setAllValues(val);
	return data;
}

template<typename T>
boost::shared_ptr<Data> createDataT(size_t length, boost::shared_array<T> array)
{
    return boost::shared_ptr<Data>(new DataImpl<T>(array, length));
}

boost::shared_ptr<Data> createData(size_t length, boost::shared_array<double> array)
{ return createDataT(length, array); }

boost::shared_ptr<Data> createData(size_t length, boost::shared_array<float> array)
{ return createDataT(length, array); }

boost::shared_ptr<Data> createData(size_t length, boost::shared_array<int> array)
{ return createDataT(length, array); }

boost::shared_ptr<Data> createData(size_t length, boost::shared_array<short> array)
{ return createDataT(length, array); }

boost::shared_ptr<Data> createData(size_t length, boost::shared_array<char> array)
{ return createDataT(length, array); }

boost::shared_ptr<Data> createData(size_t length, boost::shared_array<unsigned int> array)
{ return createDataT(length, array); }

boost::shared_ptr<Data> createData(size_t length, boost::shared_array<unsigned short> array)
{ return createDataT(length, array); }

boost::shared_ptr<Data> createData(size_t length, boost::shared_array<unsigned char> array)
{ return createDataT(length, array); }

boost::shared_ptr<Data> createData(size_t length, boost::shared_array<long long> array)
{ return createDataT(length, array); }

boost::shared_ptr<Data> createData(size_t length, boost::shared_array<unsigned long long> array)
{ return createDataT(length, array); }


boost::shared_ptr<Data> createDataSlice(CDMDataType datatype, const Data& data, size_t dataStartPos, size_t length)  {
	switch (datatype) {
		case CDM_DOUBLE: { boost::shared_ptr<DataImpl<double> > mydata(new DataImpl<double>(length)); mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
		case CDM_FLOAT:  { boost::shared_ptr<DataImpl<float> > mydata(new DataImpl<float>(length));   mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
		case CDM_INT:    { boost::shared_ptr<DataImpl<int> > mydata(new DataImpl<int>(length));       mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
		case CDM_SHORT:  { boost::shared_ptr<DataImpl<short> > mydata(new DataImpl<short>(length));   mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
		case CDM_CHAR:   { boost::shared_ptr<DataImpl<char> > mydata(new DataImpl<char>(length));     mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
		case CDM_NAT: return boost::shared_ptr<Data>(new DataImpl<char>(0));
		default: ;
	}
	throw(CDMException("cannot create dataslice of CDMDataType: " + type2string(datatype)));
}
template<>
void DataImpl<char>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
	copyData(startPos, data.asConstChar(), data.size(), first, last);
}
template<>
void DataImpl<short>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
	copyData(startPos, data.asConstShort(), data.size(), first, last);
}
template<>
void DataImpl<int>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
	copyData(startPos, data.asConstInt(), data.size(), first, last);
}
template<>
void DataImpl<unsigned char>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asConstUChar(), data.size(), first, last);
}
template<>
void DataImpl<unsigned short>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asConstUShort(), data.size(), first, last);
}
template<>
void DataImpl<unsigned int>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asConstUInt(), data.size(), first, last);
}
template<>
void DataImpl<long long>::setValues(size_t startPos, const Data& data, size_t first, size_t last)  {
    copyData(startPos, data.asConstInt64(), data.size(), first, last);
}
template<>
void DataImpl<unsigned long long>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
    copyData(startPos, data.asConstUInt64(), data.size(), first, last);
}
template<>
void DataImpl<float>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
	copyData(startPos, data.asConstFloat(), data.size(), first, last);
}
template<>
void DataImpl<double>::setValues(size_t startPos, const Data& data, size_t first, size_t last) {
	copyData(startPos, data.asConstDouble(), data.size(), first, last);
}

// specializations of getDataType
template<>
CDMDataType DataImpl<char>::getDataType() const {return CDM_CHAR;}
template<>
CDMDataType DataImpl<short>::getDataType() const {return CDM_SHORT;}
template<>
CDMDataType DataImpl<int>::getDataType() const {return CDM_INT;}
template<>
CDMDataType DataImpl<float>::getDataType() const {return CDM_FLOAT;}
template<>
CDMDataType DataImpl<double>::getDataType() const {return CDM_DOUBLE;}

// specializations of constConvertArrayType
template<>
const boost::shared_array<char> constConvertArrayType(const boost::shared_array<char>& inData, long length) {return inData;}
template<>
const boost::shared_array<short> constConvertArrayType(const boost::shared_array<short>& inData, long length) {return inData;}
template<>
const boost::shared_array<int> constConvertArrayType(const boost::shared_array<int>& inData, long length) {return inData;}
template<>
const boost::shared_array<float> constConvertArrayType(const boost::shared_array<float>& inData, long length) {return inData;}
template<>
const boost::shared_array<double> constConvertArrayType(const boost::shared_array<double>& inData, long length) {return inData;}



}
