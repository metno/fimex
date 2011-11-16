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
        case CDM_INT64:    { boost::shared_ptr<DataImpl<long long> > mydata(new DataImpl<long long>(length));       mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
		case CDM_INT:    { boost::shared_ptr<DataImpl<int> > mydata(new DataImpl<int>(length));       mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
		case CDM_SHORT:  { boost::shared_ptr<DataImpl<short> > mydata(new DataImpl<short>(length));   mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
		case CDM_CHAR:   { boost::shared_ptr<DataImpl<char> > mydata(new DataImpl<char>(length));     mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_UINT64:    { boost::shared_ptr<DataImpl<unsigned long long> > mydata(new DataImpl<unsigned long long>(length));       mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_UINT:    { boost::shared_ptr<DataImpl<unsigned int> > mydata(new DataImpl<unsigned int>(length));       mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_USHORT:  { boost::shared_ptr<DataImpl<unsigned short> > mydata(new DataImpl<unsigned short>(length));   mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
        case CDM_UCHAR:   { boost::shared_ptr<DataImpl<unsigned char> > mydata(new DataImpl<unsigned char>(length));     mydata->setValues(0, data, dataStartPos, dataStartPos+length); return mydata; }
		case CDM_NAT: return boost::shared_ptr<Data>(new DataImpl<char>(0));
		default: ;
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

}
