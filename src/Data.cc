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

#include "Data.h"
#include "DataImpl.h"

namespace MetNoFimex
{

// pure abstract class, impl. required for linker
Data::~Data() {}

boost::shared_ptr<Data> createData(CDMDataType datatype, size_t length) throw(CDMException) {
	int i = 0; // used as 0 InputIterator for first and last
	return createData(datatype, length, &i, &i);
}

boost::shared_ptr<Data> createDataSlice(CDMDataType datatype, const Data& data, size_t dataStartPos, size_t length) throw(CDMException)  {
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
void DataImpl<char>::setValues(size_t startPos, const Data& data, size_t first, size_t last) throw(CDMException){
	copyData(startPos, data.asConstChar(), data.size(), first, last);
}
template<>
void DataImpl<short>::setValues(size_t startPos, const Data& data, size_t first, size_t last) throw(CDMException){
	copyData(startPos, data.asConstShort(), data.size(), first, last);
}
template<>
void DataImpl<int>::setValues(size_t startPos, const Data& data, size_t first, size_t last) throw(CDMException){
	copyData(startPos, data.asConstInt(), data.size(), first, last);
}
template<>
void DataImpl<float>::setValues(size_t startPos, const Data& data, size_t first, size_t last) throw(CDMException){
	copyData(startPos, data.asConstFloat(), data.size(), first, last);
}
template<>
void DataImpl<double>::setValues(size_t startPos, const Data& data, size_t first, size_t last) throw(CDMException){
	copyData(startPos, data.asConstDouble(), data.size(), first, last);
}

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
