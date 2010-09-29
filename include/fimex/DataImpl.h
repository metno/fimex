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

#ifndef DATAIMPL_H_
#define DATAIMPL_H_

#include <typeinfo>
#include <boost/shared_ptr.hpp>
#include <string>
#include <sstream>
#include <iostream>
#include <iterator>
#include <cmath>
#include <algorithm>
#include "fimex/Data.h"
#include "fimex/CDMDataType.h"
#include "fimex/CDMException.h"
#include "fimex/Utils.h"

namespace MetNoFimex
{

    /**
     * @headerfile "fimex/DataImpl.h"
     */

	/**
	 * @brief create a new shared array with a different type using static_cast
	 * 
	 * @param inData original data
	 * @param length length of original data array
	 * 
	 */
	template<typename T1, typename T2>
	boost::shared_array<T1> duplicateArrayType(const boost::shared_array<T2>& inData, long length);
	/**
	 * @brief return a shared array of this data, possibly pointer to internal data
	 * 
	 * @param inData original data
	 * @param length length of original data array
	 * 
	 */
	template<typename T1, typename T2>
	const boost::shared_array<T1> constConvertArrayType(const boost::shared_array<T2>& inData, long length);
		

	
	template<typename C>
	class DataImpl : public Data
	{
	public:
		/// constructor where the array will be automatically allocated
		explicit DataImpl(long length)
		: length(length), theData(new C[length]) {}
		explicit DataImpl(boost::shared_array<C> array, long length)
		: length(length), theData(array) {}
		virtual ~DataImpl() {}

		virtual size_t size() const {return length;}
		virtual int bytes_for_one() const {return sizeof(C);}
		virtual void* getDataPtr() {return &theData[0];}
		virtual void toStream(std::ostream& os, std::string separator = "") const;

		/**
		 *  @brief get the datapointer of the data
		 */
		virtual const boost::shared_array<C> asBase() const {return theData;}
		/**
		 * general conversion function, not in base since template methods not allowed
		 */
		template<typename T>
		const boost::shared_array<T> as() const {return constConvertArrayType<T, C>(theData, length);}
		template<typename T>
		boost::shared_array<T> as() {return duplicateArrayType<T, C>(theData, length);}
		// conversion function
		const virtual boost::shared_array<char> asConstChar() const {return as<char>();}
		virtual boost::shared_array<char> asChar() {return as<char>();}
		const virtual boost::shared_array<short> asConstShort() const {return as<short>();}
		virtual boost::shared_array<short> asShort() {return as<short>();}
		const virtual boost::shared_array<int> asConstInt() const {return as<int>();}
		virtual boost::shared_array<int> asInt() {return as<int>();}
		const virtual boost::shared_array<float> asConstFloat() const {return as<float>();}
		virtual boost::shared_array<float> asFloat() {return as<float>();}
		const virtual boost::shared_array<double> asConstDouble() const {return as<double>();}
		virtual boost::shared_array<double> asDouble() {return as<double>();}
		virtual std::string asString(std::string separator = "") const;

		
		virtual void setValue(long pos, double val) {theData[pos] = static_cast<C>(val);}
		virtual void setValues(size_t startPos, const Data& data, size_t first = 0, size_t last = -1) throw(CDMException);
		virtual void setAllValues(double val) {C v = static_cast<C>(val); for (C* pos = &theData[0]; pos != (&theData[0])+length; ++pos) *pos = v;}
		virtual boost::shared_ptr<Data> slice(std::vector<size_t> orgDimSize, std::vector<size_t> startDims, std::vector<size_t> outputDimSize) throw(CDMException);
		virtual boost::shared_ptr<Data> convertDataType(double oldFill, double oldScale, double oldOffset, CDMDataType newType, double newFill, double newScale, double newOffset) throw(CDMException);
		// specialized for each known type in Data.cc
		virtual CDMDataType getDataType() const {return CDM_NAT;}

		/**
		 * set the values of the data by the input-iterator
		 */
		template<class InputIterator>
		void setValues(InputIterator begin, InputIterator end, size_t dataStartPos = 0) throw(CDMException);
		
	private:
		size_t length;
		boost::shared_array<C> theData;
		void copyData(size_t startPos, const boost::shared_array<C>& otherData, size_t otherSize, size_t otherStart, size_t otherEnd) throw(CDMException);
	};
	
	/**
	 * @brief create a Data-pointer of the datatype and fill with the data from the iterator
	 * 
	 * @param datatype
	 * @param first start of container containing the data to fill the array with
	 * @param last end (excluded) of the container containing the data to fill the array with
	 * @return Base-Class ptr of the DataImpl belonging to the datatype 
	 */
	template<class InputIterator>
	boost::shared_ptr<Data> createData(CDMDataType datatype, InputIterator first, InputIterator last) throw(CDMException);
	
	// below follow implementations of templates
	// (template definitions should be in header files (depending on compiler))
	template<typename C>
	void DataImpl<C>::toStream(std::ostream& os, std::string separator) const {
		for (size_t i = 0; i < (length-1); i++) {
			os << theData[i] << separator;
		}
		os << theData[length-1];
	}

	template<typename C>
	std::string DataImpl<C>::asString(std::string separator) const {
		std::ostringstream o;
		toStream(o, separator);
		return o.str();
	}

	template<typename C>
	void DataImpl<C>::copyData(size_t startPos, const boost::shared_array<C>& otherData, size_t otherSize, size_t otherFirst, size_t otherLast) throw(CDMException) {
		if (otherFirst > otherSize) {
			throw(CDMException("data-region-start "+ type2string(otherFirst) + " outside range: "+ type2string(otherSize)));
		}
		// fixing max range
		otherLast = std::min(otherLast, otherSize);
		otherLast = std::min(size()-startPos+otherFirst, otherLast);
		if (otherLast > otherFirst) {
			std::copy(&otherData[otherFirst], &otherData[otherLast], &theData[startPos]);
		}
	}
// don't implement all possible setValues functions, link-time error
//	template<typename C>
//	void DataImpl<C>::setValues(size_t startPos, const Data& data, size_t first, size_t last) throw(CDMException){
//		throw(CDMException("setValues not implemented for this datatype"));
//	}

	// declaration of implemented function (in Data.cc)
	template<>
	void DataImpl<char>::setValues(size_t startPos, const Data& data, size_t first, size_t last) throw(CDMException);
	template<>
	void DataImpl<short>::setValues(size_t startPos, const Data& data, size_t first, size_t last) throw(CDMException);
	template<>
	void DataImpl<int>::setValues(size_t startPos, const Data& data, size_t first, size_t last) throw(CDMException);
	template<>
	void DataImpl<float>::setValues(size_t startPos, const Data& data, size_t first, size_t last) throw(CDMException);
	template<>
	void DataImpl<double>::setValues(size_t startPos, const Data& data, size_t first, size_t last) throw(CDMException);
	
	/**
	 * recursively copy data by moving the newData and orgData pointers forward and copy the data at the current position
	 * 
	 * it's assumed that the first dim in the vector is the fastest moving (fortran like)
	 * 
	 * @param orgData pointer to the current postion of the original array
	 * @param newData pointer to the current position of the new array
	 * @orgDimSize the original dimensions of orgData
	 * @orgSliceSize helper-array with orgSliceSize[0] = 1; orgSliceSize[n] = orgDimSize[n] * orgSliceSize[n-1]
	 * @newStart the start positions in the new data
	 * @newSize the dimensions of the newData
	 * @currentDim the dimension currently under work, should be between (orgData.size()-1) and 0
	 * 
	 */
	template<typename C>
	void recursiveCopyMultiDimData(C** orgData, C** newData, const std::vector<size_t>& orgDimSize, const std::vector<size_t>& orgSliceSize, const std::vector<size_t>& newStart, const std::vector<size_t>& newSize, size_t currentDim) {
		(*orgData) += newStart[currentDim] * orgSliceSize[currentDim];
		if (currentDim == 0) {
		    *newData = std::copy(&(*orgData)[0], &(*orgData)[newSize[0]], *newData);
		    (*orgData) += newSize[0]; // forward orgData pointer
		} else {
			for (size_t i = 0; i < newSize[currentDim]; i++) {
				recursiveCopyMultiDimData(orgData, newData, orgDimSize, orgSliceSize, newStart, newSize, currentDim - 1);
			}
		}
		(*orgData) += (orgDimSize[currentDim] - (newStart[currentDim] + newSize[currentDim])) * orgSliceSize[currentDim];
	}
	
	template<typename C>
	boost::shared_ptr<Data> DataImpl<C>::slice(std::vector<size_t> orgDimSize, std::vector<size_t> startDims, std::vector<size_t> outputDimSize) throw(CDMException) {
	    // handle empty data
	    if (orgDimSize.size() == 0 || orgDimSize.size() == 0) {
	        return boost::shared_ptr<DataImpl<C> >(new DataImpl<C>(0));
	    }
		// get the sizes of the original data and the output data
		size_t orgSize = 1;
		size_t outputSize = 1;
		for (size_t i = 0; i < orgDimSize.size(); ++i) {
			outputSize *= outputDimSize[i];
			orgSize *= orgDimSize[i];
			if (orgDimSize[i] < (startDims[i] + outputDimSize[i])) throw CDMException("dimension-size error, start+size > orgSize: " + type2string(startDims[i]+outputDimSize[i]) + ">" + type2string(orgDimSize[i]) );
		}
		if (orgSize != size()) throw CDMException("dimension-mismatch: " + type2string(size()) + "!=" + type2string(orgSize));
		
		// get the old and new datacontainer
		boost::shared_ptr<DataImpl<C> > output(new DataImpl<C>(outputSize));
		C* newData = output->theData.get();
		C* oldData = theData.get();

		// pre-calculation of the slice-size of the different dimensions
		std::vector<size_t> orgSliceSize(orgDimSize.size(), 0);
		orgSliceSize[0] = 1;
		for (size_t dim = 1; dim < orgDimSize.size(); dim++) {
			orgSliceSize[dim] = orgSliceSize[dim-1] * orgDimSize[dim-1];
		}
		// slice the data
		recursiveCopyMultiDimData(&oldData, &newData, orgDimSize, orgSliceSize, startDims, outputDimSize, orgDimSize.size() - 1);
		
		return output;
	}

	template<typename C>
	template<class InputIterator>
	void DataImpl<C>::setValues(InputIterator begin, InputIterator end, size_t dataStartPos) throw(CDMException) {
	    size_t dist = std::distance(begin, end);
	    if ((dist + dataStartPos) > length)
	        throw CDMException("dataPos " + type2string(dist+dataStartPos) + " >= dataLength " + type2string(length));
	    std::transform(begin, end, &theData[dataStartPos], staticCast<C>());
	}

	template<typename OUT, typename IN>
	boost::shared_array<OUT> convertArrayType(const boost::shared_array<IN>& inData, size_t length, double oldFill, double oldScale, double oldOffset, double newFill, double newScale, double newOffset) {
		boost::shared_array<OUT> outData(new OUT[length]);
		std::transform(&inData[0], &inData[length], &outData[0], ScaleValue<IN, OUT>(oldFill, oldScale, oldOffset, newFill, newScale, newOffset));
		return outData;
	}

	template<typename C>
	boost::shared_ptr<Data> DataImpl<C>::convertDataType(double oldFill, double oldScale, double oldOffset, CDMDataType newType, double newFill, double newScale, double newOffset) throw(CDMException) 
	{
		boost::shared_ptr<Data> data(new DataImpl<char>(0)); // dummy default
		switch (newType) {
		case CDM_CHAR: data = boost::shared_ptr<Data>(new DataImpl<char>(convertArrayType<char>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
		case CDM_SHORT: data = boost::shared_ptr<Data>(new DataImpl<short>(convertArrayType<short>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
		case CDM_INT: data = boost::shared_ptr<Data>(new DataImpl<int>(convertArrayType<int>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
		case CDM_FLOAT: data = boost::shared_ptr<Data>(new DataImpl<float>(convertArrayType<float>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
		case CDM_DOUBLE: data = boost::shared_ptr<Data>(new DataImpl<double>(convertArrayType<double>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
		case CDM_STRING: throw CDMException("cannot convert string datatype"); break;
		case CDM_NAT: throw CDMException("cannot convert CDM_NAT datatype"); break;
		}
		return data;
	}

	
	template<typename T1, typename T2>
	boost::shared_array<T1> duplicateArrayType(const boost::shared_array<T2>& inData, long length) {
		boost::shared_array<T1> outData(new T1[length]);
		std::transform(&inData[0], &inData[length], &outData[0], staticCast<T2>());
		return outData;
	}
	// this version is identical to duplicateArrayType one, except that it allows for specializations
	// in the case of T1 == T2 (see Data.cc)
	template<typename T1, typename T2>
	const boost::shared_array<T1> constConvertArrayType(const boost::shared_array<T2>& inData, long length) {
		return duplicateArrayType<T1,T2>(inData, length);
	}

	template<class InputIterator>
	boost::shared_ptr<Data> createData(CDMDataType datatype, InputIterator first, InputIterator last) throw(CDMException) {
	    size_t length = std::distance(first, last);
		switch (datatype) {
			case CDM_DOUBLE: { boost::shared_ptr<DataImpl<double> > data(new DataImpl<double>(length)); data->setValues(first, last); return data; }  
			case CDM_FLOAT:  { boost::shared_ptr<DataImpl<float> > data(new DataImpl<float>(length));   data->setValues(first, last); return data; }
			case CDM_INT:    { boost::shared_ptr<DataImpl<int> > data(new DataImpl<int>(length));       data->setValues(first, last); return data; }
			case CDM_SHORT:  { boost::shared_ptr<DataImpl<short> > data(new DataImpl<short>(length));   data->setValues(first, last); return data; }
			case CDM_CHAR:   { boost::shared_ptr<DataImpl<char> > data(new DataImpl<char>(length));     data->setValues(first, last); return data; }
			case CDM_NAT: ;
			default: ;
		}
		return boost::shared_ptr<Data>(new DataImpl<char>(0)); // a dummy dataset

	}

}

#endif /*DATAIMPL_H_*/
