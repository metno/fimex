#ifndef DATAIMPL_H_
#define DATAIMPL_H_

#include <boost/shared_ptr.hpp>
#include <string>
#include <sstream>
#include <iostream>
#include "Data.h"
#include "CDMDataType.h"
#include "CDMException.h"
#include "Utils.h"

namespace MetNoUtplukk
{

	/**
	 * @brief create a new shared array with a different type using static_cast
	 * 
	 * @param inData original data
	 * @param length length of original data array
	 * 
	 */
	template<typename T1, typename T2>
	boost::shared_array<T1> convertArrayType(const boost::shared_array<T2>& inData, long length);

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
		virtual void toStream(std::ostream& os, std::string separator = "") const;

		/**
		 *  @brief get the datapointer of the data
		 */
		virtual const boost::shared_array<C> asBase() const {return theData;}
		// conversion function
		virtual boost::shared_array<char> asChar() const {return convertArrayType<char, C>(theData, length);}
		virtual boost::shared_array<short> asShort() const {return convertArrayType<short, C>(theData, length);}
		virtual boost::shared_array<int> asInt() const {return convertArrayType<int, C>(theData, length);}
		virtual boost::shared_array<float> asFloat() const {return convertArrayType<float, C>(theData, length);}
		virtual boost::shared_array<double> asDouble() const {return convertArrayType<double, C>(theData, length);}
		virtual std::string asString(std::string separator = "") const;

		virtual void setValue(long pos, double val) {theData[pos] = static_cast<C>(val);}
		virtual void setValues(size_t startPos, const Data& data, size_t first = 0, size_t last = -1) throw(CDMException);
		/**
		 * set the values of the data by the input-iterator
		 */
		template<class InputIterator>
		void setValues(InputIterator first, InputIterator last, size_t dataStartPos = 0) throw(CDMException);
		virtual boost::shared_ptr<Data> slice(std::vector<size_t> orgDimSize, std::vector<size_t> startDims, std::vector<size_t> outputDimSize) throw(CDMException);
		
	private:
		size_t length;
		boost::shared_array<C> theData;
		void copyData(size_t startPos, boost::shared_array<C> otherData, size_t otherSize, size_t otherStart, size_t otherEnd) throw(CDMException);
	};
	
	/**
	 * @brief create a Data-pointer of the datatype and fill with the data from the iterator
	 * 
	 * @param datatype
	 * @param size_t length of the data array
	 * @param first start of container containing the data to fill the array with
	 * @param last end (excluded) of the container containing the data to fill the array with
	 * @return Base-Class ptr of the DataImpl belonging to the datatype 
	 */
	template<class InputIterator>
	boost::shared_ptr<Data> createData(CDMDataType datatype, size_t length, InputIterator first, InputIterator last) throw(CDMException);
	
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
	void DataImpl<C>::copyData(size_t startPos, boost::shared_array<C> otherData, size_t otherSize, size_t otherFirst, size_t otherLast) throw(CDMException) {
		if (otherFirst > otherSize) {
			throw(CDMException("data-region-start "+ type2string(otherFirst) + " outside range: "+ type2string(otherSize)));
		}
		// fixing max range
		otherLast = std::min(otherLast, otherSize);
		otherLast = std::min(size()-startPos+otherFirst, otherLast);
		if (otherLast > otherFirst) {
			std::copy(otherData.get()+otherFirst, otherData.get()+otherLast, theData.get()+startPos);
		}
	}
	template<typename C>
	void DataImpl<C>::setValues(size_t startPos, const Data& data, size_t first, size_t last) throw(CDMException){
		throw(CDMException("setValues not implemented for this datatype"));
	}
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
			// putting loop inside if/else for performance
			for (size_t i = 0; i < newSize[0]; i++) {
				*(*newData)++ = *(*orgData)++;
			}
		} else {
			for (size_t i = 0; i < newSize[currentDim]; i++) {
				recursiveCopyMultiDimData(orgData, newData, orgDimSize, orgSliceSize, newStart, newSize, currentDim - 1);
			}
		}
		(*orgData) += (orgDimSize[currentDim] - (newStart[currentDim] + newSize[currentDim])) * orgSliceSize[currentDim];
	}
	
	template<typename C>
	boost::shared_ptr<Data> DataImpl<C>::slice(std::vector<size_t> orgDimSize, std::vector<size_t> startDims, std::vector<size_t> outputDimSize) throw(CDMException) {
		size_t orgSize = 1;
		size_t outputSize = 1;
		for (size_t i = 0; i < orgDimSize.size(); ++i) {
			outputSize *= outputDimSize[i];
			orgSize *= orgDimSize[i];
			if (orgDimSize[i] < (startDims[i] + outputDimSize[i])) throw CDMException("dimension-size error, start+size > orgSize: " + type2string(startDims[i]+outputDimSize[i]) + ">" + type2string(orgDimSize[i]) );
		}
		if (orgSize != size()) throw CDMException("dimension-mismatch: " + type2string(size()) + "!=" + type2string(orgSize));
		
		
		boost::shared_ptr<DataImpl<C> > output(new DataImpl<C>(outputSize));
		C* newData = output->theData.get();
		C* oldData = theData.get();
		// pre-calculation of the slice-size of the different dimensions
		std::vector<size_t> orgSliceSize; 
		orgSliceSize.reserve(orgDimSize.size());
		orgSliceSize[0] = 1;
		for (size_t dim = 1; dim < orgDimSize.size(); dim++) {
			orgSliceSize[dim] = orgSliceSize[dim-1] * orgDimSize[dim-1];
		}
		recursiveCopyMultiDimData(&oldData, &newData, orgDimSize, orgSliceSize, startDims, outputDimSize, orgDimSize.size() - 1);
		
		return output;
	}
	
	
	template<typename C>
	template<class InputIterator>
	void DataImpl<C>::setValues(InputIterator first, InputIterator last, size_t dataStartPos) throw(CDMException) {
		size_t dataPos = dataStartPos;
		if (dataPos < 0) {
			throw CDMException("dataPos < 0, cannot set data");
		}
		for (; first != last; ++first) {
			if (dataPos < length) {
				theData[dataPos] = static_cast<C>(*first);
			} else {
				throw CDMException("dataPos " + type2string(dataPos) + " >= dataLength " + type2string(length));
			}
			++dataPos;
		}
	}
	
	template<typename T1, typename T2>
	boost::shared_array<T1> convertArrayType(const boost::shared_array<T2>& inData, long length) {
		boost::shared_array<T1> outData(new T1[length]);
		for (int i = 0; i < length; i++) {
			outData[i] = static_cast<T1>(inData[i]);
		}
		return outData;
	}
	
	template<class InputIterator>
	boost::shared_ptr<Data> createData(CDMDataType datatype, size_t length, InputIterator first, InputIterator last) throw(CDMException) {
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
