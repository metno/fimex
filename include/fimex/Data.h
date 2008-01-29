#ifndef DATA_H_
#define DATA_H_

#include <boost/shared_array.hpp>
#include <boost/shared_ptr.hpp>
#include <string>
#include <sstream>
#include <iostream>
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

	/**
	 * General class for storing different basic array pointers plus length
	 */
	class Data
	{
	public:
		virtual ~Data() {}

		/// @brief size of the data
		virtual size_t size() const = 0;
		/// @brief sizeof the data-impl datatype
		virtual int bytes_for_one() const = 0;
		/// @brief printing of the current data to ostream, with optional separator
		virtual void toStream(std::ostream&, std::string separator = "") const = 0;

		/// @brief retrieve data-copy as char
		virtual boost::shared_array<char> asChar() const = 0;
		/// @brief retrieve data-copy as short
		virtual boost::shared_array<short> asShort() const = 0;
		/// @brief retrieve data-copy as int
		virtual boost::shared_array<int> asInt() const = 0;
		/// @brief retrieve data-copy as long
		virtual boost::shared_array<long> asLong() const = 0;
		/// @brief retrieve data-copy as float
		virtual boost::shared_array<float> asFloat() const = 0;
		/// @brief retrieve data-copy as double
		virtual boost::shared_array<double> asDouble() const = 0;
		/// @brief retrieve the whole array as a string (with possible separator)
		virtual std::string asString(std::string separator = "") const = 0;

		/// @brief set a value at the desired position
		virtual void setValue(long pos, double val) = 0;
	};

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

		/// @brief get the datapointer of the data
		virtual const boost::shared_array<C> asBase() const {return theData;}
		// conversion function
		virtual boost::shared_array<char> asChar() const {return convertArrayType<char, C>(theData, length);}
		virtual boost::shared_array<short> asShort() const {return convertArrayType<short, C>(theData, length);}
		virtual boost::shared_array<int> asInt() const {return convertArrayType<int, C>(theData, length);}
		virtual boost::shared_array<long> asLong() const {return convertArrayType<long, C>(theData, length);}
		virtual boost::shared_array<float> asFloat() const {return convertArrayType<float, C>(theData, length);}
		virtual boost::shared_array<double> asDouble() const {return convertArrayType<double, C>(theData, length);}
		virtual std::string asString(std::string separator = "") const;

		virtual void setValue(long pos, double val) {theData[pos] = static_cast<C>(val);}
		
		template<class InputIterator>
		void setValues(InputIterator first, InputIterator last, size_t dataStartPos = 0) throw(CDMException);

	private:
		size_t length;
		boost::shared_array<C> theData;

	};

	/**
	 * @brief create a Data-pointer of the datatype
	 * 
	 * @param datatype
	 * @param size_t length of the data array
	 * @return Base-Class ptr of the DataImpl belonging to the datatype 
	 */
	boost::shared_ptr<Data> createData(CDMDataType datatype, size_t length) throw(CDMException);

	/**
	 * @brief create a Data-pointer of the datatype {@see createData(CDMDataType datatype, size_t length)}
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
	template<class InputIterator>
	void DataImpl<C>::setValues(InputIterator first, InputIterator last, size_t dataStartPos) throw(CDMException) {
		size_t dataPos = dataStartPos;
		if (dataPos < 0) {
			throw CDMException("dataPos < 0, cannot set data");
		}
		while (first != last) {
			if (dataPos < length) {
				theData[dataPos++] = *first++;
			} else {
				throw CDMException("dataPos " + type2string(dataPos) + " >= dataLength " + type2string(length));
			}
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

#endif /*DATA_H_*/
