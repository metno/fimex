#ifndef DATA_H_
#define DATA_H_

#include <boost/shared_array.hpp>
#include <boost/shared_ptr.hpp>
#include <string>
#include <sstream>
#include <iostream>
#include "CDMDataType.h"

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
	virtual long size() const = 0;
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
	
private:
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
	
	virtual long size() const {return length;}
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
	
private:
	long length;
	boost::shared_array<C> theData;
		
};	

boost::shared_ptr<Data> createData(CDMDataType datatype, long length);



// below follow implementations of templates
// (template definitions should be in header files (depending on compiler))
template<typename C>
void DataImpl<C>::toStream(std::ostream& os, std::string separator) const {
	for (int i = 0; i < (length-1); i++) {
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

template<typename T1, typename T2>
boost::shared_array<T1> convertArrayType(const boost::shared_array<T2>& inData, long length) {
	boost::shared_array<T1> outData(new T1[length]);
	for (int i = 0; i < length; i++) {
		outData[i] = static_cast<T1>(inData[i]);
	}
	return outData;
}

}

#endif /*DATA_H_*/
