#ifndef DATA_H_
#define DATA_H_

#include <boost/shared_array.hpp>

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
boost::shared_array<T1> convertArrayType(const boost::shared_array<T2>& inData, long length) {
	boost::shared_array<T1> outData(new T1[length]);
	for (int i = 0; i < length; i++) {
		outData[i] = static_cast<T1>(inData[i]);
	}
	return outData;
}


/**
 * General class for storing different basic array pointers plus length
 */
class Data
{
public:
	Data() {}
	virtual ~Data() {}

	/// @brief size of the data
	virtual long size() const = 0;
	/// @brief sizeof the data-impl datatype
	virtual int bytes_for_one() const = 0;
	/// @brief printing of the current data to ostream
	virtual std::ostream& print(std::ostream&) const = 0;

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
	
private:
};

template<class C>
class DataImpl : public Data
{
public:
	explicit DataImpl(boost::shared_array<C> array, long length)
	: length(length), theData(array) {}
	virtual ~DataImpl() {}
	
	virtual long size() const {return length;}
	virtual int bytes_for_one() const {return sizeof(C);}
	virtual std::ostream& print(std::ostream& os) const {
		for (int i = 0; i < length; i++) {
			os << theData[i];
		}
		return os;
	}
	// conversion function
	virtual boost::shared_array<char> asChar() const {return convertArrayType<char, C>(theData, length);}
	virtual boost::shared_array<short> asShort() const {return convertArrayType<short, C>(theData, length);}
	virtual boost::shared_array<int> asInt() const {return convertArrayType<int, C>(theData, length);}
	virtual boost::shared_array<long> asLong() const {return convertArrayType<long, C>(theData, length);}
	virtual boost::shared_array<float> asFloat() const {return convertArrayType<float, C>(theData, length);}
	virtual boost::shared_array<double> asDouble() const {return convertArrayType<double, C>(theData, length);}

private:
	long length;
	boost::shared_array<C> theData;
		
};	

}

#endif /*DATA_H_*/
