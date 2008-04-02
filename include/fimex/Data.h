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
		/// @brief retrieve data-copy as float
		virtual boost::shared_array<float> asFloat() const = 0;
		/// @brief retrieve data-copy as double
		virtual boost::shared_array<double> asDouble() const = 0;
		/// @brief retrieve the whole array as a string (with possible separator)
		virtual std::string asString(std::string separator = "") const = 0;

		/// @brief set a value at the desired position
		virtual void setValue(long pos, double val) = 0;
		/**
		 * set the values from another Data implementation
		 * @param startPos the first position the data should be written to
		 * @param data the other data-source
		 * @param first the first data-entry
		 * @param last the last (excluded) data-entry, defaults to MAX size_t, automatically shrunken to fit size
		 */		
		virtual void setValues(size_t startPos, const Data& data, size_t first = 0, size_t last = -1) throw(CDMException) = 0;
		/**
		 * @brief get a slice of the data
		 * 
		 * This slices a multidimensional chunk out of the data.
		 * All parameters must be vectors of the same size (dimension of array).
		 * The first dimension is the fastest moving index (fortran arrays)
		 * 
		 * @param orgDimSize the dimensions of this vector. The product of all orgDimSizes must equal to data.size.
		 * @param startDims The start-position in the original data to fetch data from
		 * @param outputDimSize the size of the output data
		 * @return a Data of the size of outputDimSize with the same datatype as the original type  
		 * 
		 * @throw CDMException on dimension mismatch: (start+size > orgDimSize) or (Product(orgDimSize) != size)
		 */
		virtual boost::shared_ptr<Data> slice(std::vector<size_t> orgDimSize, std::vector<size_t> startDims, std::vector<size_t> outputDimSize) throw(CDMException) = 0;
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
	 * @brief create a dataslice from another Data object
	 * 
	 * @param datatype
	 * @param data the data to read the values from, should be convertible data-format 
	 * @param dataStartPos the first element of data to fetch
	 * @param dataSize the size of the data
	 */
	boost::shared_ptr<Data> createDataSlice(CDMDataType datatype, const Data& data, size_t dataStartPos, size_t dataSize) throw(CDMException);

}

#endif /*DATA_H_*/
