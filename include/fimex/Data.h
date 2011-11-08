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

#ifndef DATA_H_
#define DATA_H_

#include <boost/shared_array.hpp>
#include <boost/shared_ptr.hpp>
#include <string>
#include <sstream>
#include <iostream>
#include "fimex/CDMDataType.h"
#include "fimex/CDMException.h"
#include "fimex/Utils.h"

namespace MetNoFimex
{
    /**
     * @headerfile "fimex/Data.h"
     */

	/**
	 * General class for storing different basic array pointers plus length
	 */
	class Data
	{
	public:
		virtual ~Data() = 0;

		/// @brief size of the data
		virtual size_t size() const = 0;
		/// @brief sizeof the data-impl datatype
		virtual int bytes_for_one() const = 0;
		virtual void* getDataPtr() = 0;
		/// @brief printing of the current data to ostream, with optional separator
		virtual void toStream(std::ostream&, std::string separator = "") const = 0;

		/// @brief retrieve data as char
		virtual boost::shared_array<const char> asConstChar() const = 0;
		/// @brief retrieve data as char
		virtual boost::shared_array<char> asChar() = 0;
		/// @brief retrieve data as short
		virtual boost::shared_array<const short> asConstShort() const = 0;
		/// @brief retrieve data as short
		virtual boost::shared_array<short> asShort() = 0;
		/// @brief retrieve data as int
		virtual boost::shared_array<const int> asConstInt() const = 0;
		/// @brief retrieve data as int
		virtual boost::shared_array<int> asInt() = 0;
        /// @brief retrieve data as int64
        virtual boost::shared_array<const long long> asConstInt64() const = 0;
        /// @brief retrieve data as int64
        virtual boost::shared_array<long long> asInt64() = 0;
        /// @brief retrieve data as uchar
        virtual boost::shared_array<const unsigned char> asConstUChar() const = 0;
        /// @brief retrieve data as uchar
        virtual boost::shared_array<unsigned char> asUChar() = 0;
        /// @brief retrieve data as short
        virtual boost::shared_array<const unsigned short> asConstUShort() const = 0;
        /// @brief retrieve data as short
        virtual boost::shared_array<unsigned short> asUShort() = 0;
        /// @brief retrieve data as uint
        virtual boost::shared_array<const unsigned int> asConstUInt() const = 0;
        /// @brief retrieve data as uint
        virtual boost::shared_array<unsigned int> asUInt() = 0;
        /// @brief retrieve data as uint64
        virtual boost::shared_array<const unsigned long long> asConstUInt64() const = 0;
        /// @brief retrieve data as uint64
        virtual boost::shared_array<unsigned long long> asUInt64() = 0;
		/// @brief retrieve data as float
		virtual boost::shared_array<const float> asConstFloat() const = 0;
		/// @brief retrieve data as float (eventually copy)
		virtual boost::shared_array<float> asFloat() = 0;
		/// @brief retrieve data as double
		virtual boost::shared_array<const double> asConstDouble() const = 0;
		/// @brief retrieve data as double
		virtual boost::shared_array<double> asDouble() = 0;
		/// @brief retrieve the whole array as a string (with possible separator)
		virtual std::string asString(std::string separator = "") const = 0;

		/// @brief set a value at the desired position
		virtual void setValue(long pos, double val) = 0;
		/**
		 * set the values from another Data implementation
		 * @param startPos the first position the data should be written to
		 * @param data the other data-source
		 * @param first the first data-entry
		 * @param end the last (excluded) data-entry, defaults to MAX size_t, automatically shrunken to fit size
		 */
		virtual void setValues(size_t startPos, const Data& data, size_t first = 0, size_t end = -1) = 0;
		/**
		 * set all values to the submitted value
		 * @param val value to set
		 */
		virtual void setAllValues(double val) = 0;
		/**
		 * @brief duplicate the data
		 *
		 * The clone operation generates a real duplicate
		 * of the data. The internal array-data will be copied.
		 */
		virtual boost::shared_ptr<Data> clone() const = 0;
		/**
		 * @brief get a multi-dimensional slice of the data
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
		virtual boost::shared_ptr<Data> slice(std::vector<size_t> orgDimSize, std::vector<size_t> startDims, std::vector<size_t> outputDimSize) = 0;
		/**
		 * @brief convert the datatype from one type,fill,scale,offset to another
		 */
		virtual boost::shared_ptr<Data> convertDataType(double oldFill, double oldScale, double oldOffset, CDMDataType newType, double newFill, double newScale, double newOffset) = 0;
		/**
		 * return the CDMDataType of this data
		 */
		virtual CDMDataType getDataType() const = 0;
	};

	/**
	 * @brief create a Data-pointer of the datatype
	 *
	 * @param datatype
	 * @param size_t length of the data array
	 * @param val default value for data elements, 0 by default
	 * @return Base-Class ptr of the DataImpl belonging to the datatype
	 */
	boost::shared_ptr<Data> createData(CDMDataType datatype, size_t length, double val = 0);

    /**
     * @brief create a Data-pointer of type CDM_DOUBLE
     *
     * @param size_t length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
    boost::shared_ptr<Data> createData(size_t length, boost::shared_array<double> array);
    /**
     * @brief create a Data-pointer of type CDM_FLOAT
     *
     * @param size_t length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
    boost::shared_ptr<Data> createData(size_t length, boost::shared_array<float> array);
    /**
     * @brief create a Data-pointer of type CDM_INT
     *
     * @param size_t length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
    boost::shared_ptr<Data> createData(size_t length, boost::shared_array<int> array);
    /**
     * @brief create a Data-pointer of type CDM_SHORT
     *
     * @param size_t length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
    boost::shared_ptr<Data> createData(size_t length, boost::shared_array<short> array);
    /**
     * @brief create a Data-pointer of type CDM_CHAR
     *
     * @param size_t length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
    boost::shared_ptr<Data> createData(size_t length, boost::shared_array<char> array);
    /**
     * @brief create a Data-pointer of type CDM_UINT
     *
     * @param size_t length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
    boost::shared_ptr<Data> createData(size_t length, boost::shared_array<unsigned int> array);
    /**
     * @brief create a Data-pointer of type CDM_INT64
     *
     * @param size_t length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
    boost::shared_ptr<Data> createData(size_t length, boost::shared_array<long long> array);
    /**
     * @brief create a Data-pointer of type CDM_UINT64
     *
     * @param size_t length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
    boost::shared_ptr<Data> createData(size_t length, boost::shared_array<unsigned long long> array);
    /**
     * @brief create a Data-pointer of type CDM_USHORT
     *
     * @param size_t length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
    boost::shared_ptr<Data> createData(size_t length, boost::shared_array<unsigned short> array);
    /**
     * @brief create a Data-pointer of type CDM_UCHAR
     *
     * @param size_t length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
    boost::shared_ptr<Data> createData(size_t length, boost::shared_array<unsigned char> array);

    /**
     * @brief create a Data-pointer of the datatype and fill with the data from the iterator
     *
     * @param datatype
     * @param first start of container containing the data to fill the array with
     * @param last end (excluded) of the container containing the data to fill the array with
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
    template<class InputIterator>
    boost::shared_ptr<Data> createData(CDMDataType datatype, InputIterator first, InputIterator last);

    /**
	 * @brief create a one-dimensional dataslice from another Data object
	 *
	 * @param datatype of the return-data
	 * @param data the data to read the values from, should be convertible data-format
	 * @param dataStartPos the first element of data to fetch
	 * @param dataSize the size of the data
	 */
	boost::shared_ptr<Data> createDataSlice(CDMDataType datatype, const Data& data, size_t dataStartPos, size_t dataSize);

	/* BELOW follow template implementations */
    template<class InputIterator>
	boost::shared_ptr<Data> createData(CDMDataType datatype, InputIterator first, InputIterator last)
	{
        size_t length = std::distance(first, last);
	    switch (datatype) {
            case CDM_DOUBLE: { boost::shared_array<double> ary(new double[length]);     std::copy(first, last, ary.get()); return createData(length, ary); }
                case CDM_FLOAT:  { boost::shared_array<float> ary(new float[length]);   std::copy(first, last, ary.get()); return createData(length, ary); }
                case CDM_INT:    { boost::shared_array<int> ary(new int[length]);       std::copy(first, last, ary.get()); return createData(length, ary); }
                case CDM_SHORT:  { boost::shared_array<short> ary(new short[length]);   std::copy(first, last, ary.get()); return createData(length, ary); }
                case CDM_CHAR:   { boost::shared_array<char> ary(new char[length]);     std::copy(first, last, ary.get()); return createData(length, ary); }
                case CDM_INT64:    { boost::shared_array<long long> ary(new long long[length]);       std::copy(first, last, ary.get()); return createData(length, ary); }
                case CDM_UINT:    { boost::shared_array<unsigned int> ary(new unsigned int[length]);       std::copy(first, last, ary.get()); return createData(length, ary); }
                case CDM_UINT64:    { boost::shared_array<unsigned long long> ary(new unsigned long long[length]);       std::copy(first, last, ary.get()); return createData(length, ary); }
                case CDM_USHORT:  { boost::shared_array<unsigned short> ary(new unsigned short[length]);   std::copy(first, last, ary.get()); return createData(length, ary); }
                case CDM_UCHAR:   { boost::shared_array<unsigned char> ary(new unsigned char[length]);     std::copy(first, last, ary.get()); return createData(length, ary); }
	        case CDM_NAT: ;
	        default: ;
	     }
	     return createData(0, boost::shared_array<char>(new char[0])); // a dummy dataset
	}

}

#endif /*DATA_H_*/
