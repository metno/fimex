/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

#include "fimex/DataDecl.h"
#include "fimex/UnitsConverterDecl.h"

#include "fimex/CDMDataType.h"

#include "fimex/SharedArray.h"

#include <algorithm>
#include <iosfwd>
#include <string>
#include <vector>

namespace MetNoFimex
{
/**
     * @headerfile fimex/Data.h
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
    virtual void toStream(std::ostream&, const std::string& separator = "") const = 0;

    /// @brief retrieve data as char
    virtual shared_array<char> asChar() const = 0;

    /// @brief retrieve data as short
    virtual shared_array<short> asShort() const = 0;

    /// @brief retrieve data as int
    virtual shared_array<int> asInt() const = 0;

    /// @brief retrieve data as int64
    virtual shared_array<long long> asInt64() const = 0;

    /// @brief retrieve data as uchar
    virtual shared_array<unsigned char> asUChar() const = 0;

    /// @brief retrieve data as short
    virtual shared_array<unsigned short> asUShort() const = 0;

    /// @brief retrieve data as uint
    virtual shared_array<unsigned int> asUInt() const = 0;

    /// @brief retrieve data as uint64
    virtual shared_array<unsigned long long> asUInt64() const = 0;

    /// @brief retrieve data as float (eventually copy)
    virtual shared_array<float> asFloat() const = 0;

    /// @brief retrieve data as double
    virtual shared_array<double> asDouble() const = 0;

    /// @brief retrieve data as array of strings
    virtual shared_array<std::string> asStrings() const = 0;

    /// @brief retrieve the whole array as a string (with possible separator)
    virtual std::string asString(const std::string& separator = "") const = 0;

    /**
         *  @brief get a value at the desired position
         *
         *  Usefull in combination with MetNoFimex::Index and getDims().
         */
    virtual double getDouble(size_t pos) = 0;
    /**
         *  @brief get a value at the desired position
         *
         *  Usefull in combination with MetNoFimex::Index and getDims().
         */
    virtual long long getLongLong(size_t pos) = 0;

    /// @brief set a value at the desired position
    virtual void setValue(size_t pos, double val) = 0;

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
    virtual DataPtr clone() const = 0;

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
    virtual DataPtr slice(const std::vector<size_t>& orgDimSize, const std::vector<size_t>& startDims, const std::vector<size_t>& outputDimSize) = 0;

    /**
         * @brief convert the datatype from one type,fill,scale,offset to another
         */
    virtual DataPtr convertDataType(double oldFill, double oldScale, double oldOffset, CDMDataType newType, double newFill, double newScale, double newOffset) = 0;

    virtual DataPtr convertDataType(double oldFill, double oldScale, double oldOffset, UnitsConverter_p unitConverter, CDMDataType newType, double newFill,
                                    double newScale, double newOffset) = 0;

    /**
         * return the CDMDataType of this data
         */
    virtual CDMDataType getDataType() const = 0;
};

/**
     * @brief create a Data-pointer of the datatype
     *
     * @param datatype
     * @param length of the data array
     * @param val default value for data elements, 0 by default
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(CDMDataType datatype, size_t length, double val = 0);

/**
     * @brief create a Data-pointer of type CDM_DOUBLE
     *
     * @param length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(size_t length, shared_array<double> array);
/**
     * @brief create a Data-pointer of type CDM_FLOAT
     *
     * @param length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(size_t length, shared_array<float> array);
/**
     * @brief create a Data-pointer of type CDM_INT
     *
     * @param length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(size_t length, shared_array<int> array);
/**
     * @brief create a Data-pointer of type CDM_SHORT
     *
     * @param length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(size_t length, shared_array<short> array);
/**
     * @brief create a Data-pointer of type CDM_CHAR
     *
     * @param length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(size_t length, shared_array<char> array);
/**
     * @brief create a Data-pointer of type CDM_UINT
     *
     * @param length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(size_t length, shared_array<unsigned int> array);
/**
     * @brief create a Data-pointer of type CDM_INT64
     *
     * @param length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(size_t length, shared_array<long long> array);
/**
     * @brief create a Data-pointer of type CDM_UINT64
     *
     * @param length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(size_t length, shared_array<unsigned long long> array);
/**
     * @brief create a Data-pointer of type CDM_USHORT
     *
     * @param length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(size_t length, shared_array<unsigned short> array);
/**
     * @brief create a Data-pointer of type CDM_UCHAR
     *
     * @param length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(size_t length, shared_array<unsigned char> array);
/**
     * @brief create a Data-pointer of type CDM_STRINGS
     *
     * @param length of the data array
     * @param array the data array
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(size_t length, shared_array<std::string> array);

/**
     * @brief create a Data-pointer of type CDM_STRING
     *
     * @param value the text
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
DataPtr createData(const std::string& value);

/**
     * @brief create a Data-pointer of the datatype and fill with the data from the iterator
     *
     * @param datatype
     * @param first start of container containing the data to fill the array with
     * @param last end (excluded) of the container containing the data to fill the array with
     * @return Base-Class ptr of the DataImpl belonging to the datatype
     */
template<class InputIterator>
DataPtr createData(CDMDataType datatype, InputIterator first, InputIterator last);

/**
     * @brief create a one-dimensional dataslice from another Data object
     *
     * @param datatype of the return-data
     * @param data the data to read the values from, should be convertible data-format
     * @param dataStartPos the first element of data to fetch
     * @param dataSize the size of the data
     */
DataPtr createDataSlice(CDMDataType datatype, const Data& data, size_t dataStartPos, size_t dataSize);

template <class T, class InputIterator>
DataPtr createDataFromIterator(InputIterator first, InputIterator last)
{
    const size_t length = std::distance(first, last);
    auto values = make_shared_array<T>(length);
    std::copy(first, last, values.get());
    return createData(length, values);
}

template<class InputIterator>
DataPtr createData(CDMDataType datatype, InputIterator first, InputIterator last)
{
    switch (datatype) {
    case CDM_DOUBLE:
        return createDataFromIterator<double>(first, last);
    case CDM_FLOAT:
        return createDataFromIterator<float>(first, last);
    case CDM_INT64:
        return createDataFromIterator<long long>(first, last);
    case CDM_INT:
        return createDataFromIterator<int>(first, last);
    case CDM_SHORT:
        return createDataFromIterator<short>(first, last);
    case CDM_CHAR:
        return createDataFromIterator<char>(first, last);
    case CDM_UINT64:
        return createDataFromIterator<unsigned long long>(first, last);
    case CDM_UINT:
        return createDataFromIterator<unsigned int>(first, last);
    case CDM_USHORT:
        return createDataFromIterator<unsigned short>(first, last);
    case CDM_UCHAR:
        return createDataFromIterator<unsigned char>(first, last);
    case CDM_STRING: {
        return createData(std::string(first, last));
    }
    case CDM_NAT:
    default:
        break;
    }
    return createData(0, make_shared_array<char>(0)); // a dummy dataset
}

DataPtr convertValues(const Data& data, CDMDataType newType);

} // namespace MetNoFimex

#endif /*DATA_H_*/
