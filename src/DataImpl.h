/*
 * Fimex
 *
 * (C) Copyright 2008-2019, met.no
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

#include "fimex/CDMDataType.h"
#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/DataUtils.h"
#include "fimex/MathUtils.h"
#include "fimex/Type2String.h"

#include <algorithm>
#include <cmath>
#include <iterator>
#include <limits>
#include <memory>
#include <sstream>
#include <string>

namespace MetNoFimex {

/**
 * @headerfile "DataImpl.h"
 */

/**
 * This is a private header file, to be used only within Data.h
 */

/**
 * @brief create a new shared array with a different type using static_cast
 *        or return the original array
 *
 * Usage: shared_array<OUTTYPE> x = ArrayTypeConverter<OUTTYPE, INTYPE>(inData, inLength)();
 *
 * Using functor here since case INTYPE == OUTTYPE will be partially specialized, and that is only allowed
 * for classes, not for functions in C++.
 *
 * @param inData original data
 * @param length length of original data array
 *
 */
template <typename T1, typename T2>
struct ArrayTypeConverter
{
    ArrayTypeConverter(const shared_array<T2>& inData, size_t length)
        : inData(inData)
        , length(length)
    {
    }
    shared_array<T1> operator()();

private:
    shared_array<T2> inData;
    size_t length;
};


template<typename C>
class DataImpl : public Data
{
public:
    /// constructor where the array will be automatically allocated
    explicit DataImpl(long length)
        : length(length), theData(new C[length]) {}
    explicit DataImpl(shared_array<C> array, long length)
        : length(length)
        , theData(array)
    {
    }
    ~DataImpl() {}

    size_t size() const override {return length;}
    int bytes_for_one() const override {return sizeof(C);}
    void* getDataPtr() override {return &theData[0];}
    void toStream(std::ostream& os, const std::string& separator = "") const override;

    /**
         *  @brief get the datapointer of the data
         */
    virtual const shared_array<C> asBase() const { return theData; }
    /**
         * general conversion function, not in base since template methods not allowed
         */
    template <typename T>
    const shared_array<T> as() const
    {
        return ArrayTypeConverter<T, C>(theData, length)();
    }
    template <typename T>
    shared_array<T> as()
    {
        return ArrayTypeConverter<T, C>(theData, length)();
    }
    // conversion function
    shared_array<char> asChar() const override { return as<char>(); }
    shared_array<short> asShort() const override { return as<short>(); }
    shared_array<int> asInt() const override { return as<int>(); }
    shared_array<long long> asInt64() const override { return as<long long>(); }
    shared_array<unsigned char> asUChar() const override { return as<unsigned char>(); }
    shared_array<unsigned short> asUShort() const override { return as<unsigned short>(); }
    shared_array<unsigned int> asUInt() const override { return as<unsigned int>(); }
    shared_array<unsigned long long> asUInt64() const override { return as<unsigned long long>(); }
    shared_array<std::string> asStrings() const override { return as<std::string>(); }
    shared_array<float> asFloat() const override { return as<float>(); }
    shared_array<double> asDouble() const override { return as<double>(); }
    std::string asString(const std::string& separator = "") const override;

    double getDouble(size_t pos) override {return data_caster<double, C>()(theData[pos]);}
    long long getLongLong(size_t pos) override {return data_caster<long long, C>()(theData[pos]);}
    void setValue(size_t pos, double val) override {theData[pos] = data_caster<C, double>()(val);}
    void setValues(size_t startPos, const Data& data, size_t first = 0, size_t last = -1) override;
    void setAllValues(double val) override {std::fill(&theData[0], (&theData[0])+length, data_caster<C, double>()(val));}
    DataPtr clone() const override;
    DataPtr slice(const std::vector<size_t>& orgDimSize, const std::vector<size_t>& startDims, const std::vector<size_t>& outputDimSize) override;
    DataPtr convertDataType(double oldFill, double oldScale, double oldOffset, CDMDataType newType, double newFill, double newScale, double newOffset) override;
    DataPtr convertDataType(double oldFill, double oldScale, double oldOffset, UnitsConverter_p unitConverter, CDMDataType newType, double newFill,
                            double newScale, double newOffset) override;
    // specialized for each known type in Data.cc
    CDMDataType getDataType() const override;

    /**
         * set the values of the data by the input-iterator
         */
    template<class InputIterator>
    void setValues(InputIterator begin, InputIterator end, size_t dataStartPos = 0);

private:
    size_t length;
    shared_array<C> theData;
    DataImpl(const DataImpl<C>& rhs);
    DataImpl<C>& operator=(const DataImpl<C> & rhs);
    void copyData(size_t startPos, const shared_array<C>& otherData, size_t otherSize, size_t otherStart, size_t otherEnd);
};


// below follow implementations of templates
// (template definitions should be in header files (depending on compiler))
template<typename C>
DataImpl<C>::DataImpl(const DataImpl<C>& rhs)
    : length(rhs.length), theData(new C[rhs.length])
{
    std::copy(&rhs.theData[0], &rhs.theData[0] + rhs.length, &theData[0]);
}

template<typename C>
DataImpl<C>& DataImpl<C>::operator=(const DataImpl<C>& rhs)
{
    length = rhs.length;
    theData = shared_array<C>(new C[rhs.length]);
    std::copy(&rhs.theData[0], &rhs.theData[0] + rhs.length, &theData[0]);
    return *this;
}

template <typename C>
void DataImpl<C>::toStream(std::ostream& os, const std::string& separator) const
{
    for (size_t i = 0; i < length; i++) {
        if (i != 0)
            os << separator;
        type2stream(os, theData[i]);
    }
}

template <typename C>
std::string DataImpl<C>::asString(const std::string& separator) const
{
    std::ostringstream o;
    toStream(o, separator);
    return o.str();
}

template <typename C>
void DataImpl<C>::copyData(size_t startPos, const shared_array<C>& otherData, size_t otherSize, size_t otherFirst, size_t otherLast)
{
    if (otherFirst > otherSize) {
        throw CDMException("data-region-start "+ type2string(otherFirst) + " outside range: "+ type2string(otherSize));
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
//	void DataImpl<C>::setValues(size_t startPos, const Data& data, size_t first, size_t last){
//		throw CDMException("setValues not implemented for this datatype");
//	}

// declaration of implemented function (in Data.cc)
template<>
void DataImpl<char>::setValues(size_t startPos, const Data& data, size_t first, size_t last);
template<>
void DataImpl<short>::setValues(size_t startPos, const Data& data, size_t first, size_t last);
template<>
void DataImpl<int>::setValues(size_t startPos, const Data& data, size_t first, size_t last);
template<>
void DataImpl<unsigned char>::setValues(size_t startPos, const Data& data, size_t first, size_t last);
template<>
void DataImpl<unsigned short>::setValues(size_t startPos, const Data& data, size_t first, size_t last);
template<>
void DataImpl<unsigned int>::setValues(size_t startPos, const Data& data, size_t first, size_t last);
template<>
void DataImpl<long long>::setValues(size_t startPos, const Data& data, size_t first, size_t last);
template<>
void DataImpl<unsigned long long>::setValues(size_t startPos, const Data& data, size_t first, size_t last);
template<>
void DataImpl<float>::setValues(size_t startPos, const Data& data, size_t first, size_t last);
template<>
void DataImpl<double>::setValues(size_t startPos, const Data& data, size_t first, size_t last);
template<>
void DataImpl<std::string>::setValues(size_t startPos, const Data& data, size_t first, size_t last);

/**
     * recursively copy data by moving the newData and orgData pointers forward and copy the data at the current position
     *
     * it's assumed that the first dim in the vector is the fastest moving (fortran like)
     *
     * @param orgData pointer to the current postion of the original array
     * @param newData pointer to the current position of the new array
     * @param orgDimSize the original dimensions of orgData
     * @param orgSliceSize helper-array with orgSliceSize[0] = 1; orgSliceSize[n] = orgDimSize[n] * orgSliceSize[n-1]
     * @param newStart the start positions in the new data
     * @param newSize the dimensions of the newData
     * @param currentDim the dimension currently under work, should be between (orgData.size()-1) and 0
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
DataPtr DataImpl<C>::slice(const std::vector<size_t>& orgDimSize, const std::vector<size_t>& startDims, const std::vector<size_t>& outputDimSize) {
    // handle scalar data
    if (orgDimSize.size() == 0) {
        return clone();
    }
    // get the sizes of the original data and the output data
    size_t orgSize = 1;
    size_t outputSize = 1;
    for (size_t i = 0; i < orgDimSize.size(); ++i) {
        outputSize *= outputDimSize[i];
        orgSize *= orgDimSize[i];
        if (orgDimSize[i] < (startDims[i] + outputDimSize[i]))
            throw CDMException("dimension-size error, start+size > orgSize: " + type2string(startDims[i] + outputDimSize[i]) + ">" +
                               type2string(orgDimSize[i]));
    }
    if (orgSize != size())
        throw CDMException("dimension-mismatch: " + type2string(size()) + "!=" + type2string(orgSize));

    // get the old and new datacontainer
    std::shared_ptr<DataImpl<C>> output(new DataImpl<C>(outputSize));
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
void DataImpl<C>::setValues(InputIterator begin, InputIterator end, size_t dataStartPos) {
    size_t dist = std::distance(begin, end);
    if ((dist + dataStartPos) > length)
        throw CDMException("dataPos " + type2string(dist+dataStartPos) + " >= dataLength " + type2string(length));
    std::transform(begin, end, &theData[dataStartPos], data_caster<C, typename InputIterator::value_type>());
}

template <typename OUT, typename IN>
shared_array<OUT> convertArrayType(const shared_array<IN>& inData, size_t length, double oldFill, double oldScale, double oldOffset,
                                   UnitsConverter_p unitsConverter, double newFill, double newScale, double newOffset)
{
    shared_array<OUT> outData(new OUT[length]);
    if (!unitsConverter) {
        ScaleValue<IN, OUT> sv(oldFill, oldScale, oldOffset, newFill, newScale, newOffset);
        std::transform(&inData[0], &inData[length], &outData[0], sv);
    } else {
        ScaleValueUnits<IN, OUT> sv(oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset);
        std::transform(&inData[0], &inData[length], &outData[0], sv);
    }
    return outData;
}

template<typename C>
DataPtr DataImpl<C>::convertDataType(double oldFill, double oldScale, double oldOffset, CDMDataType newType, double newFill, double newScale, double newOffset)
{
    return convertDataType(oldFill, oldScale, oldOffset, UnitsConverter_p(), newType, newFill, newScale, newOffset);
}


template <typename C>
DataPtr DataImpl<C>::convertDataType(double oldFill, double oldScale, double oldOffset, UnitsConverter_p unitsConverter, CDMDataType newType,
                                     double newFill, double newScale, double newOffset)
{
    if (oldFill == newFill && oldScale == newScale && oldOffset == newOffset && !unitsConverter)
        return convertValues(*this, newType);

    // clang-format off
        switch (newType) {
        case CDM_CHAR:   return createData(size(), convertArrayType<char>              (theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset));
        case CDM_SHORT:  return createData(size(), convertArrayType<short>             (theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset));
        case CDM_INT:    return createData(size(), convertArrayType<int>               (theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset));
        case CDM_UCHAR:  return createData(size(), convertArrayType<unsigned char>     (theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset));
        case CDM_USHORT: return createData(size(), convertArrayType<unsigned short>    (theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset));
        case CDM_UINT:   return createData(size(), convertArrayType<unsigned int>      (theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset));
        case CDM_INT64:  return createData(size(), convertArrayType<long long>         (theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset));
        case CDM_UINT64: return createData(size(), convertArrayType<unsigned long long>(theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset));
        case CDM_FLOAT:  return createData(size(), convertArrayType<float>             (theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset));
        case CDM_DOUBLE: return createData(size(), convertArrayType<double>            (theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset));
        case CDM_STRING:
        case CDM_STRINGS:
        case CDM_NAT: throw CDMException("cannot convert " + type2string(newType) + " datatype");
        }
    // clang-format on
    throw CDMException("cannot convert unknown datatype");
}

template <>
DataPtr DataImpl<std::string>::convertDataType(double, double, double, UnitsConverter_p, CDMDataType, double, double, double)
{
    throw CDMException("cannot convert CDM_STRINGS datatype");
}

template <typename T1, typename T2>
shared_array<T1> ArrayTypeConverter<T1, T2>::operator()()
{
    shared_array<T1> outData(new T1[length]);
    std::transform(&inData[0], &inData[length], &outData[0], data_caster<T1, T2>());
    return outData;
}

// partial specialization for T1==T2
template<typename T>
struct ArrayTypeConverter<T,T> {
    ArrayTypeConverter(const shared_array<T>& inData, size_t length)
        : inData(inData)
        , length(length)
    {
    }
    shared_array<T> operator()() { return inData; }

private:
    shared_array<T> inData;
    long length;
};

} // namespace MetNoFimex

#endif /*DATAIMPL_H_*/
