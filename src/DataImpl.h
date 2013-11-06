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
     * @headerfile "DataImpl.h"
     */

    /**
     * This is a private header file, to be used only within Data.h
     */

    /**
     * @brief create a new shared array with a different type using static_cast
     *        or return the original array
     *
     * Usage: boost::shared_array<OUTTYPE> x = ArrayTypeConverter<OUTTYPE, INTYPE>(inData, inLength)();
     *
     * Using functor here since case INTYPE == OUTTYPE will be partially specialized, and that is only allowed
     * for classes, not for functions in C++.
     *
     * @param inData original data
     * @param length length of original data array
     *
     */
    template<typename T1, typename T2>
    struct ArrayTypeConverter {
        ArrayTypeConverter(const boost::shared_array<T2>& inData, size_t length) : inData(inData), length(length){}
        boost::shared_array<T1> operator()();
      private:
        boost::shared_array<T2> inData;
        size_t length;
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
        const boost::shared_array<T> as() const {return ArrayTypeConverter<T, C>(theData, length)();}
        template<typename T>
        boost::shared_array<T> as() {return ArrayTypeConverter<T, C>(theData, length)();}
        // conversion function
        virtual const boost::shared_array<char> asConstChar() const {return as<char>();}
        virtual boost::shared_array<char> asChar() const {return as<char>();}
        virtual const boost::shared_array<short> asConstShort() const {return as<short>();}
        virtual boost::shared_array<short> asShort() const {return as<short>();}
        virtual const boost::shared_array<int> asConstInt() const {return as<int>();}
        virtual boost::shared_array<int> asInt() const {return as<int>();}
        /// @brief retrieve data as int64
        virtual const boost::shared_array<long long> asConstInt64() const {return as<long long>();}
        /// @brief retrieve data as int64
        virtual boost::shared_array<long long> asInt64() const {return as<long long>();}
        /// @brief retrieve data as uchar
        virtual const boost::shared_array<unsigned char> asConstUChar() const {return as<unsigned char>();}
        /// @brief retrieve data as uchar
        virtual boost::shared_array<unsigned char> asUChar() const {return as<unsigned char>();}
        /// @brief retrieve data as short
        virtual const boost::shared_array<unsigned short> asConstUShort() const {return as<unsigned short>();}
        /// @brief retrieve data as short
        virtual boost::shared_array<unsigned short> asUShort() const {return as<unsigned short>();}
        /// @brief retrieve data as uint
        virtual const boost::shared_array<unsigned int> asConstUInt() const {return as<unsigned int>();}
        /// @brief retrieve data as uint
        virtual boost::shared_array<unsigned int> asUInt() const {return as<unsigned int>();}
        /// @brief retrieve data as uint64
        virtual const boost::shared_array<unsigned long long> asConstUInt64() const {return as<unsigned long long>();}
        /// @brief retrieve data as uint64
        virtual boost::shared_array<unsigned long long> asUInt64() const {return as<unsigned long long>();}
        virtual const boost::shared_array<float> asConstFloat() const {return as<float>();}
        virtual boost::shared_array<float> asFloat() const {return as<float>();}
        virtual const boost::shared_array<double> asConstDouble() const {return as<double>();}
        virtual boost::shared_array<double> asDouble() const {return as<double>();}
        virtual std::string asString(std::string separator = "") const;


        virtual double getDouble(size_t pos) {return static_cast<double>(theData[pos]);}
        virtual long long getLongLong(size_t pos) {return static_cast<long long>(theData[pos]);}
        virtual void setValue(size_t pos, double val) {theData[pos] = static_cast<C>(val);}
        virtual void setValues(size_t startPos, const Data& data, size_t first = 0, size_t last = -1);
        virtual void setAllValues(double val) {std::fill(&theData[0], (&theData[0])+length, static_cast<C>(val));}
        virtual DataPtr clone() const {return DataPtr(new DataImpl<C>(*this));}
        virtual DataPtr slice(std::vector<size_t> orgDimSize, std::vector<size_t> startDims, std::vector<size_t> outputDimSize);
        virtual DataPtr convertDataType(double oldFill, double oldScale, double oldOffset, CDMDataType newType, double newFill, double newScale, double newOffset);
        virtual DataPtr convertDataType(double oldFill, double oldScale, double oldOffset, boost::shared_ptr<UnitsConverter> unitConverter, CDMDataType newType, double newFill, double newScale, double newOffset);
        // specialized for each known type in Data.cc
        virtual CDMDataType getDataType() const {return CDM_NAT;}

        /**
         * set the values of the data by the input-iterator
         */
        template<class InputIterator>
        void setValues(InputIterator begin, InputIterator end, size_t dataStartPos = 0);

    private:
        size_t length;
        boost::shared_array<C> theData;
        DataImpl(const DataImpl<C>& rhs);
        DataImpl<C>& operator=(const DataImpl<C> & rhs);
        void copyData(size_t startPos, const boost::shared_array<C>& otherData, size_t otherSize, size_t otherStart, size_t otherEnd);
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
        theData = boost::shared_array<C>(new C[rhs.length]);
        std::copy(&rhs.theData[0], &rhs.theData[0] + rhs.length, &theData[0]);
        return *this;
    }

    template<typename C>
    void DataImpl<C>::toStream(std::ostream& os, std::string separator) const {
        if (length > 0) {
            for (size_t i = 0; i < (length-1); i++) {
                os << theData[i] << separator;
            }
            // last element without separator
            os << theData[length-1];
        }
    }

    template<typename C>
    std::string DataImpl<C>::asString(std::string separator) const {
        std::ostringstream o;
        toStream(o, separator);
        return o.str();
    }

    template<typename C>
    void DataImpl<C>::copyData(size_t startPos, const boost::shared_array<C>& otherData, size_t otherSize, size_t otherFirst, size_t otherLast) {
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
//	void DataImpl<C>::setValues(size_t startPos, const Data& data, size_t first, size_t last){
//		throw(CDMException("setValues not implemented for this datatype"));
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
    DataPtr DataImpl<C>::slice(std::vector<size_t> orgDimSize, std::vector<size_t> startDims, std::vector<size_t> outputDimSize) {
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
    void DataImpl<C>::setValues(InputIterator begin, InputIterator end, size_t dataStartPos) {
        size_t dist = std::distance(begin, end);
        if ((dist + dataStartPos) > length)
            throw CDMException("dataPos " + type2string(dist+dataStartPos) + " >= dataLength " + type2string(length));
        std::transform(begin, end, &theData[dataStartPos], staticCast<C>());
    }

    template<typename OUT, typename IN>
    boost::shared_array<OUT> convertArrayType(const boost::shared_array<IN>& inData, size_t length, double oldFill, double oldScale, double oldOffset, double newFill, double newScale, double newOffset) {
        boost::shared_array<OUT> outData(new OUT[length]);
        ScaleValue<IN, OUT> sv(oldFill, oldScale, oldOffset, newFill, newScale, newOffset);
        std::transform(&inData[0], &inData[length], &outData[0], sv);
        return outData;
    }
    template<typename OUT, typename IN>
    boost::shared_array<OUT> convertArrayType(const boost::shared_array<IN>& inData, size_t length, double oldFill, double oldScale, double oldOffset, boost::shared_ptr<UnitsConverter> unitsConverter, double newFill, double newScale, double newOffset) {
        boost::shared_array<OUT> outData(new OUT[length]);
        ScaleValueUnits<IN, OUT> sv(oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset);
        std::transform(&inData[0], &inData[length], &outData[0], sv);
        return outData;
    }

    template<typename C>
    DataPtr DataImpl<C>::convertDataType(double oldFill, double oldScale, double oldOffset, CDMDataType newType, double newFill, double newScale, double newOffset)
    {
        DataPtr data(new DataImpl<char>(0)); // dummy default
        switch (newType) {
        case CDM_CHAR: data = DataPtr(new DataImpl<char>(convertArrayType<char>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
        case CDM_SHORT: data = DataPtr(new DataImpl<short>(convertArrayType<short>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
        case CDM_INT: data = DataPtr(new DataImpl<int>(convertArrayType<int>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
        case CDM_UCHAR: data = DataPtr(new DataImpl<unsigned char>(convertArrayType<unsigned char>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
        case CDM_USHORT: data = DataPtr(new DataImpl<unsigned short>(convertArrayType<unsigned short>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
        case CDM_UINT: data = DataPtr(new DataImpl<unsigned int>(convertArrayType<unsigned int>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
        case CDM_INT64: data = DataPtr(new DataImpl<long long>(convertArrayType<long long>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
        case CDM_UINT64: data = DataPtr(new DataImpl<unsigned long long>(convertArrayType<unsigned long long>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
        case CDM_FLOAT: data = DataPtr(new DataImpl<float>(convertArrayType<float>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
        case CDM_DOUBLE: data = DataPtr(new DataImpl<double>(convertArrayType<double>(theData, size(), oldFill, oldScale, oldOffset, newFill, newScale, newOffset), size())); break;
        case CDM_STRING: throw CDMException("cannot convert string datatype"); break;
        case CDM_NAT: throw CDMException("cannot convert CDM_NAT datatype"); break;
        }
        return data;
    }

    template<typename C>
    DataPtr DataImpl<C>::convertDataType(double oldFill, double oldScale, double oldOffset, boost::shared_ptr<UnitsConverter> unitsConverter, CDMDataType newType, double newFill, double newScale, double newOffset)
    {
        DataPtr data(new DataImpl<char>(0)); // dummy default
        switch (newType) {
        case CDM_CHAR: data = DataPtr(new DataImpl<char>(convertArrayType<char>(theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset), size())); break;
        case CDM_SHORT: data = DataPtr(new DataImpl<short>(convertArrayType<short>(theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset), size())); break;
        case CDM_INT: data = DataPtr(new DataImpl<int>(convertArrayType<int>(theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset), size())); break;
        case CDM_UCHAR: data = DataPtr(new DataImpl<unsigned char>(convertArrayType<unsigned char>(theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset), size())); break;
        case CDM_USHORT: data = DataPtr(new DataImpl<unsigned short>(convertArrayType<unsigned short>(theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset), size())); break;
        case CDM_UINT: data = DataPtr(new DataImpl<unsigned int>(convertArrayType<unsigned int>(theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset), size())); break;
        case CDM_INT64: data = DataPtr(new DataImpl<long long>(convertArrayType<long long>(theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset), size())); break;
        case CDM_UINT64: data = DataPtr(new DataImpl<unsigned long long>(convertArrayType<unsigned long long>(theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset), size())); break;
        case CDM_FLOAT: data = DataPtr(new DataImpl<float>(convertArrayType<float>(theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset), size())); break;
        case CDM_DOUBLE: data = DataPtr(new DataImpl<double>(convertArrayType<double>(theData, size(), oldFill, oldScale, oldOffset, unitsConverter, newFill, newScale, newOffset), size())); break;
        case CDM_STRING: throw CDMException("cannot convert string datatype"); break;
        case CDM_NAT: throw CDMException("cannot convert CDM_NAT datatype"); break;
        }
        return data;
    }

    template<typename T1, typename T2>
    boost::shared_array<T1> ArrayTypeConverter<T1,T2>::operator()() {
        boost::shared_array<T1> outData(new T1[length]);
        std::transform(&inData[0], &inData[length], &outData[0], staticCast<T2>());
        return outData;
    }

    // partial specialization for T1==T2
    template<typename T>
    struct ArrayTypeConverter<T,T> {
        ArrayTypeConverter(const boost::shared_array<T>& inData, size_t length) : inData(inData), length(length){}
        boost::shared_array<T> operator()() {return inData;}
      private:
        boost::shared_array<T> inData;
        long length;
    };


}

#endif /*DATAIMPL_H_*/
