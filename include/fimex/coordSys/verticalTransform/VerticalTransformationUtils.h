#ifndef VERTICALTRANSFORMATIONUTILS_H
#define VERTICALTRANSFORMATIONUTILS_H

#include "fimex/DataDecl.h"

#include <boost/shared_array.hpp>
#include <boost/shared_ptr.hpp>

#include <algorithm>
#include <string>
#include <vector>

namespace MetNoFimex {

class CDM;
class CDMReader;
class CoordinateSystem;
class SliceBuilder;
class ArrayDims;
class VerticalConverter;

typedef boost::shared_ptr<CDMReader> CDMReaderPtr;
typedef boost::shared_ptr<const CoordinateSystem> CoordSysPtr;

void removeDimsWithLength1(const CDM& cdm, std::vector<std::string>& dims);
std::string findVariableWithDims(const CDM& cdm, const std::string& standard_name, const std::vector<std::string>& dims);
std::string findVariableWithCS(const CDM& cdm, CoordSysPtr cs, const std::string& standard_name);

template<class C>
void erase_value(C& container, typename C::value_type const& value)
{
    container.erase(std::remove(container.begin(), container.end(), value), container.end());
}

 // FIXME implemented in CDMPressureConversions.cc
DataPtr checkSize(DataPtr data, size_t expected, const std::string& what);
DataPtr checkData(DataPtr data, size_t expected, const std::string& what);

size_t getSliceSize(const SliceBuilder& sb, const std::string& dimName);

SliceBuilder adaptSliceBuilder(const CDM& cdm, const std::string& varName, const SliceBuilder& sbOrig);

DataPtr getSliceData(CDMReaderPtr reader, const SliceBuilder& sbOrig, const std::string& varName, const std::string& unit);
boost::shared_array<float> getSliceFloats(CDMReaderPtr reader, const SliceBuilder& sbOrig, const std::string& varName, const std::string& unit);
boost::shared_array<double> getSliceDoubles(CDMReaderPtr reader, const SliceBuilder& sbOrig, const std::string& varName, const std::string& unit);

std::vector<size_t> getDimSizes(const CDM& cdm, const std::vector<std::string>& dimNames);

SliceBuilder createSliceBuilder(const CDM& cdm, const std::vector<std::string>& dimNames);
SliceBuilder createSliceBuilder(const CDM& cdm, boost::shared_ptr<VerticalConverter> converter);

void setUnLimDimPos(const CDM& cdm, SliceBuilder& sb, size_t unLimDimPos);
size_t setUnLimDimPos(const CDM& cdm, ArrayDims& s, size_t unLimDimPos);

void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1);
void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2);
void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3);
void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4);
void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4, ArrayDims& s5);

ArrayDims makeArrayDims(const CDM& cdm, const std::vector<std::string>& dimNames);
ArrayDims makeArrayDims(const CDM& cdm, const std::string& varName);
ArrayDims makeArrayDims(const CDM& cdm, boost::shared_ptr<VerticalConverter> converter);
ArrayDims makeArrayDims(const SliceBuilder& sb);

boost::shared_ptr<VerticalConverter> verticalConverter(CoordSysPtr cs, CDMReaderPtr reader, int verticalType);
DataPtr verticalData4D(boost::shared_ptr<VerticalConverter> converter, const CDM& cdm, size_t unLimDimPos);
DataPtr verticalData4D(CoordSysPtr cs, CDMReaderPtr reader, size_t unLimDimPos, int verticalType);

DataPtr checkSize(DataPtr data, size_t expected, const std::string& what);
DataPtr checkData(DataPtr data, size_t expected, const std::string& what);

} // namespace MetNoFimex

#endif // VERTICALTRANSFORMATIONUTILS_H

