/*
  Fimex, include/fimex/coordSys/verticalTransform/VerticalTransformationUtils.h

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://github.com/metno/fimex/wiki

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/

#ifndef VERTICALTRANSFORMATIONUTILS_H
#define VERTICALTRANSFORMATIONUTILS_H

#include "fimex/ArrayLoop.h"
#include "fimex/CDMReaderDecl.h"
#include "fimex/DataDecl.h"
#include "fimex/SharedArray.h"
#include "fimex/SliceBuilder.h"
#include "fimex/coordSys/CoordSysDecl.h"

#include <algorithm>
#include <memory>
#include <string>
#include <vector>

namespace MetNoFimex {

class CDM;

void removeDimsWithLength1(const CDM& cdm, std::vector<std::string>& dims);
std::string findVariableWithDims(const CDM& cdm, const std::string& standard_name, const std::vector<std::string>& dims);
std::string findVariableWithCS(const CDM& cdm, CoordinateSystem_cp cs, const std::string& standard_name);

template<class C>
void erase_value(C& container, typename C::value_type const& value)
{
    container.erase(std::remove(container.begin(), container.end(), value), container.end());
}

 // FIXME implemented in CDMPressureConversions.cc
DataPtr checkSize(DataPtr data, size_t expected, const std::string& what);
DataPtr checkData(DataPtr data, size_t expected, const std::string& what);

size_t getSliceSize(const SliceBuilder& sb, const std::string& dimName);

class ShapeMerger {
    typedef std::vector<std::string> shape_t;
public:
    ShapeMerger(const CDM& rcdm, CoordinateSystem_cp cs)
        : rcdm_(rcdm)
        , cs_(cs)
    {
    }

    ShapeMerger& merge(const std::string& varName, bool skipLength1=false);
    ShapeMerger& merge(VerticalConverter_p vc, bool skipLength1 = false);

    shape_t shape() const
      { return shape_; }

private:
    ShapeMerger& merge(const shape_t& shape, bool skipLength1);
    shape_t::iterator findAxis(CoordinateAxis_cp axis);
    const std::string& axisDim(CoordinateAxis_cp axis) const;
    bool equalsAxisDim(const std::string& dim, CoordinateAxis_cp axis) const;

private:
    const CDM& rcdm_;
    CoordinateSystem_cp cs_;
    shape_t shape_;
};

SliceBuilder adaptSliceBuilder(const CDM& cdm, const std::string& varName, const SliceBuilder& sbOrig);
SliceBuilder adaptSliceBuilder(const CDM& cdm, VerticalConverter_p converter, const SliceBuilder& sb);

DataPtr getSliceData(CDMReader_p reader, const SliceBuilder& sbOrig, const std::string& varName, const std::string& unit);
shared_array<float> getSliceFloats(CDMReader_p reader, const SliceBuilder& sbOrig, const std::string& varName, const std::string& unit);
shared_array<double> getSliceDoubles(CDMReader_p reader, const SliceBuilder& sbOrig, const std::string& varName, const std::string& unit);

std::vector<size_t> getDimSizes(const CDM& cdm, const std::vector<std::string>& dimNames);

SliceBuilder createSliceBuilder(const CDM& cdm, const std::vector<std::string>& dimNames);
SliceBuilder createSliceBuilder(const CDM& cdm, VerticalConverter_p converter);

void setUnLimDimPos(const CDM& cdm, SliceBuilder& sb, size_t unLimDimPos);
size_t setUnLimDimPos(const CDM& cdm, ArrayDims& s, size_t unLimDimPos);

void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1);
void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2);
void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3);
void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4);
void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4, ArrayDims& s5);

ArrayDims makeArrayDims(const CDM& cdm, const std::vector<std::string>& dimNames);
ArrayDims makeArrayDims(const CDM& cdm, const std::string& varName);
ArrayDims makeArrayDims(const CDM& cdm, VerticalConverter_p converter);
ArrayDims makeArrayDims(const SliceBuilder& sb);

VerticalConverter_p verticalConverter(CoordinateSystem_cp cs, CDMReader_p reader, int verticalType);
DataPtr verticalData4D(VerticalConverter_p converter, const CDM& cdm, size_t unLimDimPos);
DataPtr verticalData4D(CoordinateSystem_cp cs, CDMReader_p reader, size_t unLimDimPos, int verticalType);

DataPtr checkSize(DataPtr data, size_t expected, const std::string& what);
DataPtr checkData(DataPtr data, size_t expected, const std::string& what);

struct Var {
    Var(CDMReader_p reader, const std::string& varName, const std::string& unit, const SliceBuilder& sbOrig);
    Var(CDMReader_p reader, VerticalConverter_p converter, const SliceBuilder& sbOrig);

    operator bool() const { return !!data; }

    const SliceBuilder sb;
    ArrayDims dims;
    DataPtr data;
};

struct VarFloat : public Var {
    VarFloat(CDMReader_p reader, const std::string& varName, const std::string& unit, const SliceBuilder& sbOrig);
    VarFloat(CDMReader_p reader, VerticalConverter_p converter, const SliceBuilder& sbOrig);

    shared_array<float> values;
};

struct VarDouble : public Var {
    VarDouble(CDMReader_p reader, const std::string& varName, const std::string& unit, const SliceBuilder& sbOrig);
    VarDouble(CDMReader_p reader, VerticalConverter_p converter, const SliceBuilder& sbOrig);

    shared_array<double> values;
};

} // namespace MetNoFimex

#endif // VERTICALTRANSFORMATIONUTILS_H

