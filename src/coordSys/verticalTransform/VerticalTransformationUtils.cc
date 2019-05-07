/*
  Fimex, src/coordSys/verticalTransform/VerticalTransformationUtils.cc

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

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

#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalConverter.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/SliceBuilder.h"
#include "fimex/Utils.h"

#include <map>

namespace MetNoFimex {

namespace {

static Logger_p logger = getLogger("fimex.VerticalTransformationUtils");

typedef std::map<std::string, std::string> string_string_m;

struct CompleteCSForVariable : public std::unary_function<std::string, bool>
{
public:
    CompleteCSForVariable(CoordinateSystem_cp cs)
        : cs_(cs)
    {
    }
    bool operator()(const std::string& varName) const
      { return cs_->isCSAndCompleteFor(varName); }
private:
    CoordinateSystem_cp cs_;
};

} // anonymous namespace

void removeDimsWithLength1(const CDM& cdm, string_v& dims)
{
    string_v::iterator it = dims.begin();
    while (it != dims.end()) {
        if (!cdm.hasDimension(*it) || cdm.getDimension(*it).getLength() == 1)
            it = dims.erase(it);
        else
            ++it;
    }
}

std::string findVariableWithDims(const CDM& cdm, const std::string& standard_name, const string_v& dims)
{
    string_string_m attrs;
    attrs["standard_name"] = standard_name;
    const string_v vars = cdm.findVariables(attrs, dims);
    if (!vars.empty())
        return vars.front();
    // maybe try removing axes of undefined type ... ?
    return std::string();
}

std::string findVariableWithCS(const CDM& cdm, CoordinateSystem_cp cs, const std::string& standard_name)
{
    string_string_m attrs;
    attrs["standard_name"] = standard_name;
    const string_v vars = cdm.findVariables(attrs, string_v());
    string_v::const_iterator it = std::find_if(vars.begin(), vars.end(), CompleteCSForVariable(cs));
    if (it != vars.end())
        return *it;
    // maybe try removing axes of undefined type ... ?
    return std::string();
}

size_t getSliceSize(const SliceBuilder& sb, const std::string& dimName)
{
    size_t start, size;
    sb.getStartAndSize(dimName, start, size);
    return size;
}

ShapeMerger& ShapeMerger::merge(const std::string& varName, bool skipLength1)
{
    if (!varName.empty())
        return merge(rcdm_.getVariable(varName).getShape(), skipLength1);
    else
        return *this;
}

ShapeMerger& ShapeMerger::merge(VerticalConverter_p vc, bool skipLength1)
{
    return merge(vc->getShape(), skipLength1);
}

const std::string& ShapeMerger::axisDim(CoordinateAxis_cp axis) const
{
    return rcdm_.getVariable(axis->getName()).getShape().front();
}

ShapeMerger::shape_t::iterator ShapeMerger::findAxis(CoordinateAxis_cp axis)
{
    if (axis)
        return std::find(shape_.begin(), shape_.end(), axisDim(axis));
    else
        return shape_.end();
}

bool ShapeMerger::equalsAxisDim(const std::string& dim, CoordinateAxis_cp axis) const
{
    return axis && dim == axisDim(axis);
}

ShapeMerger& ShapeMerger::merge(const shape_t& append, bool skipLength1)
{
    LOG4FIMEX(logger, Logger::DEBUG, "merge initial shape:");
    for (size_t i=0; i<shape_.size(); ++i)
        LOG4FIMEX(logger, Logger::DEBUG, "shape_[" << i << "]='" << shape_[i] << "'");
    LOG4FIMEX(logger, Logger::DEBUG, "merge adding shape:");
    for (size_t i=0; i<append.size(); ++i)
        LOG4FIMEX(logger, Logger::DEBUG, "append[" << i << "]='" << append[i] << "'");

    // not sure if it would be better to use cs_ to construct a shape ...
    for (shape_t::const_iterator it = append.begin(); it != append.end(); ++it) {
        if (skipLength1) {
            if (rcdm_.getDimension(*it).getLength() == 1)
                continue;
        }
        if (std::find(shape_.begin(), shape_.end(), *it) != shape_.end()) {
            LOG4FIMEX(logger, Logger::DEBUG, "not appending '" << *it << "', already in shape_");
            continue;
        }

        // need to insert at a good position: x,y,z,others,time, see CF
        shape_t::iterator itI = shape_.end();
        bool placement = false;
        if (equalsAxisDim(*it, cs_->getGeoXAxis())) {
            itI = shape_.begin(); // x axis first
            placement = true;
            LOG4FIMEX(logger, Logger::DEBUG, "inserting x axis '" << *it << "' at start");
        } else if (equalsAxisDim(*it, cs_->getGeoYAxis())) {
            LOG4FIMEX(logger, Logger::DEBUG, "appending y axis '" << *it << "'");
            shape_t::iterator itF;
            if ((itF = findAxis(cs_->getGeoXAxis())) != shape_.end()) {
                itI = ++itF; // y after x
                placement = true;
                LOG4FIMEX(logger, Logger::DEBUG, "appending y axis '" << *it << "' after x");
            }
            if (!placement && (itF = findAxis(cs_->getGeoZAxis())) != shape_.end()) {
                itI = itF; // y before z
                placement = true;
                LOG4FIMEX(logger, Logger::DEBUG, "appending y axis '" << *it << "' before z");
            }
            if (!placement) {
                itI = shape_.begin();
                placement = true;
                LOG4FIMEX(logger, Logger::DEBUG, "inserting y axis '" << *it << "' at start");
            }
        } else if (equalsAxisDim(*it, cs_->getGeoZAxis())) {
            LOG4FIMEX(logger, Logger::DEBUG, "appending z axis '" << *it << "'");
            shape_t::iterator itF;
            if ((itF = findAxis(cs_->getGeoYAxis())) != shape_.end()) {
                itI = ++itF; // z after y
                placement = true;
                LOG4FIMEX(logger, Logger::DEBUG, "appending z axis '" << *it << "' after y");
            }
            if (!placement && (itF = findAxis(cs_->getGeoXAxis())) != shape_.end()) {
                itI = ++itF; // z after x if no y
                placement = true;
                LOG4FIMEX(logger, Logger::DEBUG, "appending z axis '" << *it << "' after x because no y");
            }
            if (!placement) {
                itI = shape_.begin();
                placement = true;
                LOG4FIMEX(logger, Logger::DEBUG, "inserting z axis '" << *it << "' at start");
            }
        } else if (equalsAxisDim(*it, cs_->getTimeAxis())) {
            // itI = shape_.end(); time at end
            placement = true;
            LOG4FIMEX(logger, Logger::DEBUG, "appending t axis '" << *it << "' at end");
        }
        if (!placement) {
            shape_t::iterator itF;
            if ((itF = findAxis(cs_->getTimeAxis())) != shape_.end()) {
                itI = itF; // insert before t
                placement = true;
                LOG4FIMEX(logger, Logger::DEBUG, "inserting axis '" << *it << "' before time axis");
            }
        }
        shape_.insert(itI, *it);
    }
    LOG4FIMEX(logger, Logger::DEBUG, "merge final shape:");
    for (size_t i=0; i<shape_.size(); ++i)
        LOG4FIMEX(logger, Logger::DEBUG, "shape_[" << i << "]='" << shape_[i] << "'");
    return *this;
}

static void copySliceBuilder(SliceBuilder& sbVar, const SliceBuilder& sb)
{
    const string_v& varDimNames = sbVar.getDimensionNames();
    const string_v& orgDimNames = sb.getDimensionNames();
    for (string_v::const_iterator itO = orgDimNames.begin(); itO != orgDimNames.end(); ++itO) {
        LOG4FIMEX(logger, Logger::DEBUG, "*itO='" << *itO << "'");
    }
    for (string_v::const_iterator itD = varDimNames.begin(); itD != varDimNames.end(); ++itD) {
        LOG4FIMEX(logger, Logger::DEBUG, "*itD='" << *itD << "'");
        if (std::find(orgDimNames.begin(), orgDimNames.end(), *itD) != orgDimNames.end()) {
            size_t start, size;
            sb.getStartAndSize(*itD, start, size);
            LOG4FIMEX(logger, Logger::DEBUG, "*itD copy start=" << start << " size=" << size);
            sbVar.setStartAndSize(*itD, start, size);
        } else {
            size_t start, size;
            sbVar.getStartAndSize(*itD, start, size);
            LOG4FIMEX(logger, Logger::DEBUG, "*itD default start=" << start << " size=" << size);
        }
    }
}

SliceBuilder adaptSliceBuilder(const CDM& cdm, const std::string& varName, const SliceBuilder& sb)
{
    SliceBuilder sbVar(cdm, varName);
    copySliceBuilder(sbVar, sb);
    return sbVar;
}

SliceBuilder adaptSliceBuilder(const CDM& cdm, VerticalConverter_p converter, const SliceBuilder& sb)
{
    const string_v& conShape= converter->getShape();
    SliceBuilder sbCon(conShape, getDimSizes(cdm, conShape));
    copySliceBuilder(sbCon, sb);
    return sbCon;
}

DataPtr getSliceData(CDMReader_p reader, const SliceBuilder& sbOrig, const std::string& varName, const std::string& unit)
{
    const SliceBuilder sbVar = adaptSliceBuilder(reader->getCDM(), varName, sbOrig);
    return reader->getScaledDataSliceInUnit(varName, unit, sbVar);
}

shared_array<float> getSliceFloats(CDMReader_p reader, const SliceBuilder& sbOrig, const std::string& varName, const std::string& unit)
{
    return getSliceData(reader, sbOrig, varName, unit)->asFloat();
}

shared_array<double> getSliceDoubles(CDMReader_p reader, const SliceBuilder& sbOrig, const std::string& varName, const std::string& unit)
{
    return getSliceData(reader, sbOrig, varName, unit)->asDouble();
}

std::vector<size_t> getDimSizes(const CDM& cdm, const string_v& dimNames)
{
    size_v dimSizes(dimNames.size(), 1);
    for (size_t i=0; i<dimNames.size(); ++i) {
        if (cdm.hasDimension(dimNames[i]))
            dimSizes[i] = cdm.getDimension(dimNames[i]).getLength();
    }
    return dimSizes;
}

SliceBuilder createSliceBuilder(const CDM& cdm, const string_v& dimNames)
{
    const size_v dimSizes = getDimSizes(cdm, dimNames);
    return SliceBuilder(dimNames, dimSizes);
}

SliceBuilder createSliceBuilder(const CDM& cdm, VerticalConverter_p converter)
{
    return createSliceBuilder(cdm, converter->getShape());
}

void setUnLimDimPos(const CDM& cdm, SliceBuilder& sb, size_t unLimDimPos)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim()) {
        LOG4FIMEX(logger, Logger::DEBUG, "cdm has unlimited dim '" << uld->getName() << "'");
        const string_v dimNames = sb.getDimensionNames();
        if (std::find(dimNames.begin(), dimNames.end(), uld->getName()) != dimNames.end())
            sb.setStartAndSize(uld->getName(), unLimDimPos, 1);
    }
}

size_t setUnLimDimPos(const CDM& cdm, ArrayDims& s, size_t unLimDimPos)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim()) {
        s.set_not_shared(uld->getName());
        return unLimDimPos * s.delta(uld->getName());
    }
    return 0;
}

void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim())
        set_length(uld->getName(), 1, s1);
}

void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim())
        set_length(uld->getName(), 1, s1, s2);
}

void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim())
        set_length(uld->getName(), 1, s1, s2, s3);
}

void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim())
        set_length(uld->getName(), 1, s1, s2, s3, s4);
}

void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4, ArrayDims& s5)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim())
        set_length(uld->getName(), 1, s1, s2, s3, s4, s5);
}

ArrayDims makeArrayDims(const CDM& cdm, const string_v& dimNames)
{
    const size_v dimSizes = getDimSizes(cdm, dimNames);
    return ArrayDims(dimNames, dimSizes);
}

ArrayDims makeArrayDims(const CDM& cdm, const std::string& varName)
{
    return makeArrayDims(cdm, cdm.getVariable(varName).getShape());
}

ArrayDims makeArrayDims(const CDM& cdm, VerticalConverter_p converter)
{
    const string_v dimNames = converter->getShape();
    const size_v dimSizes = getDimSizes(cdm, dimNames);
    return ArrayDims(dimNames, dimSizes);
}

ArrayDims makeArrayDims(const SliceBuilder& sb)
{
    const string_v dimNames = sb.getDimensionNames();
    const size_v& dimSizes = sb.getDimensionSizes();
    return ArrayDims(dimNames, dimSizes);
}

VerticalConverter_p verticalConverter(CoordinateSystem_cp cs, CDMReader_p reader, int verticalType)
{
    if (VerticalTransformation_cp vt = cs->getVerticalTransformation()) {
        if (VerticalConverter_p converter = vt->getConverter(reader, cs, verticalType))
            return converter;
    }
    throw CDMException("no vertical transformation found: " + cs->id());
}

DataPtr verticalData4D(VerticalConverter_p converter, const CDM& cdm, size_t unLimDimPos)
{
    SliceBuilder sb = createSliceBuilder(cdm, converter);
    setUnLimDimPos(cdm, sb, unLimDimPos);
    return converter->getDataSlice(sb);
}

DataPtr verticalData4D(CoordinateSystem_cp cs, CDMReader_p reader, size_t unLimDimPos, int verticalType)
{
    return verticalData4D(verticalConverter(cs, reader, verticalType), reader->getCDM(), unLimDimPos);
}

DataPtr checkSize(DataPtr data, size_t expected, const std::string& what)
{
    if (data && data->size() != expected)
        throw CDMException("data for '" + what + "' have size " + type2string(data->size()) + ", expected " + type2string(expected));
    return data;
}

DataPtr checkData(DataPtr data, size_t expected, const std::string& what)
{
    if (!data.get())
        throw CDMException("no data for '" + what + "'");
    return checkSize(data, expected, what);
}

Var::Var(CDMReader_p reader, const std::string& varName, const std::string& unit, const SliceBuilder& sbOrig)
    : sb(adaptSliceBuilder(reader->getCDM(), varName, sbOrig))
    , dims(makeArrayDims(sb))
    , data(getSliceData(reader, sb, varName, unit))
{
}

Var::Var(CDMReader_p reader, VerticalConverter_p converter, const SliceBuilder& sbOrig)
    : sb(adaptSliceBuilder(reader->getCDM(), converter, sbOrig))
    , dims(makeArrayDims(sb))
    , data(converter->getDataSlice(sb))
{
}

VarFloat::VarFloat(CDMReader_p reader, const std::string& varName, const std::string& unit, const SliceBuilder& sbOrig)
    : Var(reader, varName, unit, sbOrig)
    , values(data ? data->asFloat() : shared_array<float>())
{
}

VarFloat::VarFloat(CDMReader_p reader, VerticalConverter_p converter, const SliceBuilder& sbOrig)
    : Var(reader, converter, sbOrig)
    , values(data ? data->asFloat() : shared_array<float>())
{
}

VarDouble::VarDouble(CDMReader_p reader, const std::string& varName, const std::string& unit, const SliceBuilder& sbOrig)
    : Var(reader, varName, unit, sbOrig)
    , values(data ? data->asDouble() : shared_array<double>())
{
}

VarDouble::VarDouble(CDMReader_p reader, VerticalConverter_p converter, const SliceBuilder& sbOrig)
    : Var(reader, converter, sbOrig)
    , values(data ? data->asDouble() : shared_array<double>())
{
}


} // namespace MetNoFimex
