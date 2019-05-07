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

#include "fimex/CDMReader.h"

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/SliceBuilder.h"
#include "fimex/Type2String.h"
#include "fimex/Units.h"
#include "fimex/UnitsConverter.h"
#include "fimex/mifi_constants.h"

#include <cassert>
#include <functional>
#include <numeric>

namespace MetNoFimex {

CDMReader::CDMReader()
    : cdm_(new CDM())
{
}

CDMReader::~CDMReader() {}

const CDM& CDMReader::getCDM() const
{
    return *cdm_;
}

CDM& CDMReader::getInternalCDM()
{
    return const_cast<CDM&>(getCDM());
}

void CDMReader::setInternalCDM(const CDM& cdm)
{
    *cdm_ = cdm;
}

std::vector<std::size_t> CDMReader::getDims(const std::string& varName)
{
    std::vector<std::size_t> dims;
    const CDMVariable& variable = getCDM().getVariable(varName);
    const std::vector<std::string>& shape = variable.getShape();
    for (std::vector<std::string>::const_iterator dimIt = shape.begin(); dimIt != shape.end(); ++dimIt) {
        const CDMDimension& dim = getCDM().getDimension(*dimIt);
        dims.push_back(dim.getLength());
    }
    return dims;
}

std::vector<std::size_t> CDMReader::getDimsSlice(const std::string& varName)
{
    std::vector<std::size_t> dims;
    const CDMVariable& variable = getCDM().getVariable(varName);
    const std::vector<std::string>& shape = variable.getShape();
    for (std::vector<std::string>::const_iterator dimIt = shape.begin(); dimIt != shape.end(); ++dimIt) {
        const CDMDimension& dim = getCDM().getDimension(*dimIt);
        if (!dim.isUnlimited()) {
            dims.push_back(dim.getLength());
        }
    }
    return dims;
}

DataPtr CDMReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    using namespace std;
    DataPtr retData;
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        retData = variable.getData()->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());
    } else {
        if (cdm_->hasUnlimitedDim(variable)) {
            string unLimDim = cdm_->getUnlimitedDim()->getName();
            vector<string> dimNames = sb.getDimensionNames();
            // get the data along the unlimited dimension and join
            // unlimited dimension must be outer=first dimension!
            size_t unLimDimStart = 0;
            size_t unLimDimSize = 0;
            vector<size_t> dimStart;
            vector<size_t> dimSize;
            vector<size_t> maxDimSize;
            const vector<size_t>& orgDimStart = sb.getDimensionStartPositions();
            const vector<size_t>& orgDimSize = sb.getDimensionSizes();
            const vector<size_t>& orgMaxDimSize = sb.getMaxDimensionSizes();
            size_t unLimSliceSize = 1;
            for (size_t i = 0; i < dimNames.size(); ++i) {
                if (dimNames.at(i) == unLimDim) {
                    unLimDimStart = orgDimStart.at(i);
                    unLimDimSize = orgDimSize.at(i);
                } else {
                    dimStart.push_back(orgDimStart.at(i));
                    dimSize.push_back(orgDimSize.at(i));
                    maxDimSize.push_back(orgMaxDimSize.at(i));
                    unLimSliceSize *= orgDimSize.at(i);
                }
            }
            if (unLimDimSize == 0) {
                return createData(variable.getDataType(), 0);
            }
            // read now each unlimdim-slice
            // slice that slice according to the other dimensions
            // join those slices
            retData = createData(variable.getDataType(), unLimSliceSize*unLimDimSize, cdm_->getFillValue(varName));
            for (size_t i = 0; i < unLimDimSize; ++i) {
                DataPtr unLimDimData = getDataSlice(varName, i+unLimDimStart);
                if (unLimDimData->size() != 0) {
                    unLimDimData = unLimDimData->slice(maxDimSize, dimStart, dimSize);
                    if (unLimDimData->size() != unLimSliceSize) {
                        throw CDMException("size mismatch with unlimited slices for var " + varName+": " + type2string(unLimDimData->size()) + "!=" + type2string(unLimSliceSize));
                    }
                    assert(unLimDimData->size() == unLimSliceSize);
                    retData->setValues(i*unLimSliceSize, *unLimDimData);
                }
            }
        } else {
            retData = getData(varName)->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());
        }
    }
    return retData;
}

DataPtr CDMReader::getData(const std::string& varName)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        return variable.getData()->clone();
    } else {
        if (cdm_->hasUnlimitedDim(variable)) {
            const CDMDimension* udim = cdm_->getUnlimitedDim();
            size_t uDimSize = udim->getLength();
            std::vector<size_t> dims = getDimsSlice(varName);
            size_t sliceSize = accumulate(dims.begin(), dims.end(), 1, std::multiplies<size_t>());
            DataPtr data = createData(variable.getDataType(), uDimSize*sliceSize);
            for (size_t i = 0; i < uDimSize; i++) {
                DataPtr slice = getDataSlice(varName, i);
                data->setValues(i*sliceSize, *slice, 0, sliceSize);
            }
            return data;
        } else {
            return getDataSlice(varName, 0);
        }
    }
}

void CDMReader::getScaleAndOffsetOf(const std::string& varName, double& scale, double& offset) const
{
    scale = cdm_->getScaleFactor(varName);
    offset = cdm_->getAddOffset(varName);
}

// handle data scaling using add_offset, scale_factor and _FillValue from the varName variable
DataPtr CDMReader::scaleDataOf(const std::string& varName, DataPtr data, double unitScale, double unitOffset)
{
    // retrieve scale and offset
    double scale, offset;
    getScaleAndOffsetOf(varName, scale, offset);

    // v = scale*x + offset
    // v(newUnit) = unitScale*v + unitOffset;
    double totalScale = scale * unitScale;
    double totalOffset = unitScale*offset + unitOffset;

    // fillValue
    double inFillValue = cdm_->getFillValue(varName);

    return data->convertDataType(inFillValue, totalScale, totalOffset, CDM_DOUBLE, MIFI_UNDEFINED_D,1,0);
}

DataPtr CDMReader::scaleDataOf(const std::string& varName, DataPtr data, UnitsConverter_p uc)
{
    // retrieve scale and offset
    double scale, offset;
    getScaleAndOffsetOf(varName, scale, offset);
    // fillValue
    double inFillValue = cdm_->getFillValue(varName);
    return data->convertDataType(inFillValue, scale, offset, uc, CDM_DOUBLE, MIFI_UNDEFINED_D,1,0);
}

DataPtr CDMReader::scaleDataToUnitOf(const std::string& varName, DataPtr data, const std::string& newUnit)
{
    std::string myUnit = cdm_->getUnits(varName);
    UnitsConverter_p uc = Units().getConverter(myUnit, newUnit);
    if (uc->isLinear()) {
        // performance optimization, >3x faster
        double unitOffset = 0.;
        double unitScale = 1.;
        uc->getScaleOffset(unitScale, unitOffset);
        return scaleDataOf(varName, data, unitScale, unitOffset);
    }
    return scaleDataOf(varName, data, uc);
}

DataPtr CDMReader::getScaledDataSlice(const std::string& varName, size_t unLimDimPos)
{
    return scaleDataOf(varName, getDataSlice(varName, unLimDimPos));
}

DataPtr CDMReader::getScaledDataSliceInUnit(const std::string& varName, const std::string& unit, size_t unLimDimPos)
{
    return scaleDataToUnitOf(varName, getDataSlice(varName, unLimDimPos), unit);
}

DataPtr CDMReader::getScaledDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    return scaleDataOf(varName, getDataSlice(varName, sb));
}

DataPtr CDMReader::getScaledDataSliceInUnit(const std::string& varName, const std::string& unit, const SliceBuilder& sb)
{
    return scaleDataToUnitOf(varName, getDataSlice(varName, sb), unit);
}

DataPtr CDMReader::getScaledData(const std::string& varName)
{
    return scaleDataOf(varName, getData(varName));
}

DataPtr CDMReader::getScaledDataInUnit(const std::string& varName, const std::string& unit)
{
    return scaleDataToUnitOf(varName, getData(varName), unit);
}

DataPtr CDMReader::getDataSliceFromMemory(const CDMVariable& variable, size_t unLimDimPos)
{
    if (DataPtr data = variable.getData()) {
        if (data->size() == 0) // not possible to slice
            return data;
        if (cdm_->hasUnlimitedDim(variable)) {
            // cut out the unlimited dim data
            std::vector<size_t> dims = getDimsSlice(variable.getName());
            size_t sliceSize = accumulate(dims.begin(), dims.end(), 1, std::multiplies<size_t>());
            return createDataSlice(variable.getDataType(), *data, unLimDimPos * sliceSize, sliceSize);
        } else {
            return data->clone();
        }
    } else {
        return DataPtr();
    }
}

DataPtr CDMReader::getDataSliceFromMemory(const CDMVariable& variable, const SliceBuilder& sb)
{
    if (DataPtr data = variable.getData()) {
        if (data->size() == 0)
            return data;
        return data->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());
    } else {
        return DataPtr();
    }
}

} // namespace MetNoFimex
