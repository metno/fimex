/*
 * Fimex
 *
 * (C) Copyright 2013, met.no
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

#include "fimex/CDMReaderWriter.h"
#include "fimex/CDM.h"
#include "fimex/mifi_constants.h"
#include "fimex/Data.h"
#include "fimex/Units.h"

namespace MetNoFimex {

CDMReaderWriter::CDMReaderWriter()
{
}

CDMReaderWriter::~CDMReaderWriter()
{
}

void CDMReaderWriter::putScaledDataSlice(const std::string& varName, size_t unLimDimPos, const DataPtr data)
{
    putDataSlice(varName, unLimDimPos, unscaleDataOf(varName, data));
}

void CDMReaderWriter::putScaledDataSliceInUnit(const std::string& varName, const std::string& unit, size_t unLimDimPos, const DataPtr data)
{
    putDataSlice(varName, unLimDimPos, unscaleDataFromUnitOf(varName, data, unit));
}

void CDMReaderWriter::putScaledDataSlice(const std::string& varName, const SliceBuilder& sb, const DataPtr data)
{
    putDataSlice(varName, sb, unscaleDataOf(varName, data));
}

void CDMReaderWriter::putScaledDataSliceInUnit(const std::string& varName, const std::string& unit, const SliceBuilder& sb, const DataPtr data)
{
    putDataSlice(varName, sb, unscaleDataFromUnitOf(varName, data, unit));
}

// handle data scaling using add_offset, scale_factor and _FillValue from the varName variable
DataPtr CDMReaderWriter::unscaleDataOf(const std::string& varName, DataPtr data, double unitScale, double unitOffset)
{
    // retrieve scale and offset
    double scale, offset;
    getScaleAndOffsetOf(varName, scale, offset);

    const double totalScale = scale * unitScale;
    const double totalOffset = unitScale*offset + unitOffset;

    // v_cdm = scale*x + offset
    // v_data = unitScale*v_cdm + unitOffset = unitScale*(scale*x + offset) + unitOffset
    //        = totalScale * x + totalOffset
    // => x = (v_data - totalOffset) / totalScale
    //      = (1/totalScale) * v_data + (-1*totalOffset/totalScale)

    const double totalUnscale = 1/totalScale; // ignore division-by-zero
    const double totalUnoffset = -(totalOffset / totalScale);

    // fillValue
    const double inFillValue = cdm_->getFillValue(varName);

    return data->convertDataType(inFillValue, totalUnscale, totalUnoffset, CDM_DOUBLE, MIFI_UNDEFINED_D, 1, 0);
}

DataPtr CDMReaderWriter::unscaleDataFromUnitOf(const std::string& varName, DataPtr data, const std::string& newUnit)
{
    const std::string myUnit = cdm_->getUnits(varName);
    double unitOffset = 0.;
    double unitScale = 1.;
    if (newUnit != myUnit) {
        Units u;
        u.convert(myUnit, newUnit, unitScale, unitOffset);
    }
    return unscaleDataOf(varName, data, unitScale, unitOffset);
}

} // namespace MetnoFimex
