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

#ifndef FIMEX_DATAUTILS_H_
#define FIMEX_DATAUTILS_H_

#include "fimex/MathUtils.h"
#include "fimex/UnitsConverter.h"

#include "fimex/CDMDataType.h"
#include "fimex/DataDecl.h"

namespace MetNoFimex {

/**
 * Scale a value using fill, offset and scale
 */
template <typename IN, typename OUT>
class ScaleValue
{
private:
    IN oldFill_;
    double oldScaleNewScaleInv_;
    double oldOffsetMinusNewOffsetNewScaleInv_;
    OUT newFill_;

public:
    ScaleValue(double oldFill, double oldScale, double oldOffset, double newFill, double newScale, double newOffset)
        : oldFill_(static_cast<IN>(oldFill))
        , oldScaleNewScaleInv_(oldScale / newScale)
        , oldOffsetMinusNewOffsetNewScaleInv_((oldOffset - newOffset) / newScale)
        , newFill_(static_cast<OUT>(newFill))
    {
    }
    OUT operator()(const IN& in) const
    {
        return (in == oldFill_ || mifi_isnan<IN>(in)) ? newFill_ : data_caster<OUT, double>()(oldScaleNewScaleInv_ * in + oldOffsetMinusNewOffsetNewScaleInv_);
        //(((oldScale_*in + oldOffset_)-newOffset_)/newScale_);
        // => ((oldScale_*in + oldOffsetMinusNewOffset_)*newScaleInv_);
        // => oldScaleNewScale_ * in + oldOffsetMinusNewOffsetNewScale_
    }
};

/**
 * Scale a value using fill, offset and scale, and a units-converter
 */
template <typename IN, typename OUT>
class ScaleValueUnits
{
private:
    IN oldFill_;
    double oldScale_;
    double oldOffset_;
    UnitsConverter_p uconv_;
    OUT newFill_;
    double newScaleInv_;
    double newOffset_;

public:
    ScaleValueUnits(double oldFill, double oldScale, double oldOffset, UnitsConverter_p uconv, double newFill, double newScale, double newOffset)
        : oldFill_(static_cast<IN>(oldFill))
        , oldScale_(oldScale)
        , oldOffset_(oldOffset)
        , uconv_(uconv)
        , newFill_(static_cast<OUT>(newFill))
        , newScaleInv_(1 / newScale)
        , newOffset_(newOffset)
    {
    }
    OUT operator()(const IN& in) const
    {
        return (in == oldFill_ || mifi_isnan<IN>(in)) ? newFill_
                                                      : data_caster<OUT, double>()((uconv_->convert(oldScale_ * in + oldOffset_) - newOffset_) * newScaleInv_);
    }
};

/**
 * Change the missing value
 */
template <typename IN, typename OUT>
class ChangeMissingValue
{
private:
    IN oldFill_;
    OUT newFill_;

public:
    ChangeMissingValue(double oldFill, double newFill)
        : oldFill_(static_cast<IN>(oldFill))
        , newFill_(static_cast<OUT>(newFill))
    {
    }
    OUT operator()(const IN& in) const { return (in == oldFill_ || mifi_isnan(in)) ? newFill_ : static_cast<OUT>(in); }
};

DataPtr initDataByArray(CDMDataType datatype, const std::vector<std::string>& values);

} // namespace MetNoFimex

#endif /*FIMEX_DATAUTILS_H_*/
