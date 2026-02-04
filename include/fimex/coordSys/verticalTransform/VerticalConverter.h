/*
  Fimex, include/fimex/coordSys/verticalTransform/VerticalConverter.h

  Copyright (C) 2019-2026 met.no

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

#ifndef VERTICALCONVERTER_H
#define VERTICALCONVERTER_H

#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"
#include "fimex/CDMReaderDecl.h"
#include "fimex/DataDecl.h"

#include <vector>

namespace MetNoFimex {

// forward decl.

class CoordinateSystem;
class SliceBuilder;

class VerticalConverter
{
public:
    VerticalConverter() = default;
    VerticalConverter(const VerticalConverter&) = delete;
    VerticalConverter& operator=(const VerticalConverter&) = delete;

    virtual ~VerticalConverter();

    virtual std::vector<std::string> getShape() const = 0;

    /** Return data of the converted vertical field. */
    virtual DataPtr getDataSlice(const SliceBuilder& sb) const = 0;

    /** The VLevelConverter usually knows about validity of vertical values at a certain position.
     *  This function returns the maximum valid value, e.g. surface pressure for pressure.
     *  If null is returned, everyting is valid.
     */
    virtual DataPtr getValidityMax(const SliceBuilder& sb) const = 0;

    /** The VLevelConverter usually knows about validity of vertical values at a certain position.
     *  This function returns the minimum valid value, e.g. 0 for pressure.
     *  If null is returned, everyting is valid.
     */
    virtual DataPtr getValidityMin(const SliceBuilder& sb) const = 0;

    /** Return the shape of the validity data.
     */
    virtual std::vector<std::string> getValidityMaxShape() const = 0;
    virtual std::vector<std::string> getValidityMinShape() const = 0;
};

typedef std::shared_ptr<VerticalConverter> VerticalConverter_p;

class BasicVerticalConverter : public VerticalConverter {
public:
    BasicVerticalConverter(CDMReader_p reader, CoordinateSystem_cp cs)
        : reader_(reader)
        , cs_(cs)
    {
    }

    DataPtr getValidityMax(const SliceBuilder& sb) const;
    DataPtr getValidityMin(const SliceBuilder& sb) const;
    std::vector<std::string> getValidityMaxShape() const;
    std::vector<std::string> getValidityMinShape() const;

protected:
    CDMReader_p reader_;
    CoordinateSystem_cp cs_;
};

/**
 * Constant v-levels in time and space, no changes needed
 */
class IdentityVerticalConverter : public VerticalConverter {
    const std::vector<double> vlevel_;
public:
    /**
     * @param vlevel The constant level.
     */
    IdentityVerticalConverter(const std::vector<double>& vlevel) : vlevel_(vlevel) {}
    virtual std::vector<double> operator()(size_t x, size_t y, size_t t);
};

} /* namespace MetNoFimex */

#endif // VERTICALCONVERTER_H
