/*
  Fimex, include/fimex/coordSys/verticalTransform/ToVLevelConverterAdapter.h

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

#ifndef TOVLEVELCONVERTERADAPTER_H
#define TOVLEVELCONVERTERADAPTER_H

#include "ToVLevelConverter.h"

namespace MetNoFimex {

// forward decl
class CDMReader;
class CoordinateSystem;
class SliceBuilder;
class VerticalConverter;

class ToVLevelConverterAdapter : public ToVLevelConverter {
public:
    ToVLevelConverterAdapter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p converter, size_t unLimDimPos);
    virtual std::vector<double> operator()(size_t x, size_t y, size_t t);
    virtual bool isValid(double val, size_t x, size_t y, size_t t);

private:
    SliceBuilder prepareSliceBuilder(size_t x, size_t y, size_t t) const;

private:
    CDMReader_p reader_;
    VerticalConverter_p converter_;
    std::string varGeoX_, varGeoY_, varTime_;
    size_t unlimitedTimePos_;
};

} // namespace MetNoFimex

#endif // TOVLEVELCONVERTERADAPTER_H
