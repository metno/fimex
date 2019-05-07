/*
  Fimex, include/fimex/coordSys/verticalTransform/IdentityConverter.h

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

#ifndef IDENTITYCONVERTER_H
#define IDENTITYCONVERTER_H

#include "fimex/coordSys/verticalTransform/VerticalConverter.h"
#include <string>

namespace MetNoFimex {

/**
 * Use geopotential height (in m) and topography to calculate the
 * height above ground (hg = height - topo)
 */
class IdentityConverter : public BasicVerticalConverter {
public:
    static VerticalConverter_p createConverterForStandardName(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& stdName, const std::string& unit);
    static VerticalConverter_p createConverterForVarName(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& varName, const std::string& unit);

    IdentityConverter(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& varName, const std::string& unit)
        : BasicVerticalConverter(reader, cs)
        , varName_(varName)
        , varUnit_(unit)
    {
    }

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    std::string varName_;
    std::string varUnit_;
};

} // namespace MetNoFimex

#endif // IDENTITYCONVERTER_H
