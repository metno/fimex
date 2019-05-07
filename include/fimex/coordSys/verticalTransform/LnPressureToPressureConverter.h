/*
  Fimex, include/fimex/coordSys/verticalTransform/LnPressureToPressureConverter.h

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

#ifndef LNPRESSURETOPRESSURECONVERTER_H
#define LNPRESSURETOPRESSURECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * @headerfile fimex/coordSys/verticalTransform/LnPressureToPressureConverter.h
 */

/**
 * Constant pressure levels in time and space, given as ln(p)
 */
class LnPressureToPressureConverter : public BasicVerticalConverter {
public:

    /**
     * @param p0 reference-pressure in hPa
     * @param lnP The constant pressure levels given as ln(P/P0)
     */
    LnPressureToPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& p0, const std::string& lnP)
        : BasicVerticalConverter(reader, cs)
        , p0_(p0)
        , lnP_(lnP)
    {
    }

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    const std::string p0_;
    const std::string lnP_;
};

} // namespace MetNoFimex

#endif // LNPRESSURETOPRESSURECONVERTER_H
