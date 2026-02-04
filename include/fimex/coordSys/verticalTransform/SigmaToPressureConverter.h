/*
  Fimex, include/fimex/coordSys/verticalTransform/SigmaToPressureConverter.h

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

#ifndef SIGMATOPRESSURECONVERTER_H
#define SIGMATOPRESSURECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Sigma levels. The surface-pressure
 * varies in (x,y,z). Corresponds to mifi_atmosphere_sigma_pressure()
 */
class SigmaToPressureConverter : public BasicVerticalConverter {
public:
    /**
     * @param sigma vector of size n containing the sigma parameters (dimensionless) of sigma-hybrid
     * @param ptop top of atmosphere in hPa
     * @param ps array of size nx*ny*nt containing the surface-pressure
     */
    SigmaToPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& sigma, const std::string& ptop, const std::string& ps)
        : BasicVerticalConverter(reader, cs)
        , sigma_(sigma)
        , ptop_(ptop)
        , ps_(ps)
    {
    }

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    const std::string sigma_;
    const std::string ptop_;
    const std::string ps_;
};

} /* namespace MetNoFimex */

#endif // SIGMATOPRESSURECONVERTER_H
