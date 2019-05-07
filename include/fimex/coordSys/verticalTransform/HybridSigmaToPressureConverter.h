/*
  Fimex, include/fimex/coordSys/verticalTransform/HybridSigmaToPressureConverter.h

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

#ifndef HYBRIDSIGMATOPRESSURECONVERTER_H
#define HYBRIDSIGMATOPRESSURECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Hybrid sigma levels defined by constant a and b. The surface-pressure
 * varies in (x,y,z). Corresponds to mifi_atmosphere_hybrid_sigma_pressure()
 */
class HybridSigmaToPressureConverter : public BasicVerticalConverter {
public:
    static VerticalConverter_p createConverter(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& a, const std::string& b, const std::string& ps,
                                               const std::string& p0);

    /**
     * @param a vector of size n containing the ap-parameters (dimensionless) of sigma-hybrid
     * @param b vector of size n containing the b parameters (dimensionless) of sigma-hybrid
     * @param ps surface pressure variable name
     * @param p0 reference-pressure in hPa
     */
    HybridSigmaToPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& a, const std::string& b, const std::string& ps,
                                   const std::string& p0)
        : BasicVerticalConverter(reader, cs)
        , a_(a)
        , b_(b)
        , ps_(ps)
        , p0_(p0)
    {
    }

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    const std::string a_;
    const std::string b_;
    const std::string ps_;
    const std::string p0_;
};

} // namespace MetNoFimex

#endif // HYBRIDSIGMATOPRESSURECONVERTER_H
