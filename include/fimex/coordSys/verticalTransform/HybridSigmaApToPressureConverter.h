/*
  Fimex, include/fimex/coordSys/verticalTransform/HybridSigmaApToPressureConverter.h

  Copyright (C) 2019 met.no

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

#ifndef HYBRIDSIGMAAPTOPRESSURECONVERTER_H
#define HYBRIDSIGMAAPTOPRESSURECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Hybrid sigma levels defined by constant ap and b. The surface-pressure
 * varies in (x,y,z). Corresponds to mifi_atmosphere_hybrid_sigma_ap_pressure()
 */
class HybridSigmaApToPressureConverter : public BasicVerticalConverter {
public:
    static VerticalConverter_p createConverter(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& ap, const std::string& b, const std::string& ps,
                                               const std::string& p0 = std::string());

    /**
     * @param ap vector of size n containing the ap-parameters in hPa of sigma-hybrid
     * @param b vector of size n containing the b parameters (dimensionless) of sigma-hybrid
     * @param ps array of size nx*ny*nt containing the surface-pressure
     * @param p0 reference-pressure in hPa
     */
    HybridSigmaApToPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& ap, const std::string& b, const std::string& ps,
                                     const std::string& p0)
        : BasicVerticalConverter(reader, cs)
        , ap_(ap)
        , b_(b)
        , ps_(ps)
        , p0_(p0)
    {
    }

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    const std::string ap_;
    const std::string b_;
    const std::string ps_;
    const std::string p0_;
};

} // namespace MetNoFimex

#endif // HYBRIDSIGMAAPTOPRESSURECONVERTER_H
