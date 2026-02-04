/*
 * Fimex, AtmosphereHybridHeight.h
 *
 * (C) Copyright 2024-2026, met.no
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

#ifndef ATMOSPHERE_HYBRID_HEIGHT_H_
#define ATMOSPHERE_HYBRID_HEIGHT_H_

#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"

namespace MetNoFimex {

/// Hybrid height vertical coordinate
class AtmosphereHybridHeight : public VerticalTransformation
{
public:
    const std::string a;
    const std::string b;
    const std::string orog;

    /**
     * Initialize AtmosphereHybridHeight with formula
     *   z(n,k,j,i) = a(k) + b(k)*orog(n,j,i)
     *
     * @param a
     * @param b
     * @param orog surface altitude
     */
    AtmosphereHybridHeight(const std::string& a, const std::string& b, const std::string& orog) : a(a), b(b), orog(orog) {}

    /**
     * static NAME constant
     * @return atmosphere_sigma_coordinate
     */
    static const std::string NAME() { return "atmosphere_hybrid_height_coordinate"; }

    /**
     * @return same as static NAME()
     */
    virtual std::string getName() const override { return NAME(); }

    virtual int getPreferredVerticalType() const override { return MIFI_VINT_ALTITUDE; }
    virtual std::string getParameterString() const override { return "a=" + a + ",b=" + b + ",orog=" + orog; }
    virtual bool isComplete() const override { return a != "" && b != "" && orog != ""; }

protected:
    VerticalConverter_p getPressureConverter(CDMReader_p /*reader*/, CoordinateSystem_cp /*cs*/) const override;
    VerticalConverter_p getHeightConverter(CDMReader_p reader, CoordinateSystem_cp cs) const override;
    VerticalConverter_p getAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs) const override;
};

} /* namespace MetNoFimex */

#endif /* ATMOSPHERE_HYBRID_HEIGHT_H_ */
