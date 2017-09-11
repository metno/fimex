/*
 * Fimex, HybridSigmaPressure1.h
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
 *
 *  Created on: Aug 6, 2013
 *      Author: heikok
 */

#ifndef HYBRIDSIGMAPRESSURE1_H_
#define HYBRIDSIGMAPRESSURE1_H_

#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"

namespace MetNoFimex
{
/**
 * @headerfile fimex/coordSys/verticalTransform/HybridSigmaPressure1.h
 */
/// Hybrid sigma pressure vertical coordinate, expressed with ap and b
class HybridSigmaPressure1 : public VerticalTransformation
{
public:
    const std::string ap;
    const std::string b;
    const std::string ps;
    const std::string p0;

    /**
     * Initialize HybridSigmaPressure with formula
     *   p(k, x,y,t) = ap(k) + b(k)*ps(x,y,t)
     *
     * @param ap parameter in pressure unit
     * @param b  dimensionless parameter
     * @param ps surface pressure variable name
     * @param p0 optional p0 base-pressure, not needed for transformations
     */
    HybridSigmaPressure1(const std::string& ap, const std::string& b, const std::string& ps, const std::string& p0 = "")
        : ap(ap), b(b), ps(ps), p0(p0) {}

    /**
     * static NAME constant
     * @return atmosphere_hybrid_sigma_pressure_coordinate_1
     */
    static const std::string NAME() { return "atmosphere_hybrid_sigma_pressure_coordinate_1";}

    /**
     * @return same as static NAME()
     */
    virtual std::string getName() const { return NAME(); }
    virtual int getPreferredVerticalType() const { return MIFI_VINT_PRESSURE; }
    virtual std::string getParameterString() const { return "ap="+ap+",b="+b+",ps="+ps+",p0="+p0; }
    virtual bool isComplete() const {return !ap.empty() && !b.empty() && !ps.empty();}

protected:
    VerticalConverterPtr getPressureConverter(CDMReader_p reader, CoordSysPtr cs) const;
};

} /* namespace MetNoFimex */

#endif /* HYBRIDSIGMAPRESSURE1_H_ */
