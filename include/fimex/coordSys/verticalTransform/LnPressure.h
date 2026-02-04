/*
 * Fimex, LnPressure.h
 *
 * (C) Copyright 2013-2026, met.no
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
 *
 *  Created on: Aug 6, 2013
 *      Author: heikok
 */

#ifndef LNPRESSURE_H_
#define LNPRESSURE_H_

#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"

namespace MetNoFimex
{
/**
 * @headerfile fimex/coordSys/verticalTransform/LnPressure.h
 */
/// Hybrid sigma pressure vertical coordinate, expressed with ap andb
class LnPressure : public VerticalTransformation
{
public:
    const std::string lev;
    const std::string p0;
    /**
     * Initialize LnPressure with formula (for all x, y, t)
     *   p(k) = p0 * exp(-lev(k))
     *
     * @param lev dimensionless parameter
     * @param p0 p0 base-pressure
     */
    LnPressure(const std::string& lev, const std::string& p0) : lev(lev), p0(p0) {}
    virtual ~LnPressure() {}
    /**
     * static NAME constant
     * @return atmosphere_ln_pressure_coordinate
     */
    static const std::string NAME() {return "atmosphere_ln_pressure_coordinate";}
    /**
     * @return same as static NAME()
     */
    virtual std::string getName() const { return NAME(); }
    virtual int getPreferredVerticalType() const { return MIFI_VINT_PRESSURE; }
    virtual std::string getParameterString() const { return "lev="+lev+",p0="+p0; }
    virtual bool isComplete() const {return lev != "" && p0 != "";}

protected:
    VerticalConverter_p getPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;
};

} /* namespace MetNoFimex */
#endif /* LNPRESSURE_H_ */
