/*
 * Fimex, OceanSG1.h
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

#ifndef OCEANSG1_H_
#define OCEANSG1_H_

#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"
#include "fimex/coordSys/verticalTransform/OceanSCoordinateGToDepthConverter.h"

namespace MetNoFimex
{
/**
 * @headerfile fimex/coordSys/verticalTransform/OceanSG1.h
 */

/**
 * ocean_s_coordinate_g2 is defined by the same set of parameters, just
 * the conversion function is different.
 */
class OceanSG1 : public VerticalTransformation
{
protected:
    OceanSCoordinateGToDepthConverter::heightconversion_t heightConversionFunction;

public:
    const OceanSGVars vars;

    /**
     * Initialize OceanSG1 with formula as defined by https://www.myroms.org/wiki/index.php/Vertical_S-coordinate
     */
    OceanSG1(const OceanSGVars& vars);

    /**
     * static NAME constant
     * @return ocean_s_coordinate_g1
     */
    static const std::string NAME() {return "ocean_s_coordinate_g1";}
    /**
     * @return same as static NAME()
     */
    virtual std::string getName() const { return NAME(); }
    virtual int getPreferredVerticalType() const { return MIFI_VINT_DEPTH; }
    virtual std::string getParameterString() const { return "s="+vars.s+",C="+vars.C+",depth="+vars.depth+",depth_c="+vars.depth_c+",eta="+vars.eta; }
    virtual bool isComplete() const { return vars.isComplete(); }

protected:
    VerticalConverter_p getPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;
    VerticalConverter_p getAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;
    VerticalConverter_p getDepthConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;
};

} /* namespace MetNoFimex */
#endif /* OCEANSG1_H_ */
