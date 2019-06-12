/*
 * Fimex, OceanSG2.h
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

#ifndef OCEANSG2_H_
#define OCEANSG2_H_

#include "fimex/coordSys/verticalTransform/OceanSG1.h"

namespace MetNoFimex
{
/**
 * @headerfile fimex/coordSys/verticalTransform/OceanSG2.h
 */

/**
 * ocean_s_coordinate_g2 is defined by the same set of parameters, just
 * the conversion function is different.
 */
// for all implementations, see OceanSG1.h or OceanSG1.cc
class OceanSG2 : public OceanSG1
{
public:
    /**
     * Initialize OceanSG2 with formula as defined by https://www.myroms.org/wiki/index.php/Vertical_S-coordinate
     */
    // unconventionally defined in OceanSG1.cc
    OceanSG2(const OceanSGVars& vars);

    /**
     * static NAME constant
     * @return ocean_s_coordinate_g2
     */
    static const std::string NAME() {return "ocean_s_coordinate_g2";}
    /**
     * @return same as static NAME()
     */
    virtual std::string getName() const { return NAME(); }
};

} /* namespace MetNoFimex */
#endif /* OCEANSG2_H_ */
