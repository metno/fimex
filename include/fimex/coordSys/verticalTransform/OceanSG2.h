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
     * Initialize OceanSG2 with formular as defined by https://www.myroms.org/wiki/index.php/Vertical_S-coordinate
     *
     * @param s fractional vertical stretching coordinate sigma(k)
     * @param C free surface C(x,y,t)
     * @param depth unperturbed ocean-depth depth(x,y)
     * @param depth_c critical depth, usually min(depth(x,y))
     * @param eta optional time-varying free surface eta(x,y,z) (often also called zeta)
     */
    // unconventionally defined in OceanSG1.cc
    OceanSG2(std::string s, std::string C, std::string depth, std::string depth_c, std::string eta = "");
    virtual ~OceanSG2() {}
    /**
     * @return ocean_s_coordinate_g2
     */
    virtual std::string getName() const { return "ocean_s_coordinate_g2"; }
};

} /* namespace MetNoFimex */
#endif /* OCEANSG2_H_ */
