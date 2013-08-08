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

/**
 * @headerfile "fimex/coordSys/verticalTransform/OceanSG1.h"
 */

namespace MetNoFimex
{
/**
 * ocean_s_coordinate_g2 is defined by the same set of parameters, just
 * the conversion function is different.
 */
class OceanSG1 : public VerticalTransformation
{
protected:
    int (*heightConversionFunction)(size_t n, double h, double h_c, double zeta, const double* sigma, const double* C, double* z);
public:
    const std::string s;
    const std::string C;
    const std::string depth;
    const std::string depth_c;
    /* optional */
    const std::string eta;
    /**
     * Initialize OceanSG1 with formular as defined by https://www.myroms.org/wiki/index.php/Vertical_S-coordinate
     *
     * @param s fractional vertical stretching coordinate sigma(k)
     * @param C free surface C(x,y,t)
     * @param depth unperturbed ocean-depth depth(x,y)
     * @param depth_c critical depth, usually min(depth(x,y))
     * @param eta time-varying free surface eta(x,y,z) (often also called zeta)
     */
    OceanSG1(std::string s, std::string C, std::string depth, std::string depth_c, std::string eta = "");
    virtual ~OceanSG1() {}
    /**
     * @return ocean_s_coordinate_g1
     */
    virtual std::string getName() const { return "ocean_s_coordinate_g1"; }
    virtual std::string getParamterString() const { return "s="+s+",C="+C+",depth="+depth+",depth_c="+depth_c+",eta="+eta; }
    virtual bool isComplete() const {return s != "" && C != "" && depth != "" && depth_c != "";}
protected:
    virtual boost::shared_ptr<ToVLevelConverter> getPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nt) const;
    virtual boost::shared_ptr<ToVLevelConverter> getHeightConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const;
};

} /* namespace MetNoFimex */
#endif /* OCEANSG1_H_ */
