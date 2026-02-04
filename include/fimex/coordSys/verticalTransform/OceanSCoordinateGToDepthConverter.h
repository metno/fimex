/*
  Fimex, include/fimex/coordSys/verticalTransform/OceanSCoordinateGToDepthConverter.h

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

#ifndef OCEANSCOORDINATEGTODEPTHCONVERTER_H
#define OCEANSCOORDINATEGTODEPTHCONVERTER_H

#include "fimex/coordSys/verticalTransform/VerticalConverter.h"
#include "fimex/IndexedData.h"

namespace MetNoFimex
{
/**
 * @headerfile fimex/coordSys/verticalTransform/OceanSCoordinateGToDepthConverter.h
 */

struct OceanSGVars {
    //! k-dependent sigma coordinate
    std::string s;
    //! k-dependent stretching function
    std::string C;
    //! ocean-depth, might be time-varying for sediment applications
    std::string depth;
    //! critical depth (~ min(h(x,y)))
    std::string depth_c;
    //! time-varying free surface, might be 0 / empty
    std::string eta;

    /**
     * @param s k-dependent sigma coordinate
     * @param C k-dependent stretching function
     * @param depth ocean-depth, might be time-varying for sediment applications
     * @param depth_c critical depth (~ min(h(x,y)))
     * @param eta time-varying free surface, might be 0
     */
    OceanSGVars(const std::string& s, const std::string& C, const std::string& depth,
                     const std::string& depth_c, const std::string& eta)
        : s(s), C(C), depth(depth), depth_c(depth_c), eta(eta) { }

    bool isComplete() const
        { return s != "" && C != "" && depth != "" && depth_c != ""; }
};



/**
 * Use ocean_s_coordinate_g1 or g2 to calculate depth
 */
class OceanSCoordinateGToDepthConverter : public BasicVerticalConverter {
public:
    typedef int (*heightconversion_t)(size_t, double, double, double, const double*, const double*, double*);

    /**
     * @param func either mifi_ocean_s_g1_z() or mifi_ocean_s_g2_z()
     */
    OceanSCoordinateGToDepthConverter(CDMReader_p reader, CoordinateSystem_cp cs, const OceanSGVars& vars, heightconversion_t func);

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

    std::vector<std::string> getValidityMaxShape() const;
    DataPtr getValidityMax(const SliceBuilder& sb) const;
    std::vector<std::string> getValidityMinShape() const;
    DataPtr getValidityMin(const SliceBuilder& sb) const;

private:
    const OceanSGVars vars_;
    heightconversion_t func_;
};

} /* namespace MetNoFimex */

#endif // OCEANSCOORDINATEGTODEPTHCONVERTER_H

