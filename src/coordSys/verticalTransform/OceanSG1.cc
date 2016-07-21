/*
 * Fimex, OceanSG1.cc
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
 *  Created on: Aug 8, 2013
 *      Author: heikok
 */

#include "fimex/coordSys/verticalTransform/OceanSG1.h"
#include "fimex/coordSys/verticalTransform/OceanSG2.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/Data.h"
#include "fimex/Utils.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"
#include "fimex/vertical_coordinate_transformations.h"


namespace MetNoFimex {

OceanSG2::OceanSG2(const std::string& s, const std::string& C, const std::string& depth, const std::string& depth_c, const std::string& eta)
        : OceanSG1(s, C, depth, depth_c, eta)
{
    heightConversionFunction = mifi_ocean_s_g2_z;
}


OceanSG1::OceanSG1(const std::string& s, const std::string& C, const std::string& depth, const std::string& depth_c, const std::string& eta)
    : heightConversionFunction(mifi_ocean_s_g1_z),
      s(s), C(C), depth(depth), depth_c(depth_c), eta(eta)
{}


boost::shared_ptr<ToVLevelConverter> OceanSG1::getPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nt) const
{
    throw CDMException("no pressure converter for " + getName());
}

boost::shared_ptr<ToVLevelConverter> OceanSG1::getAltitudeConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const
{
    // default eta: single value, 0
    vector<double> sVec = getDataSliceInUnit(reader, s, "", unLimDimPos); // size k
    vector<double> CVec = getDataSliceInUnit(reader, C, "", unLimDimPos); // size k
    vector<double> depth_cVec = getDataSliceInUnit(reader, depth_c, "m", unLimDimPos); // size 1
    DataPtr depthD = reader->getScaledDataSliceInUnit(depth, "m", unLimDimPos);
    IndexedData depthDI = IndexedData(depthD, reader->getDimsSlice(depth));
    size_t dSize = (depthDI.idx().getDims().size() == 3) ? (nx*ny*nt) : (nx*ny);
    if (depthD->size() < dSize) {
        throw CDMException("unexpected size of depth " + depth + "(" + type2string(unLimDimPos) +
                "), should be " + type2string(nx * ny * nt) + " != " + type2string(depthD->size()));
            // I allow depthD to be larger than dSize for staggered grids (grids with +-1 cell)
    }
    IndexedData etaDI(createData(CDM_DOUBLE, 1, 0.), vector<size_t>(1,1));
    if (eta != "") {
        DataPtr etaD = reader->getScaledDataSliceInUnit(eta, "m", unLimDimPos);
        if (etaD->size() < (nx * ny * nt)) {
            throw CDMException("unexpected size of eta " + eta + "(" + type2string(unLimDimPos) +
                    "), should be " + type2string(nx * ny * nt) + " != " + type2string(etaD->size()));
        }
        etaDI = IndexedData(etaD, reader->getDimsSlice(eta));
    }
    return boost::shared_ptr<ToVLevelConverter>(new OceanSCoordinateGToDepthConverter(sVec, CVec, depth_cVec[0], etaDI, depthDI,
            nx, ny, nz, nt, heightConversionFunction));
}

}
