/*
 * Fimex, ToVLevelConverter.cc
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Aug 12, 2011
 *      Author: Heiko Klein
 */

#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"
#include <map>
#include <functional>
#include <algorithm>
#include <boost/regex.hpp>
#include "fimex/CDMReader.h"
#include "fimex/Data.h"
#include "fimex/CDM.h"
#include "fimex/Logger.h"
#include "fimex/interpolation.h"
#include "fimex/vertical_coordinate_transformations.h"
#include <fimex/coordSys/verticalTransform/VerticalTransformation.h>

namespace MetNoFimex {

using namespace std;

static LoggerPtr logger = getLogger("fimex.ToVLevelConverter");

LnPressureToPressureConverter::LnPressureToPressureConverter(double p0, const vector<double>& lnP) : pres_(lnP.size())
{
    mifi_atmosphere_ln_pressure(lnP.size(), p0, &lnP[0], &pres_[0]);
}

HeightStandardToPressureConverter::HeightStandardToPressureConverter(const vector<double>& h) : pres_(h.size())
{
    mifi_barometric_standard_pressure(h.size(), &h[0], &pres_[0]);
}

const vector<double> SigmaToPressureConverter::operator()(size_t x, size_t y, size_t t) {
    vector<double> p(sigma_.size());
    mifi_atmosphere_sigma_pressure(sigma_.size(), ptop_, ps_[mifi_3d_array_position(x,y,t,nx_,ny_,nt_)], &sigma_[0], &p[0]);
    return p;
}

const vector<double> HybridSigmaApToPressureConverter::operator()(size_t x, size_t y, size_t t) {
    vector<double> p(ap_.size());
    mifi_atmosphere_hybrid_sigma_ap_pressure(ap_.size(), ps_[mifi_3d_array_position(x,y,t,nx_,ny_,nt_)], &ap_[0], &b_[0], &p[0]);
    return p;
}


const vector<double> HybridSigmaToPressureConverter::operator()(size_t x, size_t y, size_t t) {
    vector<double> p(a_.size());
    mifi_atmosphere_hybrid_sigma_pressure(a_.size(), p0_, ps_[mifi_3d_array_position(x,y,t,nx_,ny_,nt_)], &a_[0], &b_[0], &p[0]);
    return p;
}


const vector<double> PressureToStandardHeightConverter::operator()(size_t x, size_t y, size_t t) {
    const vector<double> p((*presConv_)(x,y,t));
    vector<double> h(p.size());
    mifi_barometric_standard_height(p.size(), &p[0], &h[0]);
    return h;
}

const vector<double> GeopotentialToHeightConverter::operator()(size_t x, size_t y, size_t t) {
    vector<double> h(nz_);
    for (size_t z = 0; z < nz_; z++) {
        float hg = geopot_[((t*nz_ + z)*ny_ + y)*nx_ +x];
        h.at(z) =  static_cast<double>(hg - alti_[mifi_3d_array_position(x,y,t,nx_,ny_,nt_)]);
    }
    return h;
}

const vector<double> OceanSCoordinateGToDepthConverter::operator()(size_t x, size_t y, size_t t) {
    vector<double> z(nz_);
    float depth, eta;
    if (timeDependentDepth_) {
        depth = depth_.getDouble(x,y,t);
    } else {
        depth = depth_.getDouble(x,y);
    }
    eta = eta_.getDouble(x,y,t);
    func_(nz_, depth, depth_c_, eta, &s_[0], &C_[0], &z[0]);
    /* z as calculated by formulas is negative down, but we want positive down */
    transform(z.begin(), z.end(), z.begin(), bind1st(multiplies<double>(),-1.));
    return z;
}
bool OceanSCoordinateGToDepthConverter::isValid(double vVal, size_t x, size_t y, size_t t) {
    float depth;
    if (timeDependentDepth_) {
        depth = depth_.getDouble(x,y,t);
    } else {
        depth = depth_.getDouble(x,y);
    }
    return (depth >= vVal);
}


}

