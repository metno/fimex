/*
 * Fimex, ToPressureConverter.h
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
 *  Created on: Aug 9, 2011
 *      Author: Heiko Klein
 */

#ifndef TOPRESSURECONVERTER_H_
#define TOPRESSURECONVERTER_H_

#include <vector>
#include <boost/shared_array.hpp>
#include <boost/noncopyable.hpp>

// TODO: implement those classe for all vertical coordinate transformations
namespace MetNoFimex
{

using namespace std;

/**
 * Interface class for a functor describing how to get the
 * pressure levels at a certain point in time and space.
 */
class ToPressureConverter : boost::noncopyable {
public:
    virtual ~ToPressureConverter() {}
    /**
     * functor-interface to get all pressure-levels at (x,y,t)
     *
     * @param x
     * @param y
     * @param t
     * @return pressure-levels in hPa at position (x,y,t)
     */
    virtual const vector<double> operator()(size_t x, size_t y, size_t t) = 0;
};

/**
 * Constant pressure levels in time and space.
 */
class PressureToPressureConverter : public ToPressureConverter {
    const vector<double> pres_;
public:
    /**
     * @param pressure The constant pressure levels in hPa
     */
    PressureToPressureConverter(const vector<double>& pressure) : pres_(pressure.begin(), pressure.end()) {}
    virtual const vector<double> operator()(size_t x, size_t y, size_t t) {return pres_;}
};

/**
 * Constant pressure levels in time and space, given as ln(p)
 */
class LnPressureToPressureConverter : public ToPressureConverter {
    vector<double> pres_;
public:
    /**
     * @param p0 reference-pressure in hPa
     * @param lnP The constant pressure levels given as ln(P/P0)
     */
    LnPressureToPressureConverter(double p0, const vector<double>& lnP) : pres_(lnP.size()) {mifi_atmosphere_ln_pressure(lnP.size(), p0, &lnP[0], &pres_[0]);}
    virtual const vector<double> operator()(size_t x, size_t y, size_t t) {return pres_;}
};

/**
 * using the international standard atmosphere to convert height to pressure
 */
class HeightStandardToPressureConverter : public ToPressureConverter {
    vector<double> pres_;
public:
    /**
     * @param h given in m
     */
    HeightStandardToPressureConverter(const vector<double>& h) : pres_(h.size()) {mifi_barometric_standard_pressure(h.size(), &h[0], &pres_[0]);}
    virtual const vector<double> operator()(size_t x, size_t y, size_t t) {return pres_;}
};

/**
 * Sigma levels. The surface-pressure
 * varies in (x,y,z). Corresponds to mifi_atmosphere_sigma_pressure()
 */
class SigmaToPressureConverter : public ToPressureConverter {
    const vector<double> sigma_;
    double ptop_;
    const boost::shared_array<double> ps_;
    size_t nx_;
    size_t ny_;
    size_t nt_;
public:
    /**
     * @param ap vector of size n containing the ap-parameters in hPa of sigma-hybrid
     * @param b vector of size n containing the b parameters (dimensionless) of sigma-hybrid
     * @param ps array of size nx*ny*nt containing the surface-pressure
     * @param nx x-size of ps
     * @param ny y-size of ps
     * @param nt t-size of ps
     */
    SigmaToPressureConverter(const vector<double>& sigma, double ptop, const boost::shared_array<double> ps, size_t nx, size_t ny, size_t nt)
    : sigma_(sigma.begin(), sigma.end()), ptop_(ptop), ps_(ps), nx_(nx), ny_(ny), nt_(nt) {}
    virtual const vector<double> operator()(size_t x, size_t y, size_t t) {
        vector<double> p(sigma_.size());
        mifi_atmosphere_sigma_pressure(sigma_.size(), ptop_, ps_[mifi_3d_array_position(x,y,t,nx_,ny_,nt_)], &sigma_[0], &p[0]);
        return p;
    }
};


/**
 * Hybrid sigma levels defined by constant ap and b. The surface-pressure
 * varies in (x,y,z). Corresponds to mifi_atmosphere_hybrid_sigma_ap_pressure()
 */
class HybridSigmaApToPressureConverter : public ToPressureConverter {
    const vector<double> ap_;
    const vector<double> b_;
    const boost::shared_array<double> ps_;
    size_t nx_;
    size_t ny_;
    size_t nt_;
public:
    /**
     * @param ap vector of size n containing the ap-parameters in hPa of sigma-hybrid
     * @param b vector of size n containing the b parameters (dimensionless) of sigma-hybrid
     * @param ps array of size nx*ny*nt containing the surface-pressure
     * @param nx x-size of ps
     * @param ny y-size of ps
     * @param nt t-size of ps
     */
    HybridSigmaApToPressureConverter(const vector<double>& ap, const vector<double>& b, const boost::shared_array<double> ps, size_t nx, size_t ny, size_t nt)
    : ap_(ap.begin(), ap.end()), b_(b.begin(), b.end()), ps_(ps), nx_(nx), ny_(ny), nt_(nt) {}
    virtual const vector<double> operator()(size_t x, size_t y, size_t t) {
        vector<double> p(ap_.size());
        mifi_atmosphere_hybrid_sigma_ap_pressure(ap_.size(), ps_[mifi_3d_array_position(x,y,t,nx_,ny_,nt_)], &ap_[0], &b_[0], &p[0]);
        return p;
    }
};

/**
 * Hybrid sigma levels defined by constant a and b. The surface-pressure
 * varies in (x,y,z). Corresponds to mifi_atmosphere_hybrid_sigma_pressure()
 */
class HybridSigmaToPressureConverter : public ToPressureConverter {
    const vector<double> a_;
    const vector<double> b_;
    double p0_;
    const boost::shared_array<double> ps_;
    size_t nx_;
    size_t ny_;
    size_t nt_;
public:
    /**
     * @param a vector of size n containing the ap-parameters (dimensionless) of sigma-hybrid
     * @param b vector of size n containing the b parameters (dimensionless) of sigma-hybrid
     * @param p0 reference-pressure in hPa
     * @param ps array of size nx*ny*nt containing the surface-pressure
     * @param nx x-size of ps
     * @param ny y-size of ps
     * @param nt t-size of ps
     */
    HybridSigmaToPressureConverter(const vector<double>& a, const vector<double>& b, double p0, const boost::shared_array<double> ps, size_t nx, size_t ny, size_t nt)
    : a_(a.begin(), a.end()), b_(b.begin(), b.end()), p0_(p0), ps_(ps), nx_(nx), ny_(ny), nt_(nt) {}
    virtual const vector<double> operator()(size_t x, size_t y, size_t t) {
        vector<double> p(a_.size());
        mifi_atmosphere_hybrid_sigma_pressure(a_.size(), p0_, ps_[mifi_3d_array_position(x,y,t,nx_,ny_,nt_)], &a_[0], &b_[0], &p[0]);
        return p;
    }
};


} // namespace

#endif /* TOPRESSURECONVERTER_H_ */
