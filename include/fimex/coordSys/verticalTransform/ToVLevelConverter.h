/*
 * Fimex, ToVLevelConverter.h
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

#ifndef TO_VLEVEL_CONVERTER_H_
#define TO_VLEVEL_CONVERTER_H_

#include <vector>
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>
#include <boost/noncopyable.hpp>
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/mifi_constants.h"
#include "fimex/IndexedData.h"
#include "fimex/deprecated.h"

namespace MetNoFimex
{
// forward decl.
class CDMReader;

using namespace std;

/**
 * @headerfile fimex/coordSys/verticalTransform/VerticalTransformation.h
 */
/**
 * Interface class for a functor describing how to get the
 * pressure levels at a certain point in time and space.
 */
class ToVLevelConverter : boost::noncopyable {
public:
    virtual ~ToVLevelConverter() {}
    /**
     * functor-interface to get all pressure-levels at (x,y,t)
     *
     * @param x
     * @param y
     * @param t
     * @return pressure-levels in hPa at position (x,y,t)
     */
    virtual vector<double> operator()(size_t x, size_t y, size_t t) = 0;

    //! same as operator()
    inline vector<double> values(size_t x, size_t y, size_t t)
        { return operator()(x, y, t); }

    /**
     * The VLevelConverter usually knows about validity of vertical values at a certain position.
     *
     * @param val value to interpolate to, e.g. height, depth, pressure
     * @param x
     * @param y
     * @param t
     * @return physically correct value
     */
    virtual bool isValid(double val, size_t x, size_t y, size_t t);
};

/**
 * Constant v-levels in time and space, no changes needed
 */
class IdentityToVLevelConverter : public ToVLevelConverter {
    const vector<double> vlevel_;
public:
    /**
     * @param vlevel The constant level.
     */
    IdentityToVLevelConverter(const vector<double>& vlevel) : vlevel_(vlevel) {}
    virtual vector<double> operator()(size_t x, size_t y, size_t t);
};

/**
 * v-levels as 4d field, no calculation needed.
 */
class Identity4DToVLevelConverter : public ToVLevelConverter {
    const boost::shared_array<float> pressure_;
    size_t nx_;
    size_t ny_;
    size_t nz_;
    size_t nt_;
public:
    Identity4DToVLevelConverter(const boost::shared_array<float> pressure, size_t nx, size_t ny, size_t nk, size_t nt)
        : pressure_(pressure), nx_(nx), ny_(ny), nz_(nk), nt_(nt) {}
    virtual vector<double> operator()(size_t x, size_t y, size_t t);
};

/**
 * Constant pressure levels in time and space, given as ln(p)
 */
class LnPressureToPressureConverter : public ToVLevelConverter {
    vector<double> pres_;
public:
    /**
     * @param p0 reference-pressure in hPa
     * @param lnP The constant pressure levels given as ln(P/P0)
     */
    LnPressureToPressureConverter(double p0, const vector<double>& lnP);
    virtual vector<double> operator()(size_t x, size_t y, size_t t);
};

/**
 * using the international standard atmosphere to convert altitude to pressure
 */
class AltitudeStandardToPressureConverter : public ToVLevelConverter {
    vector<double> pres_;
public:
    /**
     * @param h given in m
     */
    AltitudeStandardToPressureConverter(const vector<double>& h);
    virtual vector<double> operator()(size_t x, size_t y, size_t t);
};

/**
 * Sigma levels. The surface-pressure
 * varies in (x,y,z). Corresponds to mifi_atmosphere_sigma_pressure()
 */
class SigmaToPressureConverter : public ToVLevelConverter {
    const vector<double> sigma_;
    double ptop_;
    const boost::shared_array<double> ps_;
    size_t nx_;
    size_t ny_;
    size_t nt_;
public:
    /**
     * @param sigma vector of size n containing the sigma parameters (dimensionless) of sigma-hybrid
     * @param ptop top of atmosphere in hPa
     * @param ps array of size nx*ny*nt containing the surface-pressure
     * @param nx x-size of ps
     * @param ny y-size of ps
     * @param nt t-size of ps
     */
    SigmaToPressureConverter(const vector<double>& sigma, double ptop, const boost::shared_array<double> ps, size_t nx, size_t ny, size_t nt)
    : sigma_(sigma), ptop_(ptop), ps_(ps), nx_(nx), ny_(ny), nt_(nt) {}
    virtual vector<double> operator()(size_t x, size_t y, size_t t);
};


/**
 * Hybrid sigma levels defined by constant ap and b. The surface-pressure
 * varies in (x,y,z). Corresponds to mifi_atmosphere_hybrid_sigma_ap_pressure()
 */
class HybridSigmaApToPressureConverter : public ToVLevelConverter {
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
    : ap_(ap), b_(b), ps_(ps), nx_(nx), ny_(ny), nt_(nt) {}
    virtual vector<double> operator()(size_t x, size_t y, size_t t);
};

/**
 * Hybrid sigma levels defined by constant a and b. The surface-pressure
 * varies in (x,y,z). Corresponds to mifi_atmosphere_hybrid_sigma_pressure()
 */
class HybridSigmaToPressureConverter : public ToVLevelConverter {
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
    : a_(a), b_(b), p0_(p0), ps_(ps), nx_(nx), ny_(ny), nt_(nt) {}
    virtual vector<double> operator()(size_t x, size_t y, size_t t);
};

/**
 * Conversion from pressure to height above MSL (i.e. altitude) using the international standard atmosphere.
 * The pressure levels are initialized by a previous pressure-conversion.
 */
class PressureToStandardAltitudeConverter : public ToVLevelConverter {
    const boost::shared_ptr<ToVLevelConverter> presConv_;
public:
    /**
     * @param presConv another ToVLevelConverter converting to pressure
     */
    PressureToStandardAltitudeConverter(boost::shared_ptr<ToVLevelConverter> presConv) : presConv_(presConv) {}
    virtual vector<double> operator()(size_t x, size_t y, size_t t);
};

/**
 * Use altitude (height above MSL) and topography to calculate the
 * height above ground (hg = height - topo)
 */
class AltitudeConverterToHeightConverter : public ToVLevelConverter {
    const boost::shared_ptr<ToVLevelConverter> conv_;
    const boost::shared_array<float> topo_;
    size_t nx_;
    size_t ny_;
    size_t nz_;
    size_t nt_;
public:
    AltitudeConverterToHeightConverter(const boost::shared_ptr<ToVLevelConverter> altiConverter, const boost::shared_array<float> topography, size_t nx, size_t ny, size_t nk, size_t nt)
    : conv_(altiConverter), topo_(topography), nx_(nx), ny_(ny), nz_(nk), nt_(nt) {}
    virtual vector<double> operator()(size_t x, size_t y, size_t t);
};

/**
 * Use height-converter and topography to calculate the
 * altitude = height above MSL (height = hg + topo)
 */
class HeightConverterToAltitudeConverter : public ToVLevelConverter {
    const boost::shared_ptr<ToVLevelConverter> conv_;
    const boost::shared_array<float> topo_;
    size_t nx_;
    size_t ny_;
    size_t nz_;
    size_t nt_;
public:
    HeightConverterToAltitudeConverter(const boost::shared_ptr<ToVLevelConverter> heightConverter, const boost::shared_array<float> topography, size_t nx, size_t ny, size_t nk, size_t nt)
    : conv_(heightConverter), topo_(topography), nx_(nx), ny_(ny), nz_(nk), nt_(nt) {}
    virtual vector<double> operator()(size_t x, size_t y, size_t t);
};


/**
 * Use geopotential height (in m) and topography to calculate the
 * height above ground (hg = height - topo)
 */
class GeopotentialToAltitudeConverter : public ToVLevelConverter {
    const boost::shared_array<float> geopot_;
    size_t nx_;
    size_t ny_;
    size_t nz_;
    size_t nt_;
public:
    GeopotentialToAltitudeConverter(const boost::shared_array<float> geopot_height, size_t nx, size_t ny, size_t nk, size_t nt)
    : geopot_(geopot_height), nx_(nx), ny_(ny), nz_(nk), nt_(nt) {}
    virtual vector<double> operator()(size_t x, size_t y, size_t t);
};


/**
 * Use ocean_s_coordinate_g1 or g2 to calculate depth
 */
class OceanSCoordinateGToDepthConverter : public ToVLevelConverter {
    vector<double> s_;
    vector<double> C_;
    double depth_c_;
    const IndexedData eta_;
    const IndexedData depth_;
    /* eta might be 0 */
    size_t nx_;
    size_t ny_;
    size_t nz_;
    size_t nt_;
    bool timeDependentDepth_;
    int (*func_)(size_t, double, double, double, const double*, const double*, double*);

public:
    /**
     * @param s k-dependent sigma coordinate
     * @param C k-dependent stretching function
     * @param depth_c critical depth (~ min(h(x,y)))
     * @param eta time-varying free surface, might be 0
     * @param depth ocean-depth, might be time-varying for sediment applications
     * @param nx,ny,nk,nt array sizes
     * @param func either mifi_ocean_s_g1_z() or mifi_ocean_s_g2_z()
     */
    OceanSCoordinateGToDepthConverter(const vector<double>& s, const vector<double>& C, double depth_c,
                                      IndexedData eta, IndexedData depth,
                                      size_t nx, size_t ny, size_t nk, size_t nt,
                                      int (*func)(size_t, double, double, double, const double*, const double*, double*))
    : s_(s), C_(C), depth_c_(depth_c), eta_(eta), depth_(depth),
      nx_(nx), ny_(ny), nz_(nk), nt_(nt)
    {
            timeDependentDepth_ = (depth_.idx().getDims().size() == 3);
            func_ = func;
    }
    virtual vector<double> operator()(size_t x, size_t y, size_t t);
    virtual bool isValid(double val, size_t x, size_t y, size_t t);
};

} // namespace

#endif /* TO_VLEVEL_CONVERTER_H_ */
