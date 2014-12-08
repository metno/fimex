/*
 * Fimex, vertical_coordinate_transformations.c
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
 *  Created on: Jul 28, 2011
 *      Author: Heiko Klein
 */

#include "fimex/vertical_coordinate_transformations.h"

int mifi_atmosphere_ln_pressure(size_t n, double p0, const double* lev, double* pressure)
{
    while (n--) {
        *pressure++ = p0 * exp(-(*lev++));
    }
    return MIFI_OK;
}

int mifi_atmosphere_sigma_pressure(size_t n, double ptop, double ps, const double* sigma, double* pressure)
{
    double pDiff = (ps - ptop);
    while (n--) {
        *pressure++ = ptop + *sigma++ * pDiff;
    }
    return MIFI_OK;
}

int mifi_atmosphere_pressure_sigma(size_t n, double ptop, double ps, const double* pressure, double* sigma)
{
    double pDiffInv = 1/(ps-ptop);
    while (n--) {
        *sigma++ = (*pressure++ - ptop) * pDiffInv;
    }
    return MIFI_OK;

}


int mifi_atmosphere_hybrid_sigma_pressure(size_t n, double p0, double ps, const double* a, const double* b, double* pressure)
{
    while (n--) {
        *pressure++ = (*a++ * p0) + (*b++ * ps);
    }
    return MIFI_OK;
}

int mifi_atmosphere_hybrid_sigma_ap_pressure(size_t n, double ps, const double* ap, const double* b, double* pressure)
{
    while (n--) {
        *pressure++ = (*ap++) + (*b++ * ps);
    }
    return MIFI_OK;
}

int mifi_barometric_pressure(size_t n, double P_b, const double* h, double T_b, double* pressure)
{
    const double g = 9.80665;
    const double M = 0.0289644;
    const double R = 8.31432;
    const double C = -g*M/(R*T_b);
    while (n--) {
        *pressure++ = P_b * exp(C * (*h++));
    }
    return MIFI_OK;
}

int mifi_barometric_standard_pressure(size_t n, const double* h, double* pressure)
{
    return mifi_barometric_pressure(n, 1013.25, h, 288.15, pressure);
}


int mifi_barometric_height(size_t n, double P_b, const double* p, double T_b, double* height)
{
    const double g = 9.80665;
    const double M = 0.0289644;
    const double R = 8.31432;
    const double K = -R*T_b/(g*M);
    while (n--) {
        *height++ =  K * log(*p++/P_b);
    }
    return MIFI_OK;
}

int mifi_barometric_standard_altitude(size_t n, const double* p, double* altitude)
{
    return mifi_barometric_height(n, 1013.25, p, 288.15, altitude);
}

int mifi_ocean_s_g1_z(size_t n, double h, double h_c, double zeta, const double* sigma, const double* C, double* z)
{
    double h_inv = 1/h;
    while (n--) {
        double S   = h_c * *sigma++ + (h-h_c) * *C++;
        *z++ = S + zeta*(1+S*h_inv);
    }
    return MIFI_OK;
}
int mifi_ocean_s_g2_z(size_t n, double h, double h_c, double zeta, const double* sigma, const double* C, double* z)
{
    double hph_c_inv = 1/(h+h_c);
    while (n--) {
        double S = hph_c_inv * (h_c* *sigma++ +h * *C++);
        *z++ = zeta + (zeta + h)*S;
    }
    return MIFI_OK;
}


int mifi_omega_to_vertical_wind(size_t n, const double* omega, const double* p, const double* t, double* w)
{
    // omega = - rho * g * w -> w = -omega/(g*rho)
    //
    // rho = p / ( R * T )  (see http://wikimedia.org/wikipedia/en/wiki/Density_of_air )
    // -> w = -omega * R * T / (g * p)
    // R (dry_air) = 287.058 J/(kg·K)
    const double g = 9.80665;
    const double mR_g = -1 * 287.058 / g;

    while (n--) {
        *w++ = mR_g * *omega++ * *t++ / *p++;
    }

    return MIFI_OK;
}
