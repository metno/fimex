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
    int i = 0;
    while (i++ < n) {
        *pressure++ = p0 * exp(-(*lev++));
    }
    return MIFI_OK;
}

int mifi_atmosphere_sigma_pressure(size_t n, double ptop, double ps, const double* sigma, double* pressure)
{
    int i = 0;
    while (i++ < n) {
        *pressure++ = ptop + *sigma++ * (ps - ptop);
    }
    return MIFI_OK;
}

int mifi_atmosphere_hybrid_sigma_pressure(size_t n, double p0, double ps, const double* a, const double* b, double* pressure)
{
    int i = 0;
    while (i++ < n) {
        *pressure++ = (*a++ * p0) + (*b++ * ps);
    }
    return MIFI_OK;
}

int mifi_atmosphere_hybrid_sigma_ap_pressure(size_t n, double ps, const double* ap, const double* b, double* pressure)
{
    int i = 0;
    while (i++ < n) {
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
    int i = 0;
    while (i++ < n) {
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
    int i = 0;
    while (i++ < n) {
        *height++ =  K * log(*p++/P_b);
    }
    return MIFI_OK;
}

int mifi_barometric_standard_height(size_t n, const double* p, double* height)
{
    return mifi_barometric_height(n, 1013.25, p, 288.15, height);
}

