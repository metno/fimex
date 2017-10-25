/*
 * Fimex, testVerticalCoordinates.cc
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
 *  Created on: Jul 29, 2011
 *      Author: Heiko Klein
 */

#include "fimex_config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>
using boost::unit_test_framework::test_suite;

#include "fimex/vertical_coordinate_transformations.h"

using namespace std;

BOOST_AUTO_TEST_CASE( test_atmosphere_sigma_pressure )
{
    /*
     * NCL example
     *
     *   psurf = new((/ 1, 1 /), float)
     *   psurf(0,0) = (/ 1015.00 /) ; hPa
     *   sigma = (/1., .9, .8, .7, .6, .5, .1 /)
     *   p = pres_sigma(sigma, psurf)
     *   print(p)
     */
    const size_t n = 7;
    double pSurf = 1015.;
    double pTop = 0.;
    double sigma[n] = {1., .9, .8, .7, .6, .5, .1};
    double pExp[n] =  {1015., 913.5, 812., 710.5, 609., 507.5, 101.5};
    double pOut[n];

    mifi_atmosphere_sigma_pressure(n, pTop, pSurf, sigma, pOut);
    for (size_t i = 0; i < n; ++i)
        BOOST_CHECK_CLOSE(pExp[i], pOut[i], 1e-3);
}

BOOST_AUTO_TEST_CASE( test_atmosphere_hybrid_sigma_pressure )
{
    /*
     * NCL example
     *
     *   psurf = new((/ 1, 1 /), float)
     *   psurf(0,0) = (/ 1015.00 /) ; hPa
     *   p0 = 1000. ; hPa
     *   ; first 5 and last 2 values from hirlam12
     *   a = (/1., 3., 5., 7., 9., 0.00098881774, 0.000996283525/)
     *   b = (/0., 0., 0.00011835, 0.00057981, 0.00158568, 0.98881774, 0.996283525 /)
     *
     *   p = pres_hybrid_ccm(psurf,p0,a,b)
     *   print(p)
     */
    const size_t n = 7;
    double pSurf = 1015.;
    double p0 = 1000.;
    double a[n] = {1., 3., 5., 7., 9., 0.00098881774, 0.000996283525};
    double ap[n];
    double b[n] = {0., 0., 0.00011835, 0.00057981, 0.00158568, 0.98881774, 0.996283525};
    double pExp[n] =  {1000., 3000., 5000.12, 7000.588, 9001.609, 1004.639, 1012.224};
    double pOut[n];

    mifi_atmosphere_hybrid_sigma_pressure(n, p0, pSurf, a, b, pOut);
    for (size_t i = 0; i < n; ++i)
        BOOST_CHECK_CLOSE(pExp[i], pOut[i], 1e-3);

    for (size_t i = 0; i < n; ++i)
        ap[i] = a[i]*p0;
    mifi_atmosphere_hybrid_sigma_ap_pressure(n, pSurf, ap, b, pOut);
    for (size_t i = 0; i < n; ++i)
        BOOST_CHECK_CLOSE(pExp[i], pOut[i], 1e-3);
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
