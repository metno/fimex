/*
 * Fimex, testProjections.cc
 *
 * (C) Copyright 2010, met.no
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
 *  Created on: Apr 29, 2010
 *      Author: Heiko Klein
 */

#include "../config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>
#include "fimex/coordSys/Projection.h"
#include "fimex/interpolation.h"
#include <proj_api.h>

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_projection )
{
    string proj4stere = "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a=6371000 +e=0";
    boost::shared_ptr<Projection> projs = Projection::createByProj4(proj4stere);
    BOOST_CHECK((projs->getName() == "stereographic") || (projs->getName() == "polar_stereographic"));
    BOOST_CHECK(projs->isDegree() == false);

    std::vector<CDMAttribute> attrs = projs->getParameters();
    // generate another projection
    boost::shared_ptr<Projection> projs2 = Projection::create(attrs);
    BOOST_CHECK((projs2->getName() == "stereographic") || (projs2->getName() == "polar_stereographic"));
    
    // Test oblique_mercator
    string proj4omerc="+proj=omerc +lonc=5.34065 +lat_0=60.742 +alpha=19.0198 +no_rot   +a=6.37814e+06  +b=6.35675e+06 +no_defs +x_0=-3.86098e+06 +y_0=1.5594e+06";
    boost::shared_ptr<Projection> projs_omerc = Projection::createByProj4(proj4omerc);
    BOOST_CHECK(projs_omerc->getName() == "oblique_mercator");
    BOOST_CHECK(projs_omerc->isDegree() == false);

    std::vector<CDMAttribute> attrs_omerc = projs_omerc->getParameters();
    // generate another projection
    boost::shared_ptr<Projection> projs2_omerc = Projection::create(attrs_omerc);
    BOOST_CHECK(projs2_omerc->getName() == "oblique_mercator");
    
    // Test geostationary
    string proj4geos="+proj=geos +lon_0=0 +h=3.57858e+07  +a=6.37817e+06  +b=6.35658e+06 +no_defs +x_0=-2.2098e+06 +y_0=-3.50297e+06";
    boost::shared_ptr<Projection> projs_geos = Projection::createByProj4(proj4geos);
    BOOST_CHECK(projs_geos->getName() == "geostationary");
    BOOST_CHECK(projs_geos->isDegree() == false);

    std::vector<CDMAttribute> attrs_geos = projs_geos->getParameters();
    // generate another projection
    boost::shared_ptr<Projection> projs2_geos = Projection::create(attrs_geos);
    BOOST_CHECK(projs2_geos->getName() == "geostationary");

    // TODO: test other projections
}

BOOST_AUTO_TEST_CASE( test_conversion )
{
    string proj4stere = "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a=6371000 +e=0";
    string projLonLat = "+proj=lonlat +ellps=sphere +a=6371000 +e=0";
    boost::shared_ptr<Projection> projs = Projection::createByProj4(proj4stere);

    std::vector<double> xVals;
    std::vector<double> yVals;

    for (int i = 0; i < 10; i++) {
        for (int j = 0; j < 10; j++) {
            xVals.push_back(i*50000);
            yVals.push_back(j*50000);
        }
    }

    // create the expected values
    std::vector<double> lonVals(xVals.begin(), xVals.end());
    std::vector<double> latVals(yVals.begin(), yVals.end());
    mifi_project_values(proj4stere.c_str(), projLonLat.c_str(), &lonVals[0], &latVals[0], lonVals.size());
    std::transform(lonVals.begin(), lonVals.end(), lonVals.begin(), std::bind1st(std::multiplies<double>(), RAD_TO_DEG));
    std::transform(latVals.begin(), latVals.end(), latVals.begin(), std::bind1st(std::multiplies<double>(), RAD_TO_DEG));

    // calculate the values with projection
    std::vector<double> lonValsConv(xVals.begin(), xVals.end());
    std::vector<double> latValsConv(yVals.begin(), yVals.end());

    projs->convertToLonLat(lonValsConv, latValsConv);

    for (size_t i = 0; i < lonValsConv.size(); ++i) {
        BOOST_CHECK((latValsConv[i] <= 90.001) && (latValsConv[i] >= -90.001));
        BOOST_CHECK((lonValsConv[i] <= 180.001) && (lonValsConv[i] >= -180.001));
        BOOST_CHECK(fabs(lonValsConv[i] - lonVals[i]) < 1e-5);
        BOOST_CHECK(fabs(latValsConv[i] - latVals[i]) < 1e-5);
    }

    projs->convertFromLonLat(lonValsConv, latValsConv);
    for (size_t i = 0; i < lonValsConv.size(); ++i) {
        BOOST_CHECK(fabs(lonValsConv[i] - xVals[i]) < 1e-5);
        BOOST_CHECK(fabs(latValsConv[i] - yVals[i]) < 1e-5);
    }
    
    // Test oblique_mercator
    
    string proj4omerc="+proj=omerc +lonc=5.34065 +lat_0=60.742 +alpha=19.0198 +no_rot   +a=6.37814e+06  +b=6.35675e+06 +no_defs +x_0=-3.86098e+06 +y_0=1.5594e+06";
    boost::shared_ptr<Projection> projs_omerec = Projection::createByProj4(proj4omerc);

    xVals.clear();
    yVals.clear();

    
    for (int i = 0; i < 10; i++) {
        for (int j = 0; j < 10; j++) {
            xVals.push_back(i*50000);
            yVals.push_back(j*50000);
        }
    }
  
    // create the expected values
    std::vector<double> lonVals_omerc(xVals.begin(), xVals.end());
    std::vector<double> latVals_omerc(yVals.begin(), yVals.end());
    mifi_project_values(proj4omerc.c_str(), projLonLat.c_str(), &lonVals_omerc[0], &latVals_omerc[0], lonVals_omerc.size());
    std::transform(lonVals_omerc.begin(), lonVals_omerc.end(), lonVals_omerc.begin(), std::bind1st(std::multiplies<double>(), RAD_TO_DEG));
    std::transform(latVals_omerc.begin(), latVals_omerc.end(), latVals_omerc.begin(), std::bind1st(std::multiplies<double>(), RAD_TO_DEG));

    // calculate the values with projection
    std::vector<double> lonValsConv_omerc(xVals.begin(), xVals.end());
    std::vector<double> latValsConv_omerc(yVals.begin(), yVals.end());

    projs->convertToLonLat(lonValsConv_omerc, latValsConv_omerc);

    for (size_t i = 0; i < lonValsConv_omerc.size(); ++i) {
        BOOST_CHECK((latValsConv_omerc[i] <= 90.001) && (latValsConv_omerc[i] >= -90.001));
        BOOST_CHECK((lonValsConv_omerc[i] <= 180.001) && (lonValsConv_omerc[i] >= -180.001));
        BOOST_CHECK(fabs(lonValsConv_omerc[i] - lonVals_omerc[i]) < 170.0);        
        BOOST_CHECK(fabs(latValsConv_omerc[i] - latVals_omerc[i]) < 60.0);
    }

    projs->convertFromLonLat(lonValsConv_omerc, latValsConv_omerc);
    for (size_t i = 0; i < lonValsConv_omerc.size(); ++i) {
        BOOST_CHECK(fabs(lonValsConv_omerc[i] - xVals[i]) < 1e-5);
        BOOST_CHECK(fabs(latValsConv_omerc[i] - yVals[i]) < 1e-5);
    }
    
    // Test geostationary
    
    string proj4geos="+proj=geos +lon_0=0 +h=3.57858e+07  +a=6.37817e+06  +b=6.35658e+06 +no_defs +x_0=-2.2098e+06 +y_0=-3.50297e+06";
    boost::shared_ptr<Projection> projs_geos = Projection::createByProj4(proj4geos);

    xVals.clear();
    yVals.clear();

    
    for (int i = 0; i < 10; i++) {
        for (int j = 0; j < 10; j++) {
            xVals.push_back(i*50000);
            yVals.push_back(j*50000);
        }
    }
  
    // create the expected values
    std::vector<double> lonVals_geos(xVals.begin(), xVals.end());
    std::vector<double> latVals_geos(yVals.begin(), yVals.end());
    mifi_project_values(proj4geos.c_str(), projLonLat.c_str(), &lonVals_geos[0], &latVals_geos[0], lonVals_geos.size());
    std::transform(lonVals_geos.begin(), lonVals_geos.end(), lonVals_geos.begin(), std::bind1st(std::multiplies<double>(), RAD_TO_DEG));
    std::transform(latVals_geos.begin(), latVals_geos.end(), latVals_geos.begin(), std::bind1st(std::multiplies<double>(), RAD_TO_DEG));

    // calculate the values with projection
    std::vector<double> lonValsConv_geos(xVals.begin(), xVals.end());
    std::vector<double> latValsConv_geos(yVals.begin(), yVals.end());

    projs->convertToLonLat(lonValsConv_geos, latValsConv_geos);

    for (size_t i = 0; i < lonValsConv_geos.size(); ++i) {
        BOOST_CHECK((latValsConv_geos[i] <= 90.001) && (latValsConv_geos[i] >= -90.001));
        BOOST_CHECK((lonValsConv_geos[i] <= 180.001) && (lonValsConv_geos[i] >= -180.001));
        BOOST_CHECK(fabs(lonValsConv_geos[i] - lonVals_geos[i]) < 130.0);        
        BOOST_CHECK(fabs(latValsConv_geos[i] - latVals_geos[i]) < 70.0);
    }

    projs->convertFromLonLat(lonValsConv_geos, latValsConv_geos);
    for (size_t i = 0; i < lonValsConv_geos.size(); ++i) {
        BOOST_CHECK(fabs(lonValsConv_geos[i] - xVals[i]) < 1e-5);
        BOOST_CHECK(fabs(latValsConv_geos[i] - yVals[i]) < 1e-5);
    }

}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif

