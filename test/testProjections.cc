/*
 * Fimex, testProjections.cc
 *
 * (C) Copyright 2010-2022, met.no
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

#include "testinghelpers.h"

#include "fimex/MathUtils.h"
#include "fimex/coordSys/Projection.h"
#include "fimex/interpolation.h"

#include <algorithm>

using namespace std;
using namespace MetNoFimex;

namespace {
const string projLonLat = "+proj=lonlat +ellps=sphere +a=6371000 +e=0";
} // namespace

TEST4FIMEX_TEST_CASE(test_projection)
{
    string proj4stere = "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a=6371000 +e=0";
    Projection_p projs = Projection::createByProj4(proj4stere);
    TEST4FIMEX_CHECK((projs->getName() == "stereographic") || (projs->getName() == "polar_stereographic"));
    TEST4FIMEX_CHECK(projs->isDegree() == false);

    std::vector<CDMAttribute> attrs = projs->getParameters();
    // generate another projection
    Projection_p projs2 = Projection::create(attrs);
    TEST4FIMEX_CHECK((projs2->getName() == "stereographic") || (projs2->getName() == "polar_stereographic"));
}

TEST4FIMEX_TEST_CASE(test_projection_oblique_mercator)
{
    string proj4omerc="+proj=omerc +lonc=5.34065 +lat_0=60.742 +alpha=19.0198 +no_rot   +a=6.37814e+06  +b=6.35675e+06 +no_defs +x_0=-3.86098e+06 +y_0=1.5594e+06";
    Projection_p projs_omerc = Projection::createByProj4(proj4omerc);
    TEST4FIMEX_CHECK_EQ(projs_omerc->getName(), "oblique_mercator");
    TEST4FIMEX_CHECK_EQ(projs_omerc->isDegree(), false);

    std::vector<CDMAttribute> attrs_omerc = projs_omerc->getParameters();
    // generate another projection
    Projection_p projs2_omerc = Projection::create(attrs_omerc);
    TEST4FIMEX_CHECK_EQ(projs2_omerc->getName(), "oblique_mercator");
}

TEST4FIMEX_TEST_CASE(test_projection_geostationary)
{
    string proj4geos="+proj=geos +lon_0=0 +h=3.57858e+07  +a=6.37817e+06  +b=6.35658e+06 +no_defs +x_0=-2.2098e+06 +y_0=-3.50297e+06";
    Projection_p projs_geos = Projection::createByProj4(proj4geos);
    TEST4FIMEX_CHECK_EQ(projs_geos->getName(), "geostationary");
    TEST4FIMEX_CHECK_EQ(projs_geos->isDegree(), false);

    std::vector<CDMAttribute> attrs_geos = projs_geos->getParameters();
    // generate another projection
    Projection_p projs2_geos = Projection::create(attrs_geos);
    TEST4FIMEX_CHECK_EQ(projs2_geos->getName(), "geostationary");
}

TEST4FIMEX_TEST_CASE(test_projection_sinusoidal)
{
    const string proj4 = "+proj=sinu +lon_0=0 ";
    Projection_p proj = Projection::createByProj4(proj4);
    TEST4FIMEX_CHECK_EQ(proj->getName(), "sinusoidal");
    TEST4FIMEX_CHECK_EQ(proj->isDegree(), false);

    std::vector<CDMAttribute> proj_attrs = proj->getParameters();
    // generate another projection
    Projection_p proj2 = Projection::create(proj_attrs);
    TEST4FIMEX_CHECK_EQ(proj2->getName(), proj->getName());
}

// TODO: test other projections

TEST4FIMEX_TEST_CASE(test_conversion)
{
    string proj4stere = "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a=6371000 +e=0";
    Projection_p projs = Projection::createByProj4(proj4stere);

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
    transform_rad_to_deg(lonVals);
    transform_rad_to_deg(latVals);

    // calculate the values with projection
    std::vector<double> lonValsConv(xVals.begin(), xVals.end());
    std::vector<double> latValsConv(yVals.begin(), yVals.end());

    projs->convertToLonLat(lonValsConv, latValsConv);

    for (size_t i = 0; i < lonValsConv.size(); ++i) {
        TEST4FIMEX_CHECK((latValsConv[i] <= 90.001) && (latValsConv[i] >= -90.001));
        TEST4FIMEX_CHECK((lonValsConv[i] <= 180.001) && (lonValsConv[i] >= -180.001));
        TEST4FIMEX_CHECK(fabs(lonValsConv[i] - lonVals[i]) < 1e-5);
        TEST4FIMEX_CHECK(fabs(latValsConv[i] - latVals[i]) < 1e-5);
    }

    projs->convertFromLonLat(lonValsConv, latValsConv);
    for (size_t i = 0; i < lonValsConv.size(); ++i) {
        TEST4FIMEX_CHECK(fabs(lonValsConv[i] - xVals[i]) < 1e-5);
        TEST4FIMEX_CHECK(fabs(latValsConv[i] - yVals[i]) < 1e-5);
    }
}

TEST4FIMEX_TEST_CASE(test_conversion_oblique_mercator)
{
    string proj4omerc="+proj=omerc +lonc=5.34065 +lat_0=60.742 +alpha=19.0198 +no_rot   +a=6.37814e+06  +b=6.35675e+06 +no_defs +x_0=-3.86098e+06 +y_0=1.5594e+06";
    Projection_p proj = Projection::createByProj4(proj4omerc);

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
    mifi_project_values(proj4omerc.c_str(), projLonLat.c_str(), &lonVals[0], &latVals[0], lonVals.size());
    transform_rad_to_deg(lonVals);
    transform_rad_to_deg(latVals);

    // calculate the values with projection
    std::vector<double> lonValsConv(xVals.begin(), xVals.end());
    std::vector<double> latValsConv(yVals.begin(), yVals.end());

    proj->convertToLonLat(lonValsConv, latValsConv);

    for (size_t i = 0; i < lonValsConv.size(); ++i) {
        TEST4FIMEX_CHECK((latValsConv[i] <= 90.001) && (latValsConv[i] >= -90.001));
        TEST4FIMEX_CHECK((lonValsConv[i] <= 180.001) && (lonValsConv[i] >= -180.001));
        TEST4FIMEX_CHECK(fabs(lonValsConv[i] - lonVals[i]) < 1e-5);
        TEST4FIMEX_CHECK(fabs(latValsConv[i] - latVals[i]) < 1e-5);
    }

    proj->convertFromLonLat(lonValsConv, latValsConv);
    for (size_t i = 0; i < lonValsConv.size(); ++i) {
        TEST4FIMEX_CHECK(fabs(lonValsConv[i] - xVals[i]) < 1e-5);
        TEST4FIMEX_CHECK(fabs(latValsConv[i] - yVals[i]) < 1e-5);
    }
}

TEST4FIMEX_TEST_CASE(test_conversion_geostationary)
{
    string proj4geos="+proj=geos +lon_0=0 +h=3.57858e+07  +a=6.37817e+06  +b=6.35658e+06 +no_defs +x_0=-2.2098e+06 +y_0=-3.50297e+06";
    Projection_p proj = Projection::createByProj4(proj4geos);

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
    mifi_project_values(proj4geos.c_str(), projLonLat.c_str(), &lonVals[0], &latVals[0], lonVals.size());
    transform_rad_to_deg(lonVals);
    transform_rad_to_deg(latVals);

    // calculate the values with projection
    std::vector<double> lonValsConv(xVals.begin(), xVals.end());
    std::vector<double> latValsConv(yVals.begin(), yVals.end());

    proj->convertToLonLat(lonValsConv, latValsConv);

    for (size_t i = 0; i < lonValsConv.size(); ++i) {
        TEST4FIMEX_CHECK((latValsConv[i] <= 90.001) && (latValsConv[i] >= -90.001));
        TEST4FIMEX_CHECK((lonValsConv[i] <= 180.001) && (lonValsConv[i] >= -180.001));
        TEST4FIMEX_CHECK(fabs(lonValsConv[i] - lonVals[i]) < 1e-5);
        TEST4FIMEX_CHECK(fabs(latValsConv[i] - latVals[i]) < 1e-5);
    }

    proj->convertFromLonLat(lonValsConv, latValsConv);
    for (size_t i = 0; i < lonValsConv.size(); ++i) {
        TEST4FIMEX_CHECK(fabs(lonValsConv[i] - xVals[i]) < 1e-5);
        TEST4FIMEX_CHECK(fabs(latValsConv[i] - yVals[i]) < 1e-5);
    }
}

TEST4FIMEX_TEST_CASE(test_common_projections)
{
    const std::vector<std::string> proj4 = {
        "+proj=lcc +lat_0=77.5 +lon_0=335 +lat_1=77.5 +lat_2=77.5 +no_defs +R=6.371e+06",        // Arome-Arctic 2022
        "+proj=lcc +lat_0=77.5 +lon_0=-25 +lat_1=77.5 +lat_2=77.5 +no_defs +R=6.371e+06",        // Arome-Arctic 2015
        "+proj=lcc +lat_0=63.3 +lon_0=15 +lat_1=63.3 +lat_2=63.3 +no_defs +R=6.371e+06",         // MEPS 2022
        "+proj=lcc +lon_0=15 +lat_0=63 +lat_1=63 +lat_2=63 +R=6.371e+06 +units=m +no_defs",      // MEPS 2015
        "+proj=stere +ellps=WGS84 +lat_0=90.0 +lat_ts=60.0 +x_0=3192800 +y_0=1784000 +lon_0=70", // NorKyst 2022
        "+proj=stere +lat_0=90 +lon_0=70 +lat_ts=60 +units=m +a=6.371e+06 +e=0 +no_defs",        // NorKyst 2015
        "+proj=ob_tran +o_proj=longlat +lon_0=-40 +o_lat_p=22 +R=6.371e+06 +no_defs",            // MyWave Wam 4km
        "+proj=longlat +a=6367470 +e=0 +no_defs",                                                // ECMWF 2022
    };

    for (const auto& p4 : proj4) {
        Projection_p p = Projection::createByProj4(p4);
        TEST4FIMEX_CHECK_NE(p->getName(), "unknown_to_fgdc");
        std::vector<double> lon{10}, lat{60};
        p->convertFromLonLat(lon, lat);
    }
}

TEST4FIMEX_TEST_CASE(test_bad_projection)
{
    const std::string p4 = "+proj=fork +lat_1000=123 +fish=-25 +car=6.371e+06";
    Projection_cp p = Projection::createByProj4(p4);
    TEST4FIMEX_CHECK_EQ(p->getName(), "unknown_to_fgdc");
}
