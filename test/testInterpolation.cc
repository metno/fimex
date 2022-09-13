/*
 * Fimex
 *
 * (C) Copyright 2008-2022, met.no
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
 */

#include "testinghelpers.h"
#include "fimex/interpolation.h"

#include "fimex/CDMAttribute.h"
#include "fimex/Data.h"
#include "fimex/MathUtils.h"

#include "fimex/reproject.h"

#include <cmath>
#include <fstream>
#include <string>

using MetNoFimex::pathTest;
using namespace MetNoFimex::reproject;

namespace {
inline bool near(float a, float b, float eps)
{
    return std::fabs(a - b) < eps;
}
} // namespace

TEST4FIMEX_TEST_CASE(mifi_points2position)
{
    double axis[5] = {1., 2., 3., 4., 5.};
    double points[5] = {-3., 5., 1.3, 2., 6.};
    double apoints[5] = {-4., 4., 0.3, 1., 5.}; // results
    mifi_points2position(points,5,axis,5,MIFI_PROJ_AXIS);
    for (int i = 0; i < 5; ++i) {
        TEST4FIMEX_CHECK(near(apoints[i], points[i], 1e-10));
    }
}

TEST4FIMEX_TEST_CASE(mifi_points2position_reverse)
{
    double axis[5] = {5., 4., 3., 2., 1.};
    double points[5] = {-3., 5., 1.3, 2., 6.};
    double apoints[5] = {8., 0., 3.7, 3., -1.}; // results
    mifi_points2position(points,5,axis,5,MIFI_PROJ_AXIS);
    for (int i = 0; i < 5; ++i) {
        TEST4FIMEX_CHECK(near(apoints[i], points[i], 1e-10));
    }
}

TEST4FIMEX_TEST_CASE(mifi_get_values_bilinear_f)
{
    float infield[4] = {1., 2., 2., 1+std::sqrt(2.0f)}; // (0,0), (0,1), (1,0), (1,1) #(y,x)
    float outvalues[1];

    mifi_get_values_bilinear_f(infield, outvalues, 0.3, 0., 2, 2, 1);
    TEST4FIMEX_CHECK(near(outvalues[0], 1.3, 1e-6));
    mifi_get_values_bilinear_f(infield, outvalues, 0.3, 0.0001, 2, 2, 1);
    //std::cerr << outvalues[0] << std::endl;
    TEST4FIMEX_CHECK(near(outvalues[0], 1.3, 1e-4));
    mifi_get_values_bilinear_f(infield, outvalues, 0., 0.3, 2, 2, 1);
    TEST4FIMEX_CHECK(near(outvalues[0], 1.3, 1e-6));
    mifi_get_values_bilinear_f(infield, outvalues, 0.0001, 0.3, 2, 2, 1);
    //std::cerr << outvalues[0] << std::endl;
    TEST4FIMEX_CHECK(near(outvalues[0], 1.3, 1e-4));

    // check for border values / nan
    mifi_get_values_bilinear_f(infield, outvalues, 0, 0, 2, 2, 1);
    TEST4FIMEX_CHECK(!std::isnan(outvalues[0]));
    mifi_get_values_bilinear_f(infield, outvalues, 1, 1, 2, 2, 1);
    TEST4FIMEX_CHECK(!std::isnan(outvalues[0]));
    mifi_get_values_bilinear_f(infield, outvalues, 1.5, 0.5, 2, 2, 1);
    TEST4FIMEX_CHECK(std::isnan(outvalues[0]));
    mifi_get_values_bilinear_f(infield, outvalues, 0.5, 1.5, 2, 2, 1);
    TEST4FIMEX_CHECK(std::isnan(outvalues[0]));
    mifi_get_values_bilinear_f(infield, outvalues, 0.5, -0.5, 2, 2, 1);
    TEST4FIMEX_CHECK(std::isnan(outvalues[0]));
    mifi_get_values_bilinear_f(infield, outvalues, -0.5, 0.5, 2, 2, 1);
    TEST4FIMEX_CHECK(std::isnan(outvalues[0]));
}

TEST4FIMEX_TEST_CASE(mifi_get_values_bicubic_f)
{
    /* infield constant in x-direction */
    float infield[16] = {1, 1, 1, 1,
                         2, 2, 2, 2,
                         2, 2, 2, 2,
                         1, 1, 1, 1};
    float outvalues[1];
    float infield_t[16]; /* transposed */
    for (int i = 0; i < 4; i++)
        for (int j = 0; j < 4; j++)
            infield_t[i+4*j] = infield[j+4*i];
    mifi_get_values_bicubic_f(infield, outvalues, 1, 1, 4, 4, 1);
    TEST4FIMEX_CHECK_CLOSE(2.f, outvalues[0], 1e-3);
    mifi_get_values_bicubic_f(infield, outvalues, 1, 1.99999, 4, 4, 1);
    TEST4FIMEX_CHECK_CLOSE(2.f, outvalues[0], 1e-3);
    mifi_get_values_bicubic_f(infield, outvalues, 1, 1.5, 4, 4, 1);
    TEST4FIMEX_CHECK_CLOSE(2.125f, outvalues[0], 1e-3);
    mifi_get_values_bicubic_f(infield, outvalues, 1.5, 1, 4, 4, 1);
    TEST4FIMEX_CHECK_CLOSE(2.f, outvalues[0], 1e-3);

    // and transposed
    mifi_get_values_bicubic_f(infield_t, outvalues, 1, 1, 4, 4, 1);
    TEST4FIMEX_CHECK_CLOSE(2.f, outvalues[0], 1e-3);
    mifi_get_values_bicubic_f(infield_t, outvalues, 1.99999, 1, 4, 4, 1);
    TEST4FIMEX_CHECK_CLOSE(2.f, outvalues[0], 1e-3);
    mifi_get_values_bicubic_f(infield_t, outvalues, 1.5, 1, 4, 4, 1);
    TEST4FIMEX_CHECK_CLOSE(2.125f, outvalues[0], 1e-3);
    mifi_get_values_bicubic_f(infield_t, outvalues, 1, 1.5, 4, 4, 1);
    TEST4FIMEX_CHECK_CLOSE(2.f, outvalues[0], 1e-3);

    // check for border values / nan
    mifi_get_values_bicubic_f(infield, outvalues, .5, 1, 4, 4, 1);
    TEST4FIMEX_CHECK(std::isnan(outvalues[0]));
    mifi_get_values_bicubic_f(infield, outvalues, 1, .5, 4, 4, 1);
    TEST4FIMEX_CHECK(std::isnan(outvalues[0]));
    mifi_get_values_bicubic_f(infield, outvalues, 2.5, 1, 4, 4, 1);
    TEST4FIMEX_CHECK(std::isnan(outvalues[0]));
    mifi_get_values_bicubic_f(infield, outvalues, 1, 2.5, 4, 4, 1);
    TEST4FIMEX_CHECK(std::isnan(outvalues[0]));
}

TEST4FIMEX_TEST_CASE(mifi_get_values_linear_f)
{
    const int nr = 4;
    float infieldA[nr] = {0, 1, -1, 1};
    float infieldB[nr] = {1, -1, 0, 1};
    float outfield[nr];
    // infieldA and infieldB at same position (a=b), take field1
    mifi_get_values_linear_f(infieldA, infieldB, outfield, nr, 1., 1., .5);
    for (int i = 0; i < nr; i++) {
        TEST4FIMEX_CHECK_CLOSE(outfield[i], infieldA[i], 1e-5);
    }

    // real values between a and b
    mifi_get_values_linear_f(infieldA, infieldB, outfield, nr, 1., 2., 1.5);
    for (int i = 0; i < nr; i++) {
        TEST4FIMEX_CHECK_CLOSE(outfield[i], (float).5 * (infieldA[i] + infieldB[i]), 1e-5);
    }


    // extrapolation values between a and b
    mifi_get_values_linear_f(infieldA, infieldB, outfield, nr, 0., 1., 2.);
    for (int i = 0; i < nr; i++) {
        TEST4FIMEX_CHECK_CLOSE(outfield[i], (float)infieldA[i] + 2 * (infieldB[i] - infieldA[i]), 1e-5);
    }
}

TEST4FIMEX_TEST_CASE(mifi_get_values_linear_d)
{
    const int nr = 4;
    double infieldA[nr] = {0, 1, -1, 1};
    double infieldB[nr] = {1, -1, 0, 1};
    double outfield[nr];
    // infieldA and infieldB at same position (a=b), take field1
    mifi_get_values_linear_d(infieldA, infieldB, outfield, nr, 1., 1., .5);
    for (int i = 0; i < nr; i++) {
        TEST4FIMEX_CHECK_CLOSE(outfield[i], infieldA[i], 1e-5);
    }

    // real values between a and b
    mifi_get_values_linear_d(infieldA, infieldB, outfield, nr, 1., 2., 1.5);
    for (int i = 0; i < nr; i++) {
        TEST4FIMEX_CHECK_CLOSE(outfield[i], .5 * (infieldA[i] + infieldB[i]), 1e-5);
    }


    // extrapolation values between a and b
    mifi_get_values_linear_d(infieldA, infieldB, outfield, nr, 0., 1., 2.);
    for (int i = 0; i < nr; i++) {
        TEST4FIMEX_CHECK_CLOSE(outfield[i], infieldA[i] + 2 * (infieldB[i] - infieldA[i]), 1e-5);
    }
}

TEST4FIMEX_TEST_CASE(mifi_get_values_log_f)
{
    const int nr = 1;
    float infieldA[nr] = {1000.};
    float infieldB[nr] = {100.};
    float outfield[nr];
    // corner value 100.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 100.);
    TEST4FIMEX_CHECK_CLOSE(outfield[0], 100.f, 1e-3);
    // corner value 1000.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 1000.);
    TEST4FIMEX_CHECK_CLOSE(outfield[0], 1000.f, 1e-3);
    // interpolation at 500.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 500.);
    TEST4FIMEX_CHECK_CLOSE(outfield[0], 729.073f, 1e-3);
    // extrapolation at 1500.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 1500.);
    TEST4FIMEX_CHECK_CLOSE(outfield[0], 1158.482f, 1e-3);
    // interpolation at 200.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 200.);
    TEST4FIMEX_CHECK_CLOSE(outfield[0], 370.927f, 1e-3);
    // interpolation at 800.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 800.);
    TEST4FIMEX_CHECK_CLOSE(outfield[0], 912.781f, 1e-3);
}

TEST4FIMEX_TEST_CASE(mifi_get_values_log_log_f)
{
    const int nr = 1;
    float infieldA[nr] = {1000.};
    float infieldB[nr] = {100.};
    float outfield[nr];
    // corner value 100.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 100.);
    TEST4FIMEX_CHECK_CLOSE(outfield[0], 100.f, 1e-3);
    // corner value 1000.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 1000.);
    TEST4FIMEX_CHECK_CLOSE(outfield[0], 1000.f, 1e-3);

    // below are results from NCLs vintp2p_ecmwf
    // interpolation at 500.
    mifi_get_values_log_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 500.);
    TEST4FIMEX_CHECK_CLOSE(outfield[0], 763.1873f, 1e-3);
    // interpolation at 200.
    mifi_get_values_log_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 200.);
    TEST4FIMEX_CHECK_CLOSE(outfield[0], 408.0904f, 1e-3);
    // interpolation at 800.
    mifi_get_values_log_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 800.);
    TEST4FIMEX_CHECK_CLOSE(outfield[0], 926.384f, 1e-3);
}

namespace {
const std::string latlongProj = "+proj=latlong +a=6370 +e=0";
} // namespace

TEST4FIMEX_TEST_CASE(mifi_project_axes)
{
    const std::string emepProj = "+proj=stere +a=127.4 +lat_0=90 +lon_0=-32 +lat_ts=60 +x_0=7 +y_0=109";

    double emepX[] = {6,7,8};
    double emepY[] = {108,109,110};
    double outX[9];
    double outY[9];
    TEST4FIMEX_CHECK_NO_THROW(reproject_axes(emepProj, latlongProj, &emepX[0], &emepY[0], 3, 3, &outX[0], &outY[0]));
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 3; j++)
            TEST4FIMEX_CHECK(MetNoFimex::rad_to_deg(outY[j + 3 * i]) > 89);
}

TEST4FIMEX_TEST_CASE(mifi_interpolate_f)
{
    const int iSize = 170;
    const int jSize = 150;
    const int zSize = 1;
    const int lonSize = 180;
    const int latSize = 90;
    float inArray[iSize*jSize*zSize]; // emep i, j, z
    float outArray[lonSize*latSize*zSize]; // long, lat, z
    const std::string emepProj = "+proj=stere +a=127.4 +lat_0=90 +lon_0=-32 +lat_ts=60 +x_0=7 +y_0=109";
    double emepIAxis[iSize];
    double emepJAxis[jSize];
    double latitudeAxis[latSize];
    double longitudeAxis[lonSize];

    // initialization to undefined
    for (int i = 0; i < iSize*jSize*zSize; ++i) {
        inArray[i] = MIFI_UNDEFINED_F;
    }
    for (int i = 0; i < latSize*lonSize*zSize; ++i) {
        outArray[i] = MIFI_UNDEFINED_F;
    }
    // initialization of axis (originally fortran arrays, therefore i+1)
    for (int i = 0; i < iSize; ++i) {
        emepIAxis[i] = i + 1;
    }
    for (int i = 0; i < jSize; ++i) {
        emepJAxis[i] = i + 1;
    }
    for (int i = 0; i < lonSize; ++i) {
        longitudeAxis[i] = ((i+1)/2.) - 30;
    }
    for (int i = 0; i < latSize; ++i) {
        latitudeAxis[i] = ((i+1)/2.) + 30;
    }

    // reading the data
    // i j country-id
    std::ifstream datafile (pathTest("inData.txt").c_str(), std::ios::in);
    TEST4FIMEX_REQUIRE(datafile.is_open());
    datafile.exceptions(std::ios_base::eofbit|std::ios_base::badbit|std::ios::failbit);
    int line = 0;
    while (true) {
        ++line;
        int x, y;
        float country;
        try {
            datafile >> x;
            datafile >> y;
            datafile >> country;
            inArray[mifi_3d_array_position(x-1,y-1,0,iSize,jSize,zSize)] = country;
        } catch (std::exception& fail) {
            TEST4FIMEX_REQUIRE_MESSAGE(datafile.eof(),
                                       "Exception '" << fail.what() << "' in line: " << line << " with x=" << x << " y=" << y << " country=" << country);
            break;
        }
    }
    datafile.close();
    TEST4FIMEX_REQUIRE(near(inArray[mifi_3d_array_position(93, 50, 0, iSize, jSize, zSize)], 4, 1e-5));

    TEST4FIMEX_CHECK_NO_THROW(reproject_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj, inArray, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, iSize, jSize,
                                          zSize, latlongProj, outArray, longitudeAxis, latitudeAxis, MIFI_LONGITUDE, MIFI_LATITUDE, lonSize, latSize));
    // -25 43 32 (long, lat, val)
    TEST4FIMEX_CHECK(near(outArray[mifi_3d_array_position(9, 25, 0, lonSize, latSize, zSize)], 32, 1e-6));
#if 0
    std::cerr << "long lat val: " << longitudeAxis[9] << " " << latitudeAxis[25] << " "
              << outArray[mifi_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] << std::endl;
    std::cerr << "emepProj: " << emepProj << " latlonProj: " << latlongProj << std::endl;
#endif

    for (int i = 0; i < latSize*lonSize*zSize; ++i) {
        outArray[i] = MIFI_UNDEFINED_F;
    }
    TEST4FIMEX_CHECK_NO_THROW(reproject_f(MIFI_INTERPOL_BILINEAR, emepProj, inArray, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, iSize, jSize, zSize,
                                          latlongProj, outArray, longitudeAxis, latitudeAxis, MIFI_LONGITUDE, MIFI_LATITUDE, lonSize, latSize));
    // -25 43 32 (long, lat, val)
    TEST4FIMEX_CHECK(near(outArray[mifi_3d_array_position(9, 25, 0, lonSize, latSize, zSize)], 32, 1e-6));
#if 0
    std::cerr << "long lat val: " << longitudeAxis[9] << " " << latitudeAxis[25]
              << " " << outArray[mifi_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] << std::endl;
    std::ofstream bilinearOut("bilinearOutData.txt");
    for (int lon = 0; lon < lonSize; ++lon) {
        for (int lat = 0; lat < latSize; ++lat) {
            bilinearOut << longitudeAxis[lon] << " " << latitudeAxis[lat] << " " << outArray[mifi_3d_array_position(lon, lat, 0, lonSize, latSize,zSize)] << std::endl;
        }
    }
#endif

    for (int i = 0; i < latSize*lonSize*zSize; ++i) {
        outArray[i] = MIFI_UNDEFINED_F;
    }
    TEST4FIMEX_CHECK_NO_THROW(reproject_f(MIFI_INTERPOL_BICUBIC, emepProj, inArray, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, iSize, jSize, zSize,
                                          latlongProj, outArray, longitudeAxis, latitudeAxis, MIFI_LONGITUDE, MIFI_LATITUDE, lonSize, latSize));
    // -25 43 32 (long, lat, val)
    TEST4FIMEX_CHECK(near(outArray[mifi_3d_array_position(9, 25, 0, lonSize, latSize, zSize)], 32, 1e-6));
#if 0
    std::cerr << "long lat val: " << longitudeAxis[9] << " " << latitudeAxis[25]
              << " " << outArray[mifi_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] << std::endl;
    std::ofstream bicubicOut("bicubicOutData.txt");
    for (int lon = 0; lon < lonSize; ++lon) {
        for (int lat = 0; lat < latSize; ++lat) {
            bicubicOut << longitudeAxis[lon] << " " << latitudeAxis[lat] << " " << outArray[mifi_3d_array_position(lon, lat, 0, lonSize, latSize,zSize)] << std::endl;
        }
    }
#endif
}

TEST4FIMEX_TEST_CASE(mifi_vector_reproject_values_rotate_90)
{
    const std::string emepProj = "+proj=stere +a=127.4 +lat_0=90 +lon_0=0 +lat_ts=60";
    const std::string emepProj2 = "+proj=stere +a=127.4 +lat_0=90 +lon_0=90 +lat_ts=60";

    double emepIAxis[5];
    double emepJAxis[5];
    double emepIOutAxis[5];
    double emepJOutAxis[5];
    float u[5*5];
    float v[5*5];
    // initialize axes around northpole
    for (int i = 0; i < 5; ++i) {
        emepIAxis[i] = i - 2;
        emepJAxis[i] = i - 2;
        emepIOutAxis[i] =  i - 2;
        emepJOutAxis[i] = i - 2;
    }
    // initialize values
    for (int i = 0; i < 5*5; ++i) {
        u[i] = i;
        v[i] = 25-i;
     }

    float uOut[5*5];
    float vOut[5*5];
    float uRot[5*5];
    float vRot[5*5];
    TEST4FIMEX_CHECK_NO_THROW(reproject_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj, u, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5, 1, emepProj2,
                                          uOut, emepIOutAxis, emepJOutAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5));
    TEST4FIMEX_CHECK_NO_THROW(reproject_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj, v, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5, 1, emepProj2,
                                          vOut, emepIOutAxis, emepJOutAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5));
    for (int i = 0; i < 5; ++i) {
        for (int j = 0; j < 5; ++j) {
            uRot[j*5+i] = uOut[j*5+i];
            vRot[j*5+i] = vOut[j*5+i];
            //std::cerr << "uOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << uOut[j*5+i] << std::endl;
            //std::cerr << "vOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << vOut[j*5+i] << std::endl;
        }
    }
    TEST4FIMEX_CHECK_NO_THROW(vector_reproject_values_f(emepProj, emepProj2, uOut, vOut, emepIOutAxis, emepJOutAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5, 1));
    for (int i = 0; i < 5; ++i) {
        for (int j = 0; j < 5; ++j) {
//            std::cerr << "uOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << uOut[j*5+i] << " " << vRot[j*5+i] << std::endl;
//            std::cerr << "vOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << vOut[j*5+i] << " " << uRot[j*5+i] << std::endl;
            // rotation of 90deg -> u->-v, v->u
TEST4FIMEX_CHECK(fabs(vRot[j * 5 + i] - uOut[j * 5 + i]) < 1e-4);
TEST4FIMEX_CHECK(fabs(uRot[j * 5 + i] + vOut[j * 5 + i]) < 1e-4);
        }
    }
}

TEST4FIMEX_TEST_CASE(mifi_vector_reproject_values_rotate_180)
{
    const std::string emepProj = "+proj=stere +a=127.4 +lat_0=90 +lon_0=0 +lat_ts=60";
    const std::string emepProj2 = "+proj=stere +a=127.4 +lat_0=90 +lon_0=180 +lat_ts=60";

    double emepIAxis[5];
    double emepJAxis[5];
    double emepIOutAxis[5];
    double emepJOutAxis[5];
    float u[5*5];
    float v[5*5];
    // initialize axes around northpole
    for (int i = 0; i < 5; ++i) {
        emepIAxis[i] = i - 2;
        emepJAxis[i] = i - 2;
        emepIOutAxis[i] =  i - 2;
        emepJOutAxis[i] = i - 2;
    }
    // initialize values
    for (int i = 0; i < 5*5; ++i) {
        u[i] = i;
        v[i] = 25-i;
     }

    float uOut[5*5];
    float vOut[5*5];
    float uRot[5*5];
    float vRot[5*5];
    TEST4FIMEX_CHECK_NO_THROW(reproject_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj, u, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5, 1, emepProj2,
                                          uOut, emepIOutAxis, emepJOutAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5));
    TEST4FIMEX_CHECK_NO_THROW(reproject_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj, v, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5, 1, emepProj2,
                                          vOut, emepIOutAxis, emepJOutAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5));
    for (int i = 0; i < 5; ++i) {
        for (int j = 0; j < 5; ++j) {
            uRot[j*5+i] = uOut[j*5+i];
            vRot[j*5+i] = vOut[j*5+i];
            //std::cerr << "uOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << uOut[j*5+i] << std::endl;
            //std::cerr << "vOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << vOut[j*5+i] << std::endl;
        }
    }
    TEST4FIMEX_CHECK_NO_THROW(vector_reproject_values_f(emepProj, emepProj2, uOut, vOut, emepIOutAxis, emepJOutAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5, 1));
    for (int i = 0; i < 5; ++i) {
        for (int j = 0; j < 5; ++j) {
            //std::cerr << "uOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << uOut[j*5+i] << std::endl;
            //std::cerr << "vOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << vOut[j*5+i] << std::endl;
            // rotation of 90deg -> u->v, v->-u
            TEST4FIMEX_CHECK(fabs(vRot[j * 5 + i] + vOut[j * 5 + i]) < 1e-5);
            TEST4FIMEX_CHECK(fabs(uRot[j * 5 + i] + uOut[j * 5 + i]) < 1e-5);
        }
    }
}

TEST4FIMEX_TEST_CASE(mifi_vector_reproject_keep_size)
{
    const std::string emepProj = "+proj=stere +a=127.4 +lat_0=90 +lon_0=-32 +lat_ts=60 +x_0=7 +y_0=109";

    double emepIAxis[4];
    double emepJAxis[4];
    double latitudeAxis[4];
    double longitudeAxis[4];
    float u[4*4];
    float v[4*4];
    // initialize axes around northpole
    for (int i = 0; i < 4; ++i) {
        emepIAxis[i] = i + 6;
    }
    for (int i = 0; i < 4; ++i) {
        emepJAxis[i] = i + 108;
    }
    for (int i = 0; i < 4; ++i) {
        longitudeAxis[i] = (i*60);
    }
    for (int i = 0; i < 4; ++i) {
        latitudeAxis[i] = (i/2.) + 88.5;
    }
    // initialize values
    for (int i = 0; i < 4*4; ++i) {
        u[i] = i;
        v[i] = -16+i;
     }

    float uOut[4*4];
    float vOut[4*4];
    float uRot[4*4];
    float vRot[4*4];
    TEST4FIMEX_CHECK_NO_THROW(reproject_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj, u, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 4, 4, 1,
                                          latlongProj, uOut, longitudeAxis, latitudeAxis, MIFI_LONGITUDE, MIFI_LATITUDE, 4, 4));
    TEST4FIMEX_CHECK_NO_THROW(reproject_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj, v, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 4, 4, 1,
                                          latlongProj, vOut, longitudeAxis, latitudeAxis, MIFI_LONGITUDE, MIFI_LATITUDE, 4, 4));
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            //std::cerr << "uOut(" << longitudeAxis[i] << "," << latitudeAxis[j] << ") = " << uOut[j*4+i] << std::endl;
            //std::cerr << "vOut(" << longitudeAxis[i] << "," << latitudeAxis[j] << ") = " << vOut[j*4+i] << std::endl;
            uRot[j*4+i] = uOut[j*4+i];
            vRot[j*4+i] = vOut[j*4+i];
        }
    }
    TEST4FIMEX_CHECK_NO_THROW(
        vector_reproject_values_f(emepProj, latlongProj, uOut, vOut, longitudeAxis, latitudeAxis, MIFI_LONGITUDE, MIFI_LATITUDE, 4, 4, 1));
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            //std::cerr << "uOut(" << longitudeAxis[i] << "," << latitudeAxis[j] << ") = " << uOut[j*4+i] << std::endl;
            //std::cerr << "vOut(" << longitudeAxis[i] << "," << latitudeAxis[j] << ") = " << vOut[j*4+i] << std::endl;
            // check equal length
            double diff2 = (uOut[j*4+i]*uOut[j*4+i] + vOut[j*4+i]*vOut[j*4+i] - uRot[j*4+i]*uRot[j*4+i] - vRot[j*4+i]*vRot[j*4+i]);
            if (!std::isnan(diff2)) {
                //std::cerr << diff2  << std::endl;
                TEST4FIMEX_CHECK(fabs(diff2) < 1e-3);
            }
        }
    }
}

TEST4FIMEX_TEST_CASE(mifi_vector_reproject_directions)
{
    const std::string emepProj = "+proj=stere +a=127.4 +lat_0=90 +lon_0=0 +lat_ts=60";

    int ox = 5;
    int oy = 5;
    int oz = 1;
    double emepIAxis[ox];
    double emepJAxis[oy];
    // in_x_field[x+oy*y] = inXAxis[x]
    double in_x_field[ox*oy];
    double in_y_field[ox*oy];
    float angles[ox*oy];
    // initialize axes around northpole
    for (int i = 0; i < 5; ++i) {
        emepIAxis[i] = (i - 2) * 1000;
        emepJAxis[i] = (i - 2) * 1000;
    }
    // initialize values
    for (int i = 0; i < 5; ++i) {
        for (int j = 0; j < 5; ++j) {
            angles[oy*j + i] = 0; // all point to y-axis
            in_x_field[i+oy*j] = emepIAxis[i];
            in_y_field[i+oy*j] = emepJAxis[j];
        }
     }


    double xOut[ox*oy];
    double yOut[ox*oy];
    TEST4FIMEX_CHECK_NO_THROW(reproject_axes(emepProj, latlongProj, emepIAxis, emepJAxis, ox, oy, xOut, yOut));
    Matrix_cp matrix;
    // calculate the positions in the original proj.
    TEST4FIMEX_CHECK_NO_THROW(matrix = get_vector_reproject_matrix_field(emepProj, latlongProj, in_x_field, in_y_field, ox, oy));

    TEST4FIMEX_CHECK_NO_THROW(vector_reproject_direction_by_matrix_f(matrix, angles, oz));
#if 0
    for (int i = 0; i < ox; ++i) {
        for (int j = 0; j < oy; ++j) {
            std::cerr << "(" << i << "," << j << ") = ("
                      <<(xOut[oy*j+i]*RAD_TO_DEG) <<"," << (yOut[oy*j+i]*RAD_TO_DEG)
                      << ") -> " << angles[oy*j+i] << std::endl;
        }
    }
#endif
    TEST4FIMEX_CHECK_CLOSE(315, angles[0 + oy * 0], 1);
    TEST4FIMEX_CHECK_CLOSE(270, angles[0 + oy * 2], 1);
    TEST4FIMEX_CHECK_CLOSE(225, angles[0 + oy * 4], 1);
    float out = angles[2 + oy*0];
    if (out > 300)
        out -= 360;
    TEST4FIMEX_CHECK_CLOSE(10, 10 + out, 1);
    out = angles[2 + oy*1];
    if (out > 300)
        out -= 360;
    TEST4FIMEX_CHECK_CLOSE(10, 10 + out, 1);
    TEST4FIMEX_CHECK_CLOSE(180, angles[2 + oy * 3], 1);
    TEST4FIMEX_CHECK_CLOSE(180, angles[2 + oy * 4], 1);

    TEST4FIMEX_CHECK_CLOSE(45, angles[4 + oy * 0], 1);
    TEST4FIMEX_CHECK_CLOSE(90, angles[4 + oy * 2], 1);
    TEST4FIMEX_CHECK_CLOSE(135, angles[4 + oy * 4], 1);
}

TEST4FIMEX_TEST_CASE(linear_no_extrapol)
{
    const float in0 = 200, in1 = 300;
    const double a = 2, b = 3;
    float out = 12345;
    mifi_get_values_linear_no_extrapol_f(&in0, &in1, &out, 1, a, b, 0.5);
    TEST4FIMEX_CHECK(std::isnan(out));
    mifi_get_values_linear_no_extrapol_f(&in0, &in1, &out, 1, a, b, 1.5);
    TEST4FIMEX_CHECK(std::isnan(out));
    mifi_get_values_linear_no_extrapol_f(&in0, &in1, &out, 1, a, b, 2.5);
    TEST4FIMEX_CHECK(near(out, 250, 0.01));
    mifi_get_values_linear_no_extrapol_f(&in0, &in1, &out, 1, a, b, 3.5);
    TEST4FIMEX_CHECK(std::isnan(out));
    mifi_get_values_linear_no_extrapol_f(&in0, &in1, &out, 1, a, b, 4.5);
    TEST4FIMEX_CHECK(std::isnan(out));
}

TEST4FIMEX_TEST_CASE(linear_const_extrapol)
{
    const float in0 = 200, in1 = 300;
    const double a = 2, b = 3;
    float out = 12345;
    mifi_get_values_linear_const_extrapol_f(&in0, &in1, &out, 1, a, b, 0.5);
    TEST4FIMEX_CHECK(near(out, 200, 0.01));
    mifi_get_values_linear_const_extrapol_f(&in0, &in1, &out, 1, a, b, 1.5);
    TEST4FIMEX_CHECK(near(out, 200, 0.01));
    mifi_get_values_linear_const_extrapol_f(&in0, &in1, &out, 1, a, b, 2.5);
    TEST4FIMEX_CHECK(near(out, 250, 0.01));
    mifi_get_values_linear_const_extrapol_f(&in0, &in1, &out, 1, a, b, 3.5);
    TEST4FIMEX_CHECK(near(out, 300, 0.01));
    mifi_get_values_linear_const_extrapol_f(&in0, &in1, &out, 1, a, b, 4.5);
    TEST4FIMEX_CHECK(near(out, 300, 0.01));
}

TEST4FIMEX_TEST_CASE(linear_weak_extrapol)
{
    const float in0 = 200, in1 = 300;
    const double a = 2, b = 3;
    float out = 12345;
    mifi_get_values_linear_weak_extrapol_f(&in0, &in1, &out, 1, a, b, 0.5);
    TEST4FIMEX_CHECK(std::isnan(out));
    mifi_get_values_linear_weak_extrapol_f(&in0, &in1, &out, 1, a, b, 1.5);
    TEST4FIMEX_CHECK(near(out, 150, 0.01));
    mifi_get_values_linear_weak_extrapol_f(&in0, &in1, &out, 1, a, b, 2.5);
    TEST4FIMEX_CHECK(near(out, 250, 0.01));
    mifi_get_values_linear_weak_extrapol_f(&in0, &in1, &out, 1, a, b, 3.5);
    TEST4FIMEX_CHECK(near(out, 350, 0.01));
    mifi_get_values_linear_weak_extrapol_f(&in0, &in1, &out, 1, a, b, 4.5);
    TEST4FIMEX_CHECK(std::isnan(out));
}

TEST4FIMEX_TEST_CASE(linear)
{
    const float in0 = 200, in1 = 300;
    const double a = 2, b = 3;
    float out = 12345;
    mifi_get_values_linear_f(&in0, &in1, &out, 1, a, b, 0.5);
    TEST4FIMEX_CHECK(near(out, 50, 0.01));
    mifi_get_values_linear_f(&in0, &in1, &out, 1, a, b, 1.5);
    TEST4FIMEX_CHECK(near(out, 150, 0.01));
    mifi_get_values_linear_f(&in0, &in1, &out, 1, a, b, 2.5);
    TEST4FIMEX_CHECK(near(out, 250, 0.01));
    mifi_get_values_linear_f(&in0, &in1, &out, 1, a, b, 3.5);
    TEST4FIMEX_CHECK(near(out, 350, 0.01));
    mifi_get_values_linear_f(&in0, &in1, &out, 1, a, b, 4.5);
    TEST4FIMEX_CHECK(near(out, 450, 0.01));
}

TEST4FIMEX_TEST_CASE(binary_search)
{
    const int N = 10;
    const double values[N] = {1, 3, 5, 7, 9, 13, 17, 21, 24, 28};
    TEST4FIMEX_CHECK_EQ(3, bsearchDoubleIndex(7, values, N, ascendingDoubleComparator));
    TEST4FIMEX_CHECK_EQ(-5, bsearchDoubleIndex(8, values, N, ascendingDoubleComparator));
}
