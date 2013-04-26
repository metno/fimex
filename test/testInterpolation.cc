/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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

#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#include "fimex/interpolation.h"

#include <cmath>
#include <iostream>
#include <fstream>
#include <string>

#include "fimex/CDMAttribute.h"
#include "fimex/Data.h"

// definitions from proj_api.h
#define RAD_TO_DEG      57.29577951308232

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK

#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

using boost::unit_test_framework::test_suite;


BOOST_AUTO_TEST_CASE( test_mifi_points2position )
{
    double axis[5] = {1., 2., 3., 4., 5.};
    double points[5] = {-3., 5., 1.3, 2., 6.};
    double apoints[5] = {-4., 4., 0.3, 1., 5.}; // results
    mifi_points2position(points,5,axis,5,MIFI_PROJ_AXIS);
    for (int i = 0; i < 5; ++i) {
        //BOOST_CHECK_CLOSE not implemented in FC5 boost
        //BOOST_CHECK_CLOSE(apoints[i], points[i], /* tolerance */ 1e-10);

        double res = std::fabs(apoints[i] - points[i]);
        //std::cerr << apoints[i] << "-" << points[i] << "=" << res << std::endl;
        BOOST_CHECK((res < 1e-10));
    }
}

BOOST_AUTO_TEST_CASE( test_mifi_points2position_reverse )
{
    double axis[5] = {5., 4., 3., 2., 1.};
    double points[5] = {-3., 5., 1.3, 2., 6.};
    double apoints[5] = {8., 0., 3.7, 3., -1.}; // results
    mifi_points2position(points,5,axis,5,MIFI_PROJ_AXIS);
    for (int i = 0; i < 5; ++i) {
        //BOOST_CHECK_CLOSE not implemented in FC5 boost
        //BOOST_CHECK_CLOSE(apoints[i], points[i], /* tolerance */ 1e-10);

        double res = std::fabs(apoints[i] - points[i]);
        //std::cerr << apoints[i] << "-" << points[i] << "=" << res << std::endl;
        BOOST_CHECK((res < 1e-10));
    }
}

BOOST_AUTO_TEST_CASE( test_mifi_get_values_f )
{
    float infield[4] = {1., 2., 1., 2.}; // (0,0), (0,1), (1,0), (1,1) #(y,x)
    float outvalues[1];

    mifi_get_values_f(infield, outvalues, 0.3, 0.3, 2, 2, 1);
    // std::cerr << outvalues[0] << std::endl;
    BOOST_CHECK(std::fabs(outvalues[0] - 1.) < 1e-10);
}

BOOST_AUTO_TEST_CASE( test_mifi_get_values_bilinear_f )
{
    float infield[4] = {1., 2., 2., 1+sqrt(2.)}; // (0,0), (0,1), (1,0), (1,1) #(y,x)
    float outvalues[1];

    mifi_get_values_bilinear_f(infield, outvalues, 0.3, 0., 2, 2, 1);
    BOOST_CHECK(std::fabs(outvalues[0] - 1.3) < 1e-6);
    mifi_get_values_bilinear_f(infield, outvalues, 0.3, 0.0001, 2, 2, 1);
    //std::cerr << outvalues[0] << std::endl;
    BOOST_CHECK(std::fabs(outvalues[0] - 1.3) < 1e-4);
    mifi_get_values_bilinear_f(infield, outvalues, 0., 0.3, 2, 2, 1);
    BOOST_CHECK(std::fabs(outvalues[0] - 1.3) < 1e-6);
    mifi_get_values_bilinear_f(infield, outvalues, 0.0001, 0.3, 2, 2, 1);
    //std::cerr << outvalues[0] << std::endl;
    BOOST_CHECK(std::fabs(outvalues[0] - 1.3) < 1e-4);

    // check for border values / nan
    mifi_get_values_bilinear_f(infield, outvalues, 0, 0, 2, 2, 1);
    BOOST_CHECK(!mifi_isnanf(outvalues[0]));
    mifi_get_values_bilinear_f(infield, outvalues, 1, 1, 2, 2, 1);
    BOOST_CHECK(!mifi_isnanf(outvalues[0]));
    mifi_get_values_bilinear_f(infield, outvalues, 1.5, 0.5, 2, 2, 1);
    BOOST_CHECK(mifi_isnanf(outvalues[0]));
    mifi_get_values_bilinear_f(infield, outvalues, 0.5, 1.5, 2, 2, 1);
    BOOST_CHECK(mifi_isnanf(outvalues[0]));
    mifi_get_values_bilinear_f(infield, outvalues, 0.5, -0.5, 2, 2, 1);
    BOOST_CHECK(mifi_isnanf(outvalues[0]));
    mifi_get_values_bilinear_f(infield, outvalues, -0.5, 0.5, 2, 2, 1);
    BOOST_CHECK(mifi_isnanf(outvalues[0]));

}

BOOST_AUTO_TEST_CASE( test_mifi_get_values_bicubic_f )
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
    BOOST_CHECK_CLOSE(2.f, outvalues[0], 1e-3);
    mifi_get_values_bicubic_f(infield, outvalues, 1, 1.99999, 4, 4, 1);
    BOOST_CHECK_CLOSE(2.f, outvalues[0], 1e-3);
    mifi_get_values_bicubic_f(infield, outvalues, 1, 1.5, 4, 4, 1);
    BOOST_CHECK_CLOSE(2.125f, outvalues[0], 1e-3);
    mifi_get_values_bicubic_f(infield, outvalues, 1.5, 1, 4, 4, 1);
    BOOST_CHECK_CLOSE(2.f, outvalues[0], 1e-3);

    // and transposed
    mifi_get_values_bicubic_f(infield_t, outvalues, 1, 1, 4, 4, 1);
    BOOST_CHECK_CLOSE(2.f, outvalues[0], 1e-3);
    mifi_get_values_bicubic_f(infield_t, outvalues, 1.99999, 1, 4, 4, 1);
    BOOST_CHECK_CLOSE(2.f, outvalues[0], 1e-3);
    mifi_get_values_bicubic_f(infield_t, outvalues, 1.5, 1, 4, 4, 1);
    BOOST_CHECK_CLOSE(2.125f, outvalues[0], 1e-3);
    mifi_get_values_bicubic_f(infield_t, outvalues, 1, 1.5, 4, 4, 1);
    BOOST_CHECK_CLOSE(2.f, outvalues[0], 1e-3);

    // check for border values / nan
    mifi_get_values_bicubic_f(infield, outvalues, .5, 1, 4, 4, 1);
    BOOST_CHECK(mifi_isnanf(outvalues[0]));
    mifi_get_values_bicubic_f(infield, outvalues, 1, .5, 4, 4, 1);
    BOOST_CHECK(mifi_isnanf(outvalues[0]));
    mifi_get_values_bicubic_f(infield, outvalues, 2.5, 1, 4, 4, 1);
    BOOST_CHECK(mifi_isnanf(outvalues[0]));
    mifi_get_values_bicubic_f(infield, outvalues, 1, 2.5, 4, 4, 1);
    BOOST_CHECK(mifi_isnanf(outvalues[0]));

}

BOOST_AUTO_TEST_CASE( test_mifi_get_values_linear_f )
{
    const int nr = 4;
    float infieldA[nr] = {0, 1, -1, 1};
    float infieldB[nr] = {1, -1, 0, 1};
    float outfield[nr];
    // infieldA and infieldB at same position (a=b), take field1
    mifi_get_values_linear_f(infieldA, infieldB, outfield, nr, 1., 1., .5);
    for (int i = 0; i < nr; i++) {
        BOOST_CHECK_CLOSE(outfield[i], infieldA[i], 1e-5);
    }

    // real values between a and b
    mifi_get_values_linear_f(infieldA, infieldB, outfield, nr, 1., 2., 1.5);
    for (int i = 0; i < nr; i++) {
        BOOST_CHECK_CLOSE(outfield[i], (float).5*(infieldA[i]+infieldB[i]), 1e-5);
    }


    // extrapolation values between a and b
    mifi_get_values_linear_f(infieldA, infieldB, outfield, nr, 0., 1., 2.);
    for (int i = 0; i < nr; i++) {
        BOOST_CHECK_CLOSE(outfield[i], (float)infieldA[i]+2*(infieldB[i]-infieldA[i]), 1e-5);
    }
}

BOOST_AUTO_TEST_CASE( test_mifi_get_values_linear_d )
{
    const int nr = 4;
    double infieldA[nr] = {0, 1, -1, 1};
    double infieldB[nr] = {1, -1, 0, 1};
    double outfield[nr];
    // infieldA and infieldB at same position (a=b), take field1
    mifi_get_values_linear_d(infieldA, infieldB, outfield, nr, 1., 1., .5);
    for (int i = 0; i < nr; i++) {
        BOOST_CHECK_CLOSE(outfield[i], infieldA[i], 1e-5);
    }

    // real values between a and b
    mifi_get_values_linear_d(infieldA, infieldB, outfield, nr, 1., 2., 1.5);
    for (int i = 0; i < nr; i++) {
        BOOST_CHECK_CLOSE(outfield[i], .5*(infieldA[i]+infieldB[i]), 1e-5);
    }


    // extrapolation values between a and b
    mifi_get_values_linear_d(infieldA, infieldB, outfield, nr, 0., 1., 2.);
    for (int i = 0; i < nr; i++) {
        BOOST_CHECK_CLOSE(outfield[i], infieldA[i]+2*(infieldB[i]-infieldA[i]), 1e-5);
    }
}


BOOST_AUTO_TEST_CASE( test_mifi_get_values_log_f )
{
    const int nr = 1;
    float infieldA[nr] = {1000.};
    float infieldB[nr] = {100.};
    float outfield[nr];
    // corner value 100.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 100.);
    BOOST_CHECK_CLOSE(outfield[0], 100.f, 1e-3);
    // corner value 1000.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 1000.);
    BOOST_CHECK_CLOSE(outfield[0], 1000.f, 1e-3);
    // interpolation at 500.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 500.);
    BOOST_CHECK_CLOSE(outfield[0], 729.073f, 1e-3);
    // extrapolation at 1500.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 1500.);
    BOOST_CHECK_CLOSE(outfield[0], 1158.482f, 1e-3);
    // interpolation at 200.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 200.);
    BOOST_CHECK_CLOSE(outfield[0], 370.927f, 1e-3);
    // interpolation at 800.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 800.);
    BOOST_CHECK_CLOSE(outfield[0], 912.781f, 1e-3);
}

BOOST_AUTO_TEST_CASE( test_mifi_get_values_log_log_f )
{
    const int nr = 1;
    float infieldA[nr] = {1000.};
    float infieldB[nr] = {100.};
    float outfield[nr];
    // corner value 100.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 100.);
    BOOST_CHECK_CLOSE(outfield[0], 100.f, 1e-3);
    // corner value 1000.
    mifi_get_values_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 1000.);
    BOOST_CHECK_CLOSE(outfield[0], 1000.f, 1e-3);

    // below are results from NCLs vintp2p_ecmwf
    // interpolation at 500.
    mifi_get_values_log_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 500.);
    BOOST_CHECK_CLOSE(outfield[0], 763.1873f, 1e-3);
    // interpolation at 200.
    mifi_get_values_log_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 200.);
    BOOST_CHECK_CLOSE(outfield[0], 408.0904f, 1e-3);
    // interpolation at 800.
    mifi_get_values_log_log_f(infieldA, infieldB, outfield, nr, 1000., 100., 800.);
    BOOST_CHECK_CLOSE(outfield[0], 926.384f, 1e-3);
}



BOOST_AUTO_TEST_CASE( test_mifi_project_axes)
{
    std::string emepProj("+ellps=sphere +a=127.4 +e=0 +proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +x_0=7 +y_0=109");
    std::string latlongProj("+ellps=sphere +a=6370 +e=0 +proj=latlong");

    double emepX[] = {6,7,8};
    double emepY[] = {108,109,110};
    double outX[9];
    double outY[9];
    BOOST_CHECK(MIFI_OK == mifi_project_axes(emepProj.c_str(), latlongProj.c_str(), &emepX[0], &emepY[0], 3, 3, &outX[0], &outY[0]));
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 3; j++) BOOST_CHECK((RAD_TO_DEG * outY[j+3*i]) > 89);

}

BOOST_AUTO_TEST_CASE( test_mifi_interpolate_f )
{
    const int iSize = 170;
    const int jSize = 150;
    const int zSize = 1;
    const int lonSize = 180;
    const int latSize = 90;
    float inArray[iSize*jSize*zSize]; // emep i, j, z
    float outArray[lonSize*latSize*zSize]; // long, lat, z
    std::string emepProj("+ellps=sphere +a=127.4 +e=0 +proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +x_0=7 +y_0=109");
    std::string latlongProj("+ellps=sphere +a=6370 +e=0 +proj=latlong");
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
    std::string topSrcDir(TOP_SRCDIR);
    std::ifstream datafile (std::string(topSrcDir+"/test/inData.txt").c_str(), std::ios::in);
    if (datafile.is_open()) {
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
            } catch (std::ifstream::failure& fail) {
                if (datafile.eof()) {
                    break;
                } else {
                    std::cerr << "Exception in line: " << line << "\t" << x << " " << y << " " << country << std::endl;
                    assert(false);
                }
            }
        }
        datafile.close();
    } else {
        std::cerr << "inData.txt: no such file" << std::endl;
        assert(false);
    }
    assert(std::fabs(inArray[mifi_3d_array_position(93, 50, 0, iSize, jSize, zSize)] - 4) < 1e-5);

    //std::cerr << "emepProj: " << emepProj.c_str() << " latlonProj: " << latlongProj.c_str() << std::endl;
    BOOST_CHECK(
    mifi_interpolate_f(MIFI_INTERPOL_NEAREST_NEIGHBOR,
                       emepProj.c_str(), inArray, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, iSize, jSize, zSize,
                       latlongProj.c_str(), outArray, longitudeAxis, latitudeAxis, MIFI_LONGITUDE, MIFI_LATITUDE, lonSize, latSize)
    == MIFI_OK);
    // -25 43 32 (long, lat, val)
    BOOST_CHECK(std::fabs(outArray[mifi_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] - 32) < 1e-6);
    //std::cerr << "long lat val: " << longitudeAxis[9] << " " << latitudeAxis[25] << " " << outArray[mifi_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] << std::endl;
    //std::cerr << "emepProj: " << emepProj.c_str() << " latlonProj: " << latlongProj.c_str() << std::endl;

    for (int i = 0; i < latSize*lonSize*zSize; ++i) {
        outArray[i] = MIFI_UNDEFINED_F;
    }
    BOOST_CHECK(
    mifi_interpolate_f(MIFI_INTERPOL_BILINEAR,
                       emepProj.c_str(), inArray, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, iSize, jSize, zSize,
                       latlongProj.c_str(), outArray, longitudeAxis, latitudeAxis, MIFI_LONGITUDE, MIFI_LATITUDE, lonSize, latSize)
    == MIFI_OK);
    // -25 43 32 (long, lat, val)
    BOOST_CHECK(std::fabs(outArray[mifi_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] - 32) < 1e-6);
    std::cerr << "long lat val: " << longitudeAxis[9] << " " << latitudeAxis[25] << " " << outArray[mifi_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] << std::endl;

    std::ofstream bilinearOut (std::string(topSrcDir+"/test/bilinearOutData.txt").c_str());
    for (int lon = 0; lon < lonSize; ++lon) {
        for (int lat = 0; lat < latSize; ++lat) {
            bilinearOut << longitudeAxis[lon] << " " << latitudeAxis[lat] << " " << outArray[mifi_3d_array_position(lon, lat, 0, lonSize, latSize,zSize)] << std::endl;
        }
    }


    for (int i = 0; i < latSize*lonSize*zSize; ++i) {
        outArray[i] = MIFI_UNDEFINED_F;
    }
    BOOST_CHECK(
    mifi_interpolate_f(MIFI_INTERPOL_BICUBIC,
                       emepProj.c_str(), inArray, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, iSize, jSize, zSize,
                       latlongProj.c_str(), outArray, longitudeAxis, latitudeAxis, MIFI_LONGITUDE, MIFI_LATITUDE, lonSize, latSize)
    == MIFI_OK);
    // -25 43 32 (long, lat, val)
    BOOST_CHECK(std::fabs(outArray[mifi_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] - 32) < 1e-6);
    std::cerr << "long lat val: " << longitudeAxis[9] << " " << latitudeAxis[25] << " " << outArray[mifi_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] << std::endl;

    std::ofstream bicubicOut (std::string(topSrcDir+"/test/bicubicOutData.txt").c_str());
    for (int lon = 0; lon < lonSize; ++lon) {
        for (int lat = 0; lat < latSize; ++lat) {
            bicubicOut << longitudeAxis[lon] << " " << latitudeAxis[lat] << " " << outArray[mifi_3d_array_position(lon, lat, 0, lonSize, latSize,zSize)] << std::endl;
        }
    }
}

BOOST_AUTO_TEST_CASE( test_mifi_vector_reproject_values_rotate_90 )
{
    std::string emepProj("+ellps=sphere +a=127.4 +e=0 +proj=stere +lat_0=90 +lon_0=0 +lat_ts=60");
    std::string emepProj2("+ellps=sphere +a=127.4 +e=0 +proj=stere +lat_0=90 +lon_0=90 +lat_ts=60");
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
    mifi_interpolate_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj.c_str(), u, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5, 1, emepProj2.c_str(), uOut, emepIOutAxis, emepJOutAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5);
    mifi_interpolate_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj.c_str(), v, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5, 1, emepProj2.c_str(), vOut, emepIOutAxis, emepJOutAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5);
    for (int i = 0; i < 5; ++i) {
        for (int j = 0; j < 5; ++j) {
            uRot[j*5+i] = uOut[j*5+i];
            vRot[j*5+i] = vOut[j*5+i];
            //std::cerr << "uOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << uOut[j*5+i] << std::endl;
            //std::cerr << "vOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << vOut[j*5+i] << std::endl;
        }
    }
    mifi_vector_reproject_values_f(MIFI_VECTOR_KEEP_SIZE, emepProj.c_str(), emepProj2.c_str(), uOut, vOut, emepIOutAxis, emepJOutAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5, 1);
    for (int i = 0; i < 5; ++i) {
        for (int j = 0; j < 5; ++j) {
//            std::cerr << "uOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << uOut[j*5+i] << " " << vRot[j*5+i] << std::endl;
//            std::cerr << "vOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << vOut[j*5+i] << " " << uRot[j*5+i] << std::endl;
            // rotation of 90deg -> u->-v, v->u
            BOOST_CHECK(fabs(vRot[j*5+i] - uOut[j*5+i]) < 1e-4);
            BOOST_CHECK(fabs(uRot[j*5+i] + vOut[j*5+i]) < 1e-4);
        }
    }
    BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE( test_mifi_vector_reproject_values_rotate_180 )
{
    std::string emepProj("+ellps=sphere +a=127.4 +e=0 +proj=stere +lat_0=90 +lon_0=0 +lat_ts=60");
    std::string emepProj2("+ellps=sphere +a=127.4 +e=0 +proj=stere +lat_0=90 +lon_0=180 +lat_ts=60");
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
    mifi_interpolate_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj.c_str(), u, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5, 1, emepProj2.c_str(), uOut, emepIOutAxis, emepJOutAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5);
    mifi_interpolate_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj.c_str(), v, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5, 1, emepProj2.c_str(), vOut, emepIOutAxis, emepJOutAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5);
    for (int i = 0; i < 5; ++i) {
        for (int j = 0; j < 5; ++j) {
            uRot[j*5+i] = uOut[j*5+i];
            vRot[j*5+i] = vOut[j*5+i];
            //std::cerr << "uOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << uOut[j*5+i] << std::endl;
            //std::cerr << "vOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << vOut[j*5+i] << std::endl;
        }
    }
    mifi_vector_reproject_values_f(MIFI_VECTOR_KEEP_SIZE, emepProj.c_str(), emepProj2.c_str(), uOut, vOut, emepIOutAxis, emepJOutAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 5, 5, 1);
    for (int i = 0; i < 5; ++i) {
        for (int j = 0; j < 5; ++j) {
            //std::cerr << "uOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << uOut[j*5+i] << std::endl;
            //std::cerr << "vOut(" << emepIOutAxis[i] << "," << emepJOutAxis[j] << ") = " << vOut[j*5+i] << std::endl;
            // rotation of 90deg -> u->v, v->-u
            BOOST_CHECK(fabs(vRot[j*5+i] + vOut[j*5+i]) < 1e-5);
            BOOST_CHECK(fabs(uRot[j*5+i] + uOut[j*5+i]) < 1e-5);
        }
    }
    BOOST_CHECK(true);
}


BOOST_AUTO_TEST_CASE( test_mifi_vector_reproject_keep_size )
{
    std::string emepProj("+ellps=sphere +a=127.4 +e=0 +proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +x_0=7 +y_0=109");
    std::string latlongProj("+ellps=sphere +a=6370 +e=0 +proj=latlong");
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
    mifi_interpolate_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj.c_str(), u, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 4, 4, 1, latlongProj.c_str(), uOut, longitudeAxis, latitudeAxis, MIFI_LONGITUDE, MIFI_LATITUDE, 4, 4);
    mifi_interpolate_f(MIFI_INTERPOL_NEAREST_NEIGHBOR, emepProj.c_str(), v, emepIAxis, emepJAxis, MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, 4, 4, 1, latlongProj.c_str(), vOut, longitudeAxis, latitudeAxis, MIFI_LONGITUDE, MIFI_LATITUDE, 4, 4);
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            //std::cerr << "uOut(" << longitudeAxis[i] << "," << latitudeAxis[j] << ") = " << uOut[j*4+i] << std::endl;
            //std::cerr << "vOut(" << longitudeAxis[i] << "," << latitudeAxis[j] << ") = " << vOut[j*4+i] << std::endl;
            uRot[j*4+i] = uOut[j*4+i];
            vRot[j*4+i] = vOut[j*4+i];
        }
    }
    mifi_vector_reproject_values_f(MIFI_VECTOR_KEEP_SIZE, emepProj.c_str(), latlongProj.c_str(), uOut, vOut, longitudeAxis, latitudeAxis, MIFI_LONGITUDE, MIFI_LATITUDE, 4, 4, 1);
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            //std::cerr << "uOut(" << longitudeAxis[i] << "," << latitudeAxis[j] << ") = " << uOut[j*4+i] << std::endl;
            //std::cerr << "vOut(" << longitudeAxis[i] << "," << latitudeAxis[j] << ") = " << vOut[j*4+i] << std::endl;
            // check equal length
            double diff2 = (uOut[j*4+i]*uOut[j*4+i] + vOut[j*4+i]*vOut[j*4+i] - uRot[j*4+i]*uRot[j*4+i] - vRot[j*4+i]*vRot[j*4+i]);
            if (!mifi_isnand(diff2)) {
                //std::cerr << diff2  << std::endl;
                BOOST_CHECK(fabs(diff2) < 1e-3);
            }
        }
    }
    BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE( test_Utils )
{
    std::vector<MetNoFimex::CDMAttribute> attrs = MetNoFimex::projStringToAttributes("+ellps=sphere +a=127.4 +e=0 +proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +x_0=7 +y_0=109");
    int found = 4;
    for (std::vector<MetNoFimex::CDMAttribute>::iterator it = attrs.begin(); it != attrs.end(); ++it) {
        if (it->getName() == "grid_mapping_name") {
            found--;
            std::cerr << it->getStringValue() << ":" << std::endl;
            BOOST_CHECK((it->getStringValue() == "stereographic") || (it->getStringValue() == "polar_stereographic"));
        }
        if (it->getName() == "scale_factor_at_projection_origin") {
            found--;
            BOOST_CHECK(std::fabs(it->getData()->asDouble()[0] - 0.93301) < 0.00001);
        }
        if ((it->getName() == "longitude_of_projection_origin") || (it->getName() == "straight_vertical_longitude_from_pole")) {
            found--;
            BOOST_CHECK(std::fabs(it->getData()->asDouble()[0] - -32.) < 0.00001);
        }
        if (it->getName() == "latitude_of_projection_origin") {
            found--;
            BOOST_CHECK(std::fabs(it->getData()->asDouble()[0] - 90.) < 0.00001);
        }
    }
    BOOST_CHECK(found == 0);
}


#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif

