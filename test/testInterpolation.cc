#include "../src/interpolation.h"

#include <cmath>
#include <iostream>
#include <fstream>
#include <string>

#include "Utils.h"

#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;


void test_miup_points2position()
{
	double axis[5] = {1., 2., 3., 4., 5.};
	double points[5] = {-3., 5., 1.3, 2., 6.};
	double apoints[5] = {-4., 4., 0.3, 1., 5.}; // results
	miup_points2position(points,5,axis,5,MIUP_PROJ_AXIS);
	for (int i = 0; i < 5; ++i) {
		//BOOST_CHECK_CLOSE not implemented in FC5 boost
		//BOOST_CHECK_CLOSE(apoints[i], points[i], /* tolerance */ 1e-10);
		
		double res = std::fabs(apoints[i] - points[i]);
		//std::cerr << apoints[i] << "-" << points[i] << "=" << res << std::endl;		
		BOOST_CHECK((res < 1e-10));
	}
}

void test_miup_points2position_reverse()
{
	double axis[5] = {5., 4., 3., 2., 1.};
	double points[5] = {-3., 5., 1.3, 2., 6.};
	double apoints[5] = {8., 0., 3.7, 3., -1.}; // results
	miup_points2position(points,5,axis,5,MIUP_PROJ_AXIS);
	for (int i = 0; i < 5; ++i) {
		//BOOST_CHECK_CLOSE not implemented in FC5 boost
		//BOOST_CHECK_CLOSE(apoints[i], points[i], /* tolerance */ 1e-10);
		
		double res = std::fabs(apoints[i] - points[i]);
		//std::cerr << apoints[i] << "-" << points[i] << "=" << res << std::endl;
		BOOST_CHECK((res < 1e-10));
	}
}

void test_miup_get_values_f()
{
	float infield[4] = {1., 2., 1., 2.}; // (0,0), (0,1), (1,0), (1,1) #(y,x)
	float outvalues[1];
	
	miup_get_values_f(infield, outvalues, 0.3, 0.3, 2, 2, 1);
	// std::cerr << outvalues[0] << std::endl;
	BOOST_CHECK(std::fabs(outvalues[0] - 1.) < 1e-10);
}

void test_miup_get_values_bilinear_f()
{
	float infield[4] = {1., 2., 2., 1+sqrt(2)}; // (0,0), (0,1), (1,0), (1,1) #(y,x)
	float outvalues[1];
	
	miup_get_values_bilinear_f(infield, outvalues, 0.3, 0., 2, 2, 1);
	BOOST_CHECK(std::fabs(outvalues[0] - 1.3) < 1e-6);
	miup_get_values_bilinear_f(infield, outvalues, 0.3, 0.0001, 2, 2, 1);
	//std::cerr << outvalues[0] << std::endl;
	BOOST_CHECK(std::fabs(outvalues[0] - 1.3) < 1e-4);
	miup_get_values_bilinear_f(infield, outvalues, 0., 0.3, 2, 2, 1);
	BOOST_CHECK(std::fabs(outvalues[0] - 1.3) < 1e-6);
	miup_get_values_bilinear_f(infield, outvalues, 0.0001, 0.3, 2, 2, 1);
	//std::cerr << outvalues[0] << std::endl;
	BOOST_CHECK(std::fabs(outvalues[0] - 1.3) < 1e-4);
}

void test_miup_interpolate_f()
{
	const int iSize = 170;
	const int jSize = 150;
	const int zSize = 1;
	const int lonSize = 180;
	const int latSize = 90;
	float inArray[iSize*jSize*zSize]; // emep i, j, z
	float outArray[lonSize*latSize*zSize]; // long, lat, z
	std::string emepProj("+elips=sphere +a=127.4 +e=0 +proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +x_0=7 +y_0=109");
	std::string latlongProj("+elips=sphere +a=6370 +e=0 +proj=latlong");
	double emepIAxis[iSize];
	double emepJAxis[jSize];
	double latitudeAxis[latSize];
	double longitudeAxis[lonSize];
	
	// initialization to undefined
	for (int i = 0; i < iSize*jSize*zSize; ++i) {
		inArray[i] = MIUP_UNDEFINED_F;
	}
	for (int i = 0; i < latSize*lonSize*zSize; ++i) {
		outArray[i] = MIUP_UNDEFINED_F;
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
	std::ifstream datafile ("inData.txt", std::ios::in);
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
				inArray[miup_3d_array_position(x-1,y-1,0,iSize,jSize,zSize)] = country;
			} catch (std::ifstream::failure fail) {
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
	assert(std::fabs(inArray[miup_3d_array_position(93, 50, 0, iSize, jSize, zSize)] - 4) < 1e-5);
	
	//std::cerr << "emepProj: " << emepProj.c_str() << " latlonProj: " << latlongProj.c_str() << std::endl;
	BOOST_CHECK(
	miup_interpolate_f(MIUP_NEAREST_NEIGHBOR,
					   emepProj.c_str(), inArray, emepIAxis, emepJAxis, MIUP_PROJ_AXIS, MIUP_PROJ_AXIS, iSize, jSize, zSize,
					   latlongProj.c_str(), outArray, longitudeAxis, latitudeAxis, MIUP_LONGITUDE, MIUP_LATITUDE, lonSize, latSize)
	== MIUP_OK);
	// -25 43 32 (long, lat, val)
	BOOST_CHECK(std::fabs(outArray[miup_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] - 32) < 1e-6);	    
	//std::cerr << "long lat val: " << longitudeAxis[9] << " " << latitudeAxis[25] << " " << outArray[miup_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] << std::endl;
	//std::cerr << "emepProj: " << emepProj.c_str() << " latlonProj: " << latlongProj.c_str() << std::endl;

	for (int i = 0; i < latSize*lonSize*zSize; ++i) {
		outArray[i] = MIUP_UNDEFINED_F;
	}
	BOOST_CHECK(
	miup_interpolate_f(MIUP_BILINEAR,
					   emepProj.c_str(), inArray, emepIAxis, emepJAxis, MIUP_PROJ_AXIS, MIUP_PROJ_AXIS, iSize, jSize, zSize,
					   latlongProj.c_str(), outArray, longitudeAxis, latitudeAxis, MIUP_LONGITUDE, MIUP_LATITUDE, lonSize, latSize)
	== MIUP_OK);
	// -25 43 32 (long, lat, val)
	BOOST_CHECK(std::fabs(outArray[miup_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] - 32) < 1e-6);				    
	std::cerr << "long lat val: " << longitudeAxis[9] << " " << latitudeAxis[25] << " " << outArray[miup_3d_array_position(9, 25, 0, lonSize, latSize, zSize)] << std::endl;
	
//	for (int lon = 0; lon < lonSize; ++lon) {
//		for (int lat = 0; lat < latSize; ++lat) {
//			std::cout << longitudeAxis[lon] << " " << latitudeAxis[lat] << " " << outArray[miup_3d_array_position(lon, lat, 0, lonSize, latSize,zSize)] << std::endl;
//		}
//	}	
}

void test_Utils() {
	std::vector<MetNoUtplukk::CDMAttribute> attrs = MetNoUtplukk::projStringToAttributes("+elips=sphere +a=127.4 +e=0 +proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +x_0=7 +y_0=109");
	int found = 4;
	for (std::vector<MetNoUtplukk::CDMAttribute>::iterator it = attrs.begin(); it != attrs.end(); ++it) {
		if (it->getName() == "grid_mapping_name") {
			found--;
			BOOST_CHECK(0 == (it->getStringValue() == "stereographic"));
		}
		if (it->getName() == "scale_factor_at_projection_origin") {
			found--;
			BOOST_CHECK(std::fabs(it->getData()->asDouble()[0] - 0.93301) < 0.00001);
		}
		if (it->getName() == "longitude_of_projection_origin") {
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


test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );

    test->add( BOOST_TEST_CASE( &test_miup_points2position ) );
	test->add( BOOST_TEST_CASE( &test_miup_points2position_reverse ) );
	test->add( BOOST_TEST_CASE( &test_miup_get_values_f ) );
	test->add( BOOST_TEST_CASE( &test_miup_get_values_bilinear_f ) );
	test->add( BOOST_TEST_CASE( &test_miup_interpolate_f ) );
	test->add( BOOST_TEST_CASE( &test_Utils ) );
    return test;
}

