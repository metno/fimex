#include "../src/config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>

#include "FeltCDMReader.h"
#include "NetCDF_CDMWriter.h"
#include "CDMInterpolator.h"
#include "interpolation.h"

using namespace std;
using namespace MetNoUtplukk;

void
test_interpolator() {
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader("flth00.dat", "../share/etc/felt2nc_variables.xml"));
	boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(feltReader));
	vector<double> xAxis, yAxis;
	for (int i = -10; i < 100; i++) {
		xAxis.push_back(i * 50000);
		yAxis.push_back(i * 50000);
	}
	interpolator->changeProjection(MIUP_BILINEAR, "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +elips=sphere +a="+type2string(EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m");
	//interpolator->getCDM().toXMLStream(cerr);
	BOOST_CHECK(true);
	
	NetCDF_CDMWriter(interpolator, "testInterpolator.nc");
	BOOST_CHECK(true);

	string grdFile("/disk1/opdata/hirlam20/grdn06.dat");
	ifstream inputFile(grdFile.c_str());
	if (inputFile.is_open()) {
		inputFile.close();
		feltReader = boost::shared_ptr<CDMReader>(new FeltCDMReader(grdFile, "../share/etc/felt2nc_variables_hirlam20.xml"));
		interpolator = boost::shared_ptr<CDMInterpolator>(new CDMInterpolator(feltReader));
		xAxis = vector<double>();
		yAxis = vector<double>();
		for (int i = -114; i < 114; i++) {
			xAxis.push_back(i * 50000);
		}
		for (int i = -147; i < 48; i++) {
			yAxis.push_back(i * 50000);
		}
		interpolator->changeProjection(MIUP_BILINEAR, "+proj=stere +lat_0=90 +lon_0=0 +lat_ts=60 +elips=sphere +a="+type2string(EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m");
		BOOST_CHECK(true);
		NetCDF_CDMWriter(interpolator, "testInterpolator2.nc");
		BOOST_CHECK(true);
	}
}

test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );
   	test->add( BOOST_TEST_CASE( &test_interpolator ) );
    return test;
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
