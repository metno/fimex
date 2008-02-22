#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>

#include "FeltCDMReader.h"
#include "CDMInterpolator.h"
#include "interpolation.h"

using namespace std;
using namespace MetNoUtplukk;

void
test_interpolator() {
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader("flth00.dat", "../etc/felt2nc_variables.xml"));
	boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(feltReader));
	vector<double> xAxis, yAxis;
	for (int i = -10; i < 11; i++) {
		xAxis.push_back(i * 50000);
		yAxis.push_back(i * 50000);
	}
	interpolator->changeProjection(MIUP_BILINEAR, "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +elips=sphere +a=3710000 +e=0", xAxis, yAxis, "m", "m");
	interpolator->getCDM().toXMLStream(cerr);
	BOOST_CHECK(true);
}

test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );
   	test->add( BOOST_TEST_CASE( &test_interpolator ) );
    return test;
}
