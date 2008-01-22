#include <iostream>
#include <boost/shared_ptr.hpp>
#include "FeltCDMReader.h"
#include "NetCDF_CDMWriter.h"

#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

using namespace std;
using namespace MetNoFelt;
using namespace MetNoUtplukk;


void
test_feltNetcdfWrite(void) {
	boost::shared_ptr<FeltCDMReader> feltReader(new FeltCDMReader("flth00.dat", "../etc/felt2nc_variables.xml"));
	NetCDF_CDMWriter(feltReader, "test.nc");
}


test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );

    test->add( BOOST_TEST_CASE( &test_feltNetcdfWrite ) );
    return test;
}
