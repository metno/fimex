#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;


#include "FeltCDMReader.h"
#include "NetCDF_CDMWriter.h"
#include "CDMExtractor.h"

using namespace std;
using namespace MetNoUtplukk;

void
test_extract() {
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader("flth00.dat", "../etc/felt2nc_variables.xml"));
	boost::shared_ptr<CDMExtractor> extract(new CDMExtractor(feltReader));
	extract->removeVariable("relative_humidity");
	try {
		extract->getCDM().getVariable("relative_humidity");
		BOOST_CHECK(false);
	} catch (...) {
		BOOST_CHECK(true);
	}

	extract->reduceDimension("y", 50, 100);
	NetCDF_CDMWriter(extract, "test3.nc");
	BOOST_CHECK(true);
}

test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );
   	test->add( BOOST_TEST_CASE( &test_extract ) );
    return test;
}
