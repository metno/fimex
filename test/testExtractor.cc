#include "config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>
#include "FeltCDMReader.h"
#include "NetCDF_CDMWriter.h"
#include "CDMExtractor.h"

using namespace std;
using namespace MetNoFimex;

void
test_extract() {
	string topSrcDir(TOP_SRCDIR);
	string fileName(topSrcDir+"/test/flth00.dat");
	if (!ifstream(fileName.c_str())) {
		// no testfile, skip test
		return;
	}
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader(fileName, topSrcDir + "/share/etc/felt2nc_variables.xml"));
	boost::shared_ptr<CDMExtractor> extract(new CDMExtractor(feltReader));
	extract->removeVariable("relative_humidity");
	try {
		extract->getCDM().getVariable("relative_humidity");
		BOOST_CHECK(false);
	} catch (...) {
		BOOST_CHECK(true);
	}

	extract->reduceDimension("y", 10, 50);
	extract->reduceDimension("x", 80, 50); // spain
	extract->reduceDimension("time", 10, 12);
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

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
