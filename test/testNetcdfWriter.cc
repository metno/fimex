#include "config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#include <iostream>
#include <fstream>
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
	string topSrcDir(TOP_SRCDIR);
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader(topSrcDir+"/test/flth00.dat", topSrcDir+"/share/etc/felt2nc_variables.xml"));
	NetCDF_CDMWriter(feltReader, "test.nc");
	string testFile2("/disk1/opdata/hirlam20/grdn06.dat");
	ifstream grdFile(testFile2.c_str());
	if (grdFile.is_open()) {
		grdFile.close();
		feltReader = boost::shared_ptr<CDMReader>(new FeltCDMReader(testFile2, topSrcDir+"/share/etc/felt2nc_variables_hirlam20.xml"));
		NetCDF_CDMWriter(feltReader, "test2.nc");
	}
}


test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );

    test->add( BOOST_TEST_CASE( &test_feltNetcdfWrite ) );
    return test;
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
