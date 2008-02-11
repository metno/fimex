#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;


#include "CDM.h"

using namespace std;
using namespace MetNoUtplukk;

void test_cdm() {
	CDM cdm;
	BOOST_CHECK(true); // cdm initialized

}


void test_variable(void) {
	vector<CDMDimension> noDim;
	string varName("test");
	CDMVariable testVar(varName, CDM_NAT, noDim);
	CDM cdm;
	cdm.addVariable(testVar);
	BOOST_CHECK(cdm.getVariables().find(varName) != cdm.getVariables().end());
	
	try {
		CDMVariable failVar(varName, CDM_NAT, noDim);
		cdm.addVariable(failVar); // adding new variable with same name should fail
		BOOST_CHECK(false);
	} catch (CDMException& ex) {
		BOOST_CHECK(true);
	}	
	
	try {
		cdm.removeVariable("dummy"); // should fail, doesn't exists
		BOOST_CHECK(false);
	} catch (CDMException& ex) {
		BOOST_CHECK(true);
	}
	cdm.removeVariable(varName);
	BOOST_CHECK(true);
}

test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );
    
	test->add( BOOST_TEST_CASE( &test_cdm ) );
	test->add( BOOST_TEST_CASE( &test_variable ) );
    return test;
}
