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
	vector<std::string> noDim;
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
	
	CDMVariable& varRef = cdm.getVariable(varName);
	BOOST_CHECK(varRef.getName() == varName);
	try {
		cdm.getVariable("dummy");
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

void test_attributes(void) {
	CDM cdm;
	string varName("test");
	vector<std::string> noDim;
	CDMVariable testVar(varName, CDM_NAT, noDim);
	cdm.addVariable(testVar);
	string varName2("test2");
	CDMVariable testVar2(varName2, CDM_NAT, noDim);
	cdm.addVariable(testVar2);
	
	cdm.addAttribute(varName, CDMAttribute("attr", "value"));
	cdm.addAttribute(varName, CDMAttribute("attr2", "value"));
	cdm.addAttribute(varName2, CDMAttribute("attr", "value"));
	cdm.addAttribute(varName2, CDMAttribute("attr2", "valueX"));
			
	vector<std::string> vars = cdm.findVariable("attr", "value");
	BOOST_CHECK(find(vars.begin(), vars.end(), varName) != vars.end());
	BOOST_CHECK(find(vars.begin(), vars.end(), varName2) != vars.end());
	vars = cdm.findVariable("attr2", "valueX");
	BOOST_CHECK(find(vars.begin(), vars.end(), varName) == vars.end());
	BOOST_CHECK(find(vars.begin(), vars.end(), varName2) != vars.end());
}

void test_dimension(void) {
	
}

test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );
    
	test->add( BOOST_TEST_CASE( &test_cdm ) );
	test->add( BOOST_TEST_CASE( &test_variable ) );
	test->add( BOOST_TEST_CASE( &test_attributes ) );
	test->add( BOOST_TEST_CASE( &test_dimension ) );
    return test;
}
