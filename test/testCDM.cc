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

#include "config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;


#include "CDM.h"

using namespace std;
using namespace MetNoFimex;

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
	BOOST_CHECK(cdm.hasVariable(varName));
	
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
			
	vector<std::string> vars = cdm.findVariables("attr", "value");
	BOOST_CHECK(find(vars.begin(), vars.end(), varName) != vars.end());
	BOOST_CHECK(find(vars.begin(), vars.end(), varName2) != vars.end());
	vars = cdm.findVariables("attr2", "valueX");
	BOOST_CHECK(find(vars.begin(), vars.end(), varName) == vars.end());
	BOOST_CHECK(find(vars.begin(), vars.end(), varName2) != vars.end());
	
	try {
		cdm.addAttribute(varName, CDMAttribute("attr", "value"));
		BOOST_CHECK(false); // should throw an error
	} catch (CDMException& ex) {
		BOOST_CHECK(true);
	}
	cdm.addOrReplaceAttribute(varName, CDMAttribute("attr", "valueNew"));
	BOOST_CHECK(cdm.findVariables("attr", "valueNew").size() > 0);
	cdm.removeAttribute("bla", "blub");
	BOOST_CHECK(true); // no error
	cdm.removeAttribute(varName, "attr");
	BOOST_CHECK(cdm.findVariables("attr", "valueNew").size() == 0);
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
#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
