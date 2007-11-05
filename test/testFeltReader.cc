/* should be moved to cppunit or similar */
#include <iostream>
#include <boost/array.hpp>
#include <cassert>
#include "felt_reader/FeltParameters.h"
#include "felt_reader/Felt_File.h"

#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

using namespace std;
using namespace MetNoFelt;

void
test_feltparameter(void) {
	FeltParameters fp = FeltParameters();
	const boost::array<short, 16>  tksoil = fp.getParameters(string("tksoil"));
	//cout << "tksoil:" << tksoil[10] << ":" << tksoil[11] << ":" << tksoil[12] << endl;
	BOOST_CHECK(tksoil[10] == 2);
	BOOST_CHECK(tksoil[11] == 29);
	BOOST_CHECK(tksoil[12] == 1000);
	//cout << "reverse lookup: " << fp.getParameterName(tksoil) << endl;
	BOOST_CHECK(fp.getParameterName(tksoil) == "tksoil");
	const boost::array<short, 16>  lameps_prob_t2m = fp.getParameters(string("lameps_prob_t2m>+30"));
	//cout << "lameps_prob_t2m>+30:" << lameps_prob_t2m[10] << ":" << lameps_prob_t2m[11] << ":" << lameps_prob_t2m[12] << endl;
	BOOST_CHECK(lameps_prob_t2m[10] == 2);
	BOOST_CHECK(lameps_prob_t2m[11] == 115);
	BOOST_CHECK(lameps_prob_t2m[12] == 1000);
	//cout << "reverse lookup: " << fp.getParameterName(lameps_prob_t2m) << endl;
	BOOST_CHECK(fp.getParameterName(lameps_prob_t2m) == "lameps_prob_t2m>+30");
	//boost::array<short, 16> special = { {88, 1905, 2007, 516, 0, 20684, 1, 12137, 3, 0, 2, 58, 1000, 0, 1, 101} };
	//cout << "special array is: " << fp.getParameterName(special) << endl;
}

void
test_feltfile() {
	Felt_File ff("flth00.dat");
	vector<Felt_Array> vec = ff.listFeltArrays();
	for (vector<Felt_Array>::iterator it = vec.begin(); it != vec.end(); ++it) {
		//cout << it->getName() << endl;
	}
	Felt_Array& fa = ff.getFeltArray("u10m");
	vector<short> levels = fa.getLevels();
	//cout << fa.getName() << ": "<<levels.size() << ": " << fa.getTimes().size() << " size: " << fa.getFieldSize(fa.getTimes().at(0), levels.at(0)) << endl;
	BOOST_CHECK(levels.size() == 1);
	BOOST_CHECK( fa.getName() == "u10m" );
	BOOST_CHECK( fa.getTimes().size() == 61);
	BOOST_CHECK( fa.getFieldSize(fa.getTimes().at(50), levels.at(0)) == 44904 );
	
	
	try {
		ff.getFeltArray("this parameter is intentionally unknown");
		BOOST_CHECK(false); // should never reach this line
	} catch (Felt_File_Error& ffe) {
		//cout << ffe.toString() << endl;
		BOOST_CHECK(true);
	}
	//FeltParameters xx("/home/heikok/bla/test");
	
	
}

test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );

    test->add( BOOST_TEST_CASE( &test_feltparameter ) );
	test->add( BOOST_TEST_CASE( &test_feltfile ) );
    return test;
}
