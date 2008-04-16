#include "../src/config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#include <iostream>
#include "Time.h"

#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

using namespace MetNoUtplukk;

void test_time_constructor()
{
	try {
		Time t1("seconds since 1970-01-01 00:00:00 +00:00", 0.f);
		Time t2(t1.getUnit(), 100.f);
	} catch (TimeException& ex) {
		std::cerr << ex.what() << std::endl;
		BOOST_CHECK(false);
	}
	try {
		Time t1("not a time", 0.f);
	} catch (TimeException& ex) {
		BOOST_CHECK(true);
		//std::cerr << ex.what() << std::endl;
	}
}

void test_time_convert()
{
	Time t1("seconds since 1970-01-01 00:00:00 +00:00", 0.f);
	Time t2("seconds since 1970-01-01 00:00:59 +00:00", 0.f);
	t1.convert(t2);
	BOOST_CHECK(((int)t1.getValue()) == -59);
}



test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );

    test->add( BOOST_TEST_CASE( &test_time_constructor ) );
	test->add( BOOST_TEST_CASE( &test_time_convert ) );
    return test;
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
