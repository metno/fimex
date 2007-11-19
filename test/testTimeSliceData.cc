#include <iostream>
#include "Data.h"
#include "Time.h"
#include "TimeSliceData.h"

#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

using namespace MetNoUtplukk;

void test_timeslicedata_constructor() {
	boost::shared_array<int> ary(new int[100]);
	boost::shared_ptr<Data> data(new DataImpl<int>(ary, 100));
	BOOST_CHECK(data->size() == 100);
	TimeSliceData tsd(0, Time("seconds since 1970-01-01"), data);
	BOOST_CHECK(tsd.getData()->size() == 100);
}

void test_timeslicedata_convert() {
	boost::shared_array<double> ary(new double[3]);
	ary[0] = .0;
	ary[1] = .5;
	ary[2] = 1.;
	boost::shared_ptr<Data> data(new DataImpl<double>(ary, 3));
	boost::shared_array<int> intData = data->asInt();
	BOOST_CHECK(intData[1] == static_cast<int>(ary[1]));
	boost::shared_array<float> floatData = data->asFloat();
	BOOST_CHECK(floatData[1] == static_cast<float>(ary[1]));
}

test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );

    test->add( BOOST_TEST_CASE( &test_timeslicedata_constructor ) );
	test->add( BOOST_TEST_CASE( &test_timeslicedata_convert ) );
    return test;
}
