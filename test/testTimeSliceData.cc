#include <iostream>
#include "Time.h"
#include "TimeSliceData.h"

#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

using namespace MetNoUtplukk;

void test_timeslicedata_constructor() {
	std::auto_ptr<TimeSliceData> tsd(new TimeSliceDataImpl<int>(0, Time("seconds since 1970-01-01"), 100, boost::shared_array<int>(new int[100])));
	BOOST_CHECK(tsd->size() == 100);
}

void test_timeslicedata_convert() {
	boost::shared_array<double> data(new double[3]);
	data[0] = .0;
	data[1] = .5;
	data[2] = 1.;
	std::auto_ptr<TimeSliceData> tsd(new TimeSliceDataImpl<double>(0, Time("seconds since 1970-01-01"), 100, data));
	boost::shared_array<int> intData = tsd->asInt();
	BOOST_CHECK(intData[1] == static_cast<int>(data[1]));
	boost::shared_array<float> floatData = tsd->asFloat();
	BOOST_CHECK(floatData[1] == static_cast<float>(data[1]));
}

test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );

    test->add( BOOST_TEST_CASE( &test_timeslicedata_constructor ) );
	test->add( BOOST_TEST_CASE( &test_timeslicedata_convert ) );
    return test;
}
