#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;


#include "Data.h"

using namespace std;
using namespace MetNoUtplukk;

void test_slicing() {
	DataImpl<int> data(10);
	for (int i = 0; i < 10; i++) {
		data.setValue(i, i);
	}
	BOOST_CHECK(data.asBase()[0] == 0);
	int start = 4;
	BOOST_CHECK(data.asBase()[start] == start);
	
	std::vector<size_t> orgDimSize(1, 10);
	std::vector<size_t> newDimStart(1, start);
	size_t newSize = 2;
	std::vector<size_t> newDimSize(1, newSize);
	boost::shared_ptr<Data> slice = data.slice(orgDimSize, newDimStart, newDimSize);

	BOOST_CHECK(slice->size() == newSize);
	BOOST_CHECK((slice->asInt())[0] == start);
}

void test_slicing2D() {
	DataImpl<int> data(100);
	for (int i = 0; i < 10; i++) {
		for (int j = 0; j < 10; j++) {
			data.setValue(i*10+j, i*10+j);
		}
	}
	BOOST_CHECK(data.asBase()[0] == 0);
	int start = 4;
	BOOST_CHECK(data.asBase()[start] == start);
	
	std::vector<size_t> orgDimSize(2, 10);
	std::vector<size_t> newDimStart(2, start);
	size_t newSize = 2;
	std::vector<size_t> newDimSize(2, newSize);
	boost::shared_ptr<Data> slice = data.slice(orgDimSize, newDimStart, newDimSize);

	BOOST_CHECK(slice->size() == newSize*newSize);
	BOOST_CHECK((slice->asInt())[0] == (newDimStart[1]*10 + start)); // 44
	BOOST_CHECK((slice->asInt())[newSize] == (newDimStart[1]*10 + (newSize-1)*10 + start)); // 44
}

void test_slicing3D() {
	DataImpl<int> data(1000);
	for (int i = 0; i < 10; i++) {
		for (int j = 0; j < 10; j++) {
			for (int k = 0; k < 10; k++)
				data.setValue(k+(i*10+j)*10, k+(i*10+j)*10);
		}
	}
	BOOST_CHECK(data.asBase()[0] == 0);
	int start = 4;
	BOOST_CHECK(data.asBase()[start] == start);
	
	std::vector<size_t> orgDimSize(3, 10);
	std::vector<size_t> newDimStart(3, start);
	size_t newSize = 2;
	std::vector<size_t> newDimSize(3, newSize);
	boost::shared_ptr<Data> slice = data.slice(orgDimSize, newDimStart, newDimSize);

	BOOST_CHECK(slice->size() == newSize*newSize*newSize);
	BOOST_CHECK((slice->asInt())[0] == (start*100 + start*10 + start)); // 444
	BOOST_CHECK((slice->asInt())[newSize*newSize+1] == 545); // 545
}


test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );
   	test->add( BOOST_TEST_CASE( &test_slicing ) );
   	test->add( BOOST_TEST_CASE( &test_slicing2D ) );
   	test->add( BOOST_TEST_CASE( &test_slicing3D ) );
    return test;
}
