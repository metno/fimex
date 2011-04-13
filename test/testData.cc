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

#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;


#include "fimex/Data.h"
#include "fimex/DataImpl.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_slicing )
{
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

	// test_slicing of a scalar (size 0 or size 1)
	DataImpl<double> scalar(1);
	scalar.setValue(0, 3.);
    boost::shared_ptr<Data> scalarSlice = scalar.slice(vector<size_t>(0), vector<size_t>(0), vector<size_t>(0));
    BOOST_CHECK(scalar.size() == scalarSlice->size());
    BOOST_CHECK(scalar.asConstDouble()[0] == scalarSlice->asConstDouble()[0]);
}

BOOST_AUTO_TEST_CASE( test_slicing2D )
{
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

BOOST_AUTO_TEST_CASE( test_slicing3D )
{
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

BOOST_AUTO_TEST_CASE( test_slice_asym )
{
	size_t sigma = 1;
	size_t y = 3;
	size_t x = 5;
	DataImpl<int> data(x*y*sigma);
	for (size_t i = 0; i < sigma; i++)
		for (size_t j = 0; j < y; j++)
			for (size_t k = 0; k < x; k++) {
				int pos = i*(x*y) + j*x + k;
				// cerr << i << ":" << j << ":" << k << " = " << pos << ":" << j << endl;
				data.setValue(pos, j);
			}

	std::vector<size_t> orgDimSize(3, 0);
	orgDimSize[0] = x;
	orgDimSize[1] = y;
	orgDimSize[2] = sigma;
	std::vector<size_t> newDimStart(3, 0);
	newDimStart[1] = 1;
	std::vector<size_t> newDimSize(3, 0);
	newDimSize[0] = x;
	newDimSize[1] = 2;
	newDimSize[2] = sigma;
	boost::shared_ptr<Data> slice = data.slice(orgDimSize, newDimStart, newDimSize);
	BOOST_CHECK(slice->size() == newDimSize[0]*newDimSize[1]*newDimSize[2]);
	boost::shared_array<int> intSlice = slice->asInt();
	for (size_t i = 0; i < newDimSize[2]; i++)
		for (size_t j = 0; j < newDimSize[1]; j++)
			for (size_t k = 0; k < newDimSize[0]; k++) {
				int pos = k + j * (newDimSize[0]) + i * (newDimSize[1]*newDimSize[0]);
				//std::cerr << i << ":" << j << ":" << k << " = " << pos << ":" << intSlice[pos] << std::endl;
				BOOST_CHECK(intSlice[pos] == j + newDimStart[1]);
			}
}

BOOST_AUTO_TEST_CASE( test_slice_segfault )
{
	// this is a hirlam20 case which caused a segfault, keeping it for interest
	size_t sigma = 1;
	size_t y = 196;
	size_t x = 229;
	DataImpl<int> data(sigma*x*y);
	for (size_t i = 0; i < sigma; i++)
		for (size_t j = 0; j < x; j++)
			for (size_t k = 0; k < y; k++)
				data.setValue(k+(i*10+j)*10, k+(i*10+j)*10);

	std::vector<size_t> orgDimSize(3, 0);
	orgDimSize[0] = sigma;
	orgDimSize[1] = y;
	orgDimSize[2] = x;
	std::vector<size_t> newDimStart(3, 0);
	newDimStart[1] = 50;
	std::vector<size_t> newDimSize(3, 0);
	newDimSize[0] = sigma;
	newDimSize[1] = 100;
	newDimSize[2] = x;
	boost::shared_ptr<Data> slice = data.slice(orgDimSize, newDimStart, newDimSize);
	BOOST_CHECK(slice->size() == newDimSize[0]*newDimSize[1]*newDimSize[2]);

}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
