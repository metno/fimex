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

#include "testinghelpers.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#include "fimex/Data.h"
#include "../src/DataImpl.h"
#include "fimex/IndexedData.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_data_retrieval )
{
    DataImpl<int> data(10);
    for (int i = 0; i < 10; i++) {
        data.setValue(i, i);
    }
    BOOST_CHECK(data.asBase()[0] == 0);
    BOOST_CHECK(data.asBase()[4] == 4);
    BOOST_CHECK(data.asInt()[0] == 0);
    BOOST_CHECK(data.asConstInt()[0] == 0);

    // changing data
    data.asInt()[0] = 10;
    BOOST_CHECK(data.asInt()[0] == 10);
    BOOST_CHECK(data.asConstInt()[0] == 10);

    // changing data with different type not reflected
    data.asDouble()[0] = 100;
    BOOST_CHECK(data.asInt()[0] == 10);
    BOOST_CHECK(data.asConstInt()[0] == 10);
}

BOOST_AUTO_TEST_CASE( test_indexed_data )
{
    DataPtr dPtr(new DataImpl<int>(9));
    for (int i = 0; i < 9; i++) {
        dPtr->setValue(i, i);
    }
    IndexedData iData(dPtr, vector<size_t>(2,3)); // 3x3 array
    BOOST_CHECK(iData.idx().getDims().size() == 2);
    for (size_t j = 0; j < 3; j++) {
        for (size_t i = 0; i < 3; i++) {
            BOOST_CHECK(iData.getLongLong(i) == i);
            BOOST_CHECK(iData.getLongLong(i,j) == (3*j)+i);
            BOOST_CHECK(iData.getLongLong(i,j,17,35) == (3*j)+i);
            BOOST_CHECK_CLOSE(iData.getDouble(i), i, 1e-5);
            BOOST_CHECK_CLOSE(iData.getDouble(i,j), (3*j)+i, 1e-5);
            BOOST_CHECK_CLOSE(iData.getDouble(i,j,17,35), (3*j)+i, 1e-5);
         }
    }
}


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
    DataPtr slice = data.slice(orgDimSize, newDimStart, newDimSize);

    BOOST_CHECK(slice->size() == newSize);
    BOOST_CHECK((slice->asInt())[0] == start);

    // test_slicing of a scalar (size 0 or size 1)
    DataImpl<double> scalar(1);
    scalar.setValue(0, 3.);
    DataPtr scalarSlice = scalar.slice(vector<size_t>(0), vector<size_t>(0), vector<size_t>(0));
    BOOST_CHECK(scalar.size() == scalarSlice->size());
    BOOST_CHECK(scalar.asDouble()[0] == scalarSlice->asDouble()[0]);
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
    DataPtr slice = data.slice(orgDimSize, newDimStart, newDimSize);

    BOOST_CHECK(slice->size() == newSize*newSize);
    BOOST_CHECK((slice->asInt())[0] == static_cast<int>(newDimStart[1]*10 + start)); // 44
    BOOST_CHECK((slice->asInt())[newSize] == static_cast<int>(newDimStart[1]*10 + (newSize-1)*10 + start)); // 44
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
    DataPtr slice = data.slice(orgDimSize, newDimStart, newDimSize);

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
    DataPtr slice = data.slice(orgDimSize, newDimStart, newDimSize);
    BOOST_CHECK(slice->size() == newDimSize[0]*newDimSize[1]*newDimSize[2]);
    boost::shared_array<int> intSlice = slice->asInt();
    for (size_t i = 0; i < newDimSize[2]; i++)
        for (size_t j = 0; j < newDimSize[1]; j++)
            for (size_t k = 0; k < newDimSize[0]; k++) {
                int pos = k + j * (newDimSize[0]) + i * (newDimSize[1]*newDimSize[0]);
                //std::cerr << i << ":" << j << ":" << k << " = " << pos << ":" << intSlice[pos] << std::endl;
                BOOST_CHECK(intSlice[pos] == static_cast<int>(j + newDimStart[1]));
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
    DataPtr slice = data.slice(orgDimSize, newDimStart, newDimSize);
    BOOST_CHECK(slice->size() == newDimSize[0]*newDimSize[1]*newDimSize[2]);

}

BOOST_AUTO_TEST_CASE( test_rounding )
{
    DataPtr dataDouble(new DataImpl<double>(40));
    for (int i = -20; i < 20; i++) {
        const int j = i+20;
        dataDouble->setValue(j, i*0.1);
    }
    DataPtr dataShort = dataDouble->convertDataType(1e30, 1, 0, CDM_SHORT, -32768, 1, 0);
    boost::shared_array<short> asS = dataShort->asShort();

    boost::shared_array<int> asI = dataDouble->asInt();
    for (int i = -20; i < 20; i++) {
        const int j = i+20;
        int expect;
        if (i <= -15)
            expect = -2;
        else if (i <= -5)
            expect = -1;
        else if (i < 5)
            expect = 0;
        else if (i < 15)
            expect = 1;
        else
            expect = 2;

        BOOST_CHECK_MESSAGE(asS[j] == expect, "short: i=" << i << " have == " << asS[j] << " expected " << expect);
        BOOST_CHECK_MESSAGE(asI[j] == expect, "int:   i=" << i << " have == " << asI[j] << " expected " << expect);
    }
}

#endif // HAVE_BOOST_UNIT_TEST_FRAMEWORK
