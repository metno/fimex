/*
 * Fimex, testMerger.cc
 *
 * (C) Copyright 2012, met.no
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
 *
 *  Created on: Aug 28, 2012
 *      Author: Alexander Bürger
 */

#include "testinghelpers.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#include "fimex/CDMconstants.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMMerger.h"
#include "fimex/Data.h"

#include <boost/make_shared.hpp>
#include <numeric>

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_merger )
{
    const string fileNameInner = pathTest("test_merge_inner.nc"), fileNameOuter = pathTest("test_merge_outer.nc");

    CDMReader_p readerI = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileNameInner),
        readerO = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileNameOuter);

    boost::shared_ptr<CDMMerger> merger = boost::make_shared<CDMMerger>(readerI, readerO);
    merger->setTargetGridFromInner();

    DataPtr sliceM = merger->getDataSlice("ga_2t_1", 0);
    BOOST_CHECK( sliceM.get() != 0 );

    const int NLON = 61, NLAT = 113;
    BOOST_CHECK( sliceM->size() == NLON*NLAT );

    // test values: middle, transition, outer
    const int iLon[] = { 28, 24,  8, -1 };
    const int iLat[] = { 56, 56, 56, -1 };
    const double expected[] = { 288.104, 288.467, 289.937, -1 };

    boost::shared_array<double> valuesM = sliceM->asDouble();
    for(int i=0; iLon[i] >= 0; ++i) {
        const int offset = iLon[i] + iLat[i]*NLON;
#if 0
        cout << "iLon=" << iLon[i] << " iLat=" << iLat[i]
             << " expected=" << expected[i]
             << " actual=" << valuesM[offset] << endl;
#endif
        BOOST_CHECK( fabs(valuesM[offset] - expected[i]) < 0.001 );
    }
}

BOOST_AUTO_TEST_CASE( test_merge_target )
{
    const string fileNameB = pathTest("merge_target_base.nc"), fileNameT = pathTest("merge_target_top.nc");

    CDMReader_p readerB = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileNameB),
        readerT = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileNameT);

    boost::shared_ptr<CDMMerger> merger = boost::make_shared<CDMMerger>(readerB, readerT);
    merger->setTargetGrid("+proj=stere +lat_0=90 +lon_0=70 +lat_ts=60 +units=m +a=6.371e+06 +e=0 +no_defs",
            "-1192800,-1192000,...,-1112800", "-1304000,-1303200,...,-1224000", "m", "m", "double", "double");

    DataPtr sliceM = merger->getDataSlice("air_temperature_2m", 0);
    BOOST_CHECK( sliceM.get() != 0 );

    const int NX = 101, NY = 101;
    BOOST_CHECK( sliceM->size() == NX*NY );

    // test values: middle, transition, outer
    const int ix[] = { 19, 22, -1 };
    const int iy[] = { 65, 21, -1 };
    const double expected[] = { 275.62, 276.30, -1 };

    boost::shared_array<double> valuesM = sliceM->asDouble();
    for(int i=0; ix[i] >= 0; ++i) {
        const int offset = ix[i] + iy[i]*NX;
#if 0
        cout << "ix=" << ix[i] << " iy=" << iy[i]
             << " expected=" << expected[i]
             << " actual=" << valuesM[offset] << endl;
#endif
        BOOST_CHECK( fabs(valuesM[offset] - expected[i]) < 0.01 );
    }
}

#endif // HAVE_BOOST_UNIT_TEST_FRAMEWORK
