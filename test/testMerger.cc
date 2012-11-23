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

#include "../config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include "fimex/CDMconstants.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMMerger.h"
#include "fimex/Data.h"

#include <boost/make_shared.hpp>

#include <iostream>
#include <fstream>
#include <numeric>

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_merger )
{
    string topSrcDir(TOP_SRCDIR);
    string fileNameInner(topSrcDir+"/test/test_merge_inner.nc"), fileNameOuter(topSrcDir+"/test/test_merge_outer.nc");
    if( not ifstream(fileNameInner.c_str()) or not ifstream(fileNameOuter.c_str()) ) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> readerI = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileNameInner),
        readerO = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileNameOuter);

    boost::shared_ptr<CDMMerger> merger = boost::make_shared<CDMMerger>(readerI, readerO);
    merger->addMergedVariable("ga_2t_1");

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

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif

