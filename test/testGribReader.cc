/*
 * Fimex, testGribReader.cc
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Oct 7, 2009
 *      Author: Heiko Klein
 */

#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#include <iostream>
#include <fstream>
#include <boost/shared_ptr.hpp>
#include <vector>

#include "fimex/GribCDMReader.h"
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/Null_CDMWriter.h"
#include "fimex/Logger.h"
#include "fimex/Data.h"

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE(test_read_grb1) {
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/test.grb1");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    vector<string> gribFiles;
    gribFiles.push_back(fileName);
    defaultLogLevel(Logger::INFO);
    boost::shared_ptr<CDMReader> grbReader(new GribCDMReader(gribFiles, XMLInputFile(topSrcDir+"/test/cdmGribReaderConfig_newEarth.xml")));
    //grbReader->getCDM().toXMLStream(cout);
    BOOST_CHECK(grbReader->getCDM().hasVariable("x_wind_10m"));
    BOOST_CHECK(true); // made it so far
    DataPtr data = grbReader->getDataSlice("x_wind_10m", 0);
    boost::shared_array<float> dataFlt = data->asFloat();
    for (int i = 85*229+50; i < 85*229+150; i++) {
        // check data-range
        BOOST_CHECK(fabs(dataFlt[i]) < 15);
        // check data-precision (0.1 according to setup-file)
        BOOST_CHECK(1e-5 > fabs(10*dataFlt[i] - MetNoFimex::round(10*dataFlt[i])));
    }

    // check grib has new earth-radius
    CDMAttribute attr;
    BOOST_CHECK(grbReader->getCDM().getAttribute("projection_polar_stereographic", "earth_radius", attr));
    BOOST_CHECK(fabs(attr.getData()->asFloat()[0] - 6372000) < 1);
    // the following test has a tendency to fail when not all tests are run in correct order
    // a remaining grbml-file uses the wrong earth radius
    BOOST_CHECK(fabs(grbReader->getData("x")->asFloat()[0] - -5719440) < 1);


    // slicebuilder
    SliceBuilder sbX(grbReader->getCDM(), "x");
    sbX.setStartAndSize("x", 4, 10);
    BOOST_CHECK(grbReader->getDataSlice("x", sbX)->size() == 10);

    SliceBuilder sb(grbReader->getCDM(), "x_wind_10m");
    sb.setStartAndSize("time", 0, 1);
    sb.setStartAndSize("x", 4, 10);
    sb.setStartAndSize("y", 2, 2);
    vector<size_t> dimStart = sb.getDimensionStartPositions();
    DataPtr dataS = grbReader->getDataSlice("x_wind_10m", sb);
    BOOST_CHECK(20 == dataS->size());

    NetCDF_CDMWriter(grbReader, "testGribRead.nc");
    BOOST_CHECK(true); // and it is even writeable

}

BOOST_AUTO_TEST_CASE(test_read_grb2) {
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/test.grb2");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    vector<string> gribFiles;
    gribFiles.push_back(fileName);
    defaultLogLevel(Logger::INFO);
    boost::shared_ptr<CDMReader> grbReader(new GribCDMReader(gribFiles, XMLInputFile(topSrcDir+"/share/etc/cdmGribReaderConfig.xml")));
    //grbReader->getCDM().toXMLStream(cout);
    BOOST_CHECK(true); // made it so far
    Null_CDMWriter(grbReader, "");
    BOOST_CHECK(true); // and it is even writeable

}


#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
