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

#include "fimex_config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>
#include "FeltCDMReader2.h"
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/CDMExtractor.h"
#include "fimex/Logger.h"
#include "fimex/Data.h"
#include "fimex/interpolation.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_extract )
{
    //defaultLogLevel(Logger::DEBUG);
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/flth00.dat");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    CDMReader_p feltReader(new FeltCDMReader2(fileName, topSrcDir + "/share/etc/felt2nc_variables.xml"));
    boost::shared_ptr<CDMExtractor> extract(new CDMExtractor(feltReader));
    extract->removeVariable("relative_humidity");
    try {
        extract->getCDM().getVariable("relative_humidity");
        BOOST_CHECK(false);
    } catch (...) {
        BOOST_CHECK(true);
    }

    extract->reduceDimension("y", 10, 50);
    extract->reduceDimension("x", 80, 50); // spain
    FimexTime startTime(2007,5,16, 9);
    FimexTime endTime(2007,5,16, 20);
    extract->reduceTime(startTime, endTime); // 12 hours 9..20
    BOOST_CHECK(extract->getData("y")->size() == 50);
    BOOST_CHECK(extract->getData("x")->size() == 50);
    BOOST_CHECK(extract->getData("time")->size() == 12);
    BOOST_CHECK(extract->getData("altitude")->size() == 50*50);
    BOOST_CHECK(extract->getData("precipitation_amount")->size() == 50*50*12);
    BOOST_CHECK(extract->getData("air_temperature")->size() == 50*50*12);
    boost::shared_array<float> precData1 = extract->getData("air_temperature")->asFloat();
    NetCDF_CDMWriter(extract, "test_extract_1.nc");
    BOOST_CHECK(true);

    // test chunked reading
    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceTime(startTime, endTime); // 12 hours 9..20
    std::set<size_t> slices;
    slices.clear(); slices.insert(10); slices.insert(11); slices.insert(13); slices.insert(16);
    extract->reduceDimension("y", slices);
    slices.clear(); slices.insert(80); slices.insert(83);
    extract->reduceDimension("x", slices);
    BOOST_CHECK(extract->getCDM().getDimension("y").getLength() == 4);
    BOOST_CHECK(extract->getData("y")->size() == 4);
    BOOST_CHECK(extract->getData("x")->size() == 2);
    BOOST_CHECK(extract->getData("time")->size() == 12);
    BOOST_CHECK(extract->getData("precipitation_amount")->size() == 4*2*12);
    boost::shared_array<float> precData2 = extract->getData("air_temperature")->asFloat();
    //cerr << join (&precData2[0], &precData2[0]+(4*2*12));
    for (size_t t = 0; t < 12; t++) {
        BOOST_CHECK(precData1[mifi_3d_array_position(0,0,t,50,50,12)] == precData2[mifi_3d_array_position(0,0,t,2,4,12)]);
//        cerr << precData1[mifi_3d_array_position(3,0,t,50,50,12)] << " " << precData2[mifi_3d_array_position(1,0,t,2,4,12)] << endl;
//        cerr << precData1[mifi_3d_array_position(0,1,t,50,50,12)] << " " <<  precData2[mifi_3d_array_position(0,1,t,2,4,12)] << endl;;
        BOOST_CHECK(precData1[mifi_3d_array_position(3,0,t,50,50,12)] == precData2[mifi_3d_array_position(1,0,t,2,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(0,1,t,50,50,12)] == precData2[mifi_3d_array_position(0,1,t,2,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(3,1,t,50,50,12)] == precData2[mifi_3d_array_position(1,1,t,2,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(0,3,t,50,50,12)] == precData2[mifi_3d_array_position(0,2,t,2,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(3,3,t,50,50,12)] == precData2[mifi_3d_array_position(1,2,t,2,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(0,6,t,50,50,12)] == precData2[mifi_3d_array_position(0,3,t,2,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(3,6,t,50,50,12)] == precData2[mifi_3d_array_position(1,3,t,2,4,12)]);
    }
    NetCDF_CDMWriter(extract, "test_extract_2.nc");
    BOOST_CHECK(true);

    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceTime(startTime, endTime); // 12 hours 9..20
    slices.clear(); slices.insert(10); slices.insert(11); slices.insert(13); slices.insert(16);
    extract->reduceDimension("y", slices);
    extract->reduceDimension("x", 80, 50); // spain
    BOOST_CHECK(extract->getCDM().getDimension("y").getLength() == 4);
    BOOST_CHECK(extract->getData("y")->size() == 4);
    BOOST_CHECK(extract->getData("x")->size() == 50);
    BOOST_CHECK(extract->getData("time")->size() == 12);
    //cerr << extract->getScaledDataInUnit("time", "hours since 2007-05-16 09:00:00 +0000")->asInt()[0] << endl;
    BOOST_CHECK(extract->getScaledDataInUnit("time", "hours since 2007-05-16 09:00:00 +0000")->asInt()[0] == 0);
    BOOST_CHECK(extract->getScaledDataInUnit("time", "hours since 2007-05-16 09:00:00 +0000")->asInt()[4] == 4);
    BOOST_CHECK(extract->getScaledDataInUnit("time", "hours since 2007-05-16 09:00:00 +0000")->asInt()[11] == 11);
    BOOST_CHECK(extract->getData("precipitation_amount")->size() == 4*50*12);
    precData2 = extract->getData("air_temperature")->asFloat();
    //cerr << join (&precData2[0], &precData2[0]+(4*2*12));
    for (size_t t = 0; t < 12; t++) {
        BOOST_CHECK(precData1[mifi_3d_array_position(0,0,t,50,50,12)] == precData2[mifi_3d_array_position(0,0,t,50,4,12)]);
//        cerr << precData1[mifi_3d_array_position(3,0,t,50,50,12)] << " " << precData2[mifi_3d_array_position(3,0,t,50,4,12)] << endl;
//        cerr << precData1[mifi_3d_array_position(0,1,t,50,50,12)] << " " <<  precData2[mifi_3d_array_position(0,1,t,50,4,12)] << endl;;
        BOOST_CHECK(precData1[mifi_3d_array_position(3,0,t,50,50,12)] == precData2[mifi_3d_array_position(3,0,t,50,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(0,1,t,50,50,12)] == precData2[mifi_3d_array_position(0,1,t,50,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(3,1,t,50,50,12)] == precData2[mifi_3d_array_position(3,1,t,50,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(0,3,t,50,50,12)] == precData2[mifi_3d_array_position(0,2,t,50,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(3,3,t,50,50,12)] == precData2[mifi_3d_array_position(3,2,t,50,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(0,6,t,50,50,12)] == precData2[mifi_3d_array_position(0,3,t,50,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(3,6,t,50,50,12)] == precData2[mifi_3d_array_position(3,3,t,50,4,12)]);
    }
    BOOST_CHECK(true);

    // slicebuilder along x
    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceTime(startTime, endTime); // 12 hours 9..20
    slices.clear(); slices.insert(10); slices.insert(11); slices.insert(13); slices.insert(16);
    extract->reduceDimension("y", slices);
    BOOST_CHECK(extract->getData("y")->size() == 4);
    BOOST_CHECK(extract->getData("time")->size() == 12);
    //cerr << extract->getScaledDataInUnit("time", "hours since 2007-05-16 09:00:00 +0000")->asInt()[0] << endl;
    SliceBuilder sb(extract->getCDM(), "air_temperature");
    sb.setStartAndSize("x", 80, 50);

    DataPtr airTemp = extract->getDataSlice("air_temperature", sb);
    BOOST_CHECK(airTemp->size() == 4*50*12);
    precData2 = airTemp->asFloat();

    //cerr << join (&precData2[0], &precData2[0]+(4*2*12));
    for (size_t t = 0; t < 12; t++) {
        BOOST_CHECK(precData1[mifi_3d_array_position(0,0,t,50,50,12)] == precData2[mifi_3d_array_position(0,0,t,50,4,12)]);
//        cerr << precData1[mifi_3d_array_position(3,0,t,50,50,12)] << " " << precData2[mifi_3d_array_position(3,0,t,50,4,12)] << endl;
//        cerr << precData1[mifi_3d_array_position(0,1,t,50,50,12)] << " " <<  precData2[mifi_3d_array_position(0,1,t,50,4,12)] << endl;;
        BOOST_CHECK(precData1[mifi_3d_array_position(3,0,t,50,50,12)] == precData2[mifi_3d_array_position(3,0,t,50,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(0,1,t,50,50,12)] == precData2[mifi_3d_array_position(0,1,t,50,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(3,1,t,50,50,12)] == precData2[mifi_3d_array_position(3,1,t,50,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(0,3,t,50,50,12)] == precData2[mifi_3d_array_position(0,2,t,50,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(3,3,t,50,50,12)] == precData2[mifi_3d_array_position(3,2,t,50,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(0,6,t,50,50,12)] == precData2[mifi_3d_array_position(0,3,t,50,4,12)]);
        BOOST_CHECK(precData1[mifi_3d_array_position(3,6,t,50,50,12)] == precData2[mifi_3d_array_position(3,3,t,50,4,12)]);
    }
    BOOST_CHECK(true);



    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceTime(FimexTime(FimexTime::min_date_time), FimexTime(FimexTime::max_date_time));
    BOOST_CHECK(extract->getData("time")->size() == 61);

    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    // reduce sigma from 4 to 2
    extract->reduceVerticalAxis("", .5, .85);
    BOOST_CHECK(extract->getData("sigma")->size() == 2);

    // reduce time to 0 and sigma to 0
    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceVerticalAxis("", -0.1, -0.05);
    extract->reduceTime(FimexTime(2006,1,1), FimexTime(2006,1,2)); // time out of range
    BOOST_CHECK(extract->getData("time")->size() == 0);
    NetCDF_CDMWriter(extract, "test_extract_0time.nc");
    BOOST_CHECK(true);

    // test selectVariable
    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    std::set<std::string> variables;
    variables.insert("time");
    variables.insert("altitude");
    variables.insert("relative_humidity");
    extract->selectVariables(variables, true);
    BOOST_CHECK(extract->getCDM().hasVariable("time"));
    BOOST_CHECK(extract->getCDM().hasVariable("altitude"));
    BOOST_CHECK(extract->getCDM().hasVariable("relative_humidity"));
    BOOST_CHECK(extract->getCDM().hasVariable("x")); // auxiliary
    BOOST_CHECK(extract->getCDM().hasVariable("y")); // auxiliary
    BOOST_CHECK(false == extract->getCDM().hasVariable("precipitation_amount"));

    // test reduceLatLonBoundingBox
    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceDimension("time", 0, 1);
    extract->reduceLatLonBoundingBox(55.,65., 5, 15);
    //cerr << "sizes: " << extract->getData("x")->size() << " " << extract->getData("y")->size() << endl;
    BOOST_CHECK(extract->getData("x")->size() == 15);
    BOOST_CHECK(extract->getData("y")->size() == 24);

    // test selection of non existing selectVariable
    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    variables.insert("not_there");
    extract->selectVariables(variables);
    BOOST_CHECK(extract->getCDM().hasVariable("relative_humidity"));
    BOOST_CHECK(false == extract->getCDM().hasVariable("precipitation_amount"));



}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
