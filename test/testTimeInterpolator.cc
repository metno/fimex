/*
 * Fimex, timeInterpolator.cc
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
 *
 *  Created on: Jan 6, 2009
 *      Author: Heiko Klein
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
#include "fimex/Data.h"
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/NetCDF_CDMReader.h"
#include "fimex/CDMTimeInterpolator.h"
#include "fimex/Logger.h"

#include <boost/filesystem/operations.hpp>

using namespace std;
using namespace MetNoFimex;
namespace fs = boost::filesystem;

BOOST_AUTO_TEST_CASE( test_timeInterpolator )
{
    //defaultLogLevel(Logger::DEBUG);
    const string outputName = "test_timeInterpolator.nc";
    fs::remove(outputName);

    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/flth00.dat");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    CDMReader_p feltReader(new FeltCDMReader2(fileName, topSrcDir + "/share/etc/felt2nc_variables.xml"));
    boost::shared_ptr<CDMTimeInterpolator> timeInterpol(new CDMTimeInterpolator(feltReader));
    timeInterpol->changeTimeAxis("2007-05-16 10:00:00,2007-05-16 13:00:00,...,2007-05-16 22:00:00;unit=hours since 2007-05-16 00:00:00");
    DataPtr times = timeInterpol->getCDM().getVariable("time").getData();
    BOOST_CHECK_EQUAL(times->size(), 5);
    boost::shared_array<float> timeAry = times->asFloat();
    BOOST_CHECK_EQUAL(timeAry[0], 10);
    BOOST_CHECK_EQUAL(timeAry[4], 10+12);
    string airTemp = "air_temperature";
    BOOST_ASSERT(feltReader->getCDM().getVariable(airTemp).getName() == airTemp);
    BOOST_CHECK(timeInterpol->getCDM().getVariable(airTemp).getName() == airTemp);
    NetCDF_CDMWriter(timeInterpol, outputName);
    BOOST_CHECK(true);

    // check that the correct data is written
    CDMReader_p ncReader(new NetCDF_CDMReader(outputName));
    DataPtr ncTimes = ncReader->getData("time");
    BOOST_CHECK_EQUAL(ncTimes->size(), 5);
    boost::shared_array<float> ncTimeAry = ncTimes->asFloat();
    BOOST_CHECK_EQUAL(ncTimeAry[0], 10);
    BOOST_CHECK_EQUAL(ncTimeAry[4], 10+12);


}

BOOST_AUTO_TEST_CASE( test_timeInterpolatorRelative )
{
    const string outputName = "test_timeInterpolatorRelative.nc";
    fs::remove(outputName);

    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/flth00.dat");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    CDMReader_p feltReader(new FeltCDMReader2(fileName, topSrcDir + "/share/etc/felt2nc_variables.xml"));
    boost::shared_ptr<CDMTimeInterpolator> timeInterpol(new CDMTimeInterpolator(feltReader));
    timeInterpol->changeTimeAxis("0,3,...,x;relativeUnit=hours since 2001-01-01 10:00:00;unit=hours since 2007-05-16 00:00:00");
    DataPtr times = timeInterpol->getCDM().getVariable("time").getData();
    BOOST_CHECK_EQUAL(times->size(), 21);
    boost::shared_array<float> timeAry = times->asFloat();
    BOOST_CHECK_EQUAL(timeAry[0], -2);
    BOOST_CHECK_EQUAL(timeAry[4], 10);
    string airTemp = "air_temperature";
    BOOST_ASSERT(feltReader->getCDM().getVariable(airTemp).getName() == airTemp);
    BOOST_CHECK(timeInterpol->getCDM().getVariable(airTemp).getName() == airTemp);
    NetCDF_CDMWriter(timeInterpol, outputName);
    BOOST_CHECK(true);

    // check that the correct data is written
    CDMReader_p ncReader(new NetCDF_CDMReader(outputName));
    DataPtr ncTimes = ncReader->getData("time");
    BOOST_CHECK_EQUAL(ncTimes->size(), 21);
    boost::shared_array<float> ncTimeAry = ncTimes->asFloat();
    BOOST_CHECK_EQUAL(ncTimeAry[0], -2);
    BOOST_CHECK_EQUAL(ncTimeAry[4], 10);


}


#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
