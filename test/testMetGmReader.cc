/*
 * Fimex, testMetgmReader.cc
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Jun 13, 2011
 *      Author: Aleksandar Babic
 */

#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#include <iostream>
#include <fstream>
#include <boost/shared_ptr.hpp>
#include <vector>

#include "fimex/MetGmCDMReader.h"
#ifdef HAVE_NETCDF_H
#include "fimex/NetCDF_CDMWriter.h"
#endif
#include "fimex/Null_CDMWriter.h"
#include "fimex/Logger.h"
#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/CoordinateSystemSliceBuilder.h"
#include "fimex/Data.h"


#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE(test_read_sample_ed1) {
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/sample_ed1.gm");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }

    defaultLogLevel(Logger::INFO);
    boost::shared_ptr<CDMReader> metgmReaderEd1(new MetGmCDMReader(fileName, XMLInputFile(topSrcDir+"/share/etc/cdmMetGmReaderConfig.xml")));
    BOOST_CHECK(true); // made it so far
#ifdef HAVE_NETCDF_H
    NetCDF_CDMWriter(metgmReaderEd1, "testMetgmReadEd1.nc");
    BOOST_CHECK(true); // and it is even writeable
#endif
    Null_CDMWriter(metgmReaderEd1, "");
    BOOST_CHECK(true); // and it is even writeable

}

BOOST_AUTO_TEST_CASE(test_read_metgm2) {
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/sample_ed2.gm");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }

    defaultLogLevel(Logger::INFO);
    boost::shared_ptr<CDMReader> metgmReaderEd2(new MetGmCDMReader(fileName, XMLInputFile(topSrcDir+"/share/etc/cdmMetGmReaderConfig.xml")));
    BOOST_CHECK(true); // made it so far
#ifdef HAVE_NETCDF_H
    NetCDF_CDMWriter(metgmReaderEd2, "testMetgmReadEd2.nc");
    BOOST_CHECK(true); // and it is even writeable
#endif
    Null_CDMWriter(metgmReaderEd2, "");
    BOOST_CHECK(true); // and it is even writeable

}

BOOST_AUTO_TEST_CASE(test_slicebuilder_metgm1)
{
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/sample_ed1.gm");
    boost::shared_ptr<CDMReader> metgmReaderEd1(new MetGmCDMReader(fileName, XMLInputFile(topSrcDir+"/share/etc/cdmMetGmReaderConfig.xml")));
    const CDM& cdm = metgmReaderEd1->getCDM();
    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(cdm);
    // find an appropriate coordinate system for a variable
    string varName = "air_temperature_GND";
    vector<boost::shared_ptr<const CoordinateSystem> >::iterator csIt =
            find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(varName));
    if (csIt == coordSys.end()) BOOST_CHECK(false);
    CoordinateSystemSliceBuilder sb(cdm, *csIt);
    sb.setReferenceTimePos(1);
    sb.setTimeStartAndSize(0, 2);

    BOOST_CHECK(sb.getUnsetDimensionNames()[0] == "height_in_meters_above_ground_level_pid_5");
    BOOST_CHECK(sb.getUnsetDimensionNames()[1] == "latitude");
    BOOST_CHECK(sb.getUnsetDimensionNames()[2] == "longitude");

    boost::shared_ptr<Data> data = metgmReaderEd1->getDataSlice("air_temperature_GND", sb.getTimeVariableSliceBuilder());
//    BOOST_CHECK(data->size() == 2);
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
