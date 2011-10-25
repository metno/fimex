/*
 * Fimex, testMetGmWriter.cc
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
#include "fimex/MetGmCDMWriter.h"
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/NetCDF_CDMReader.h"
#include "fimex/Null_CDMWriter.h"
#include "fimex/Logger.h"

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE(test_write_hirlam12_nc) {
    string topSrcDir(TOP_SRCDIR);
    string ncSource(topSrcDir+"/test/hirlam12.nc");
    if (!ifstream(ncSource.c_str())) {
        // no testfile, skip test
        return;
    }

    defaultLogLevel(Logger::INFO);

    // create reader that reads ncSource
    boost::shared_ptr<CDMReader> netcdfReader(new NetCDF_CDMReader(ncSource));
    BOOST_CHECK(true); // made it so far

    // use the metgm writer to create mgm file based on nc source
    std::string mgmFile("hirlam12.mgm");
    MetGmCDMWriter(netcdfReader, mgmFile, topSrcDir + "/share/etc/cdmMetGmWriterConfig.xml");
    BOOST_CHECK(true); // made it so far

    // create metgm reader to read newly writen mgm file
    boost::shared_ptr<CDMReader> mgmReader(new MetGmCDMReader(mgmFile, XMLInputFile(topSrcDir + "/share/etc/cdmMetGmReaderConfig.xml")));
    BOOST_CHECK(true); // made it so far

    // from metgm reader write again to nc file and compare:
    // hirlam12.nc and hirlam12.mgm.nc
    std::string newNcFile("hirlam12.mgm.nc");
    NetCDF_CDMWriter(mgmReader, newNcFile);
    BOOST_CHECK(true); // made it so far
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
