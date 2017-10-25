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

#include "fimex_config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

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

#include "testinghelpers.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE(test_write_hirlam12_nc) {
    const string ncSource = pathTest("hirlam12.nc");

    defaultLogLevel(Logger::INFO);

    // create reader that reads ncSource
    CDMReader_p netcdfReader(new NetCDF_CDMReader(ncSource));
    BOOST_CHECK(true); // made it so far

    // use the metgm writer to create mgm file based on nc source
    const std::string mgmFile("hirlam12.mgm");
    MetGmCDMWriter(netcdfReader, mgmFile, pathShareEtc("cdmMetGmWriterConfig.xml"));
    BOOST_CHECK(true); // made it so far

    // create metgm reader to read newly writen mgm file
    CDMReader_p mgmReader(new MetGmCDMReader(mgmFile, XMLInputFile(pathShareEtc("cdmMetGmReaderConfig.xml"))));
    BOOST_CHECK(true); // made it so far

    // from metgm reader write again to nc file and compare:
    // hirlam12.nc and hirlam12.mgm.nc
    const std::string newNcFile("hirlam12.mgm.nc");
    NetCDF_CDMWriter(mgmReader, newNcFile);
    BOOST_CHECK(true); // made it so far
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
