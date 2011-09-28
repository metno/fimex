/*
 * Fimex, testFileReaderFactory.cc
 *
 * (C) Copyright 2010, met.no
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
 *  Created on: May 7, 2010
 *      Author: Heiko Klein
 */

#include "../config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMconstants.h"
#ifdef HAVE_NETCDF_H
#include "fimex/NetCDF_CDMReader.h"
#endif
using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_CDMconstants )
{
    string nc(mifi_get_filetype_name(MIFI_FILETYPE_NETCDF));
    BOOST_CHECK(nc == "netcdf");
    int nc_id = mifi_get_filetype(nc.c_str());
    BOOST_CHECK(nc_id == MIFI_FILETYPE_NETCDF);
#ifdef HAVE_NETCDF_H
    BOOST_CHECK(fimexHas(MIFI_FILETYPE_NETCDF));
#else
    BOOST_CHECK(fimexHas(MIFI_FILETYPE_NETCDF) == 0);
#endif
}

BOOST_AUTO_TEST_CASE( test_fileDetection )
{
    string topSrcDir(TOP_SRCDIR);
    BOOST_CHECK(CDMFileReaderFactory::detectFileType(topSrcDir + "/test/coordTest.nc") == MIFI_FILETYPE_NETCDF);
    std::string feltFile(topSrcDir + "/test/flth00.dat");
    if (ifstream(feltFile.c_str())) {
        BOOST_CHECK(CDMFileReaderFactory::detectFileType(feltFile) == MIFI_FILETYPE_FELT);
    }
    if (fimexHas(MIFI_FILETYPE_NETCDF)) {
        boost::shared_ptr<CDMReader> reader = CDMFileReaderFactory::create("netcdf", topSrcDir + "/test/coordTest.nc");
        BOOST_CHECK(reader.get() != 0);
#ifdef HAVE_NETCDF_H
        BOOST_CHECK(dynamic_cast<NetCDF_CDMReader*>(reader.get()) != 0);
#endif

    }
    BOOST_CHECK(true);
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif

