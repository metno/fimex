/*
 * Fimex, testGribWriter.cc
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
 *  Created on: Dec 17, 2008
 *      Author: Heiko Klein
 */

#include "fimex_config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#include <boost/shared_ptr.hpp>
#include <boost/filesystem/operations.hpp>
#include "FeltCDMReader2.h"
#include "fimex/GribApiCDMWriter.h"

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

using namespace std;
using namespace MetNoFelt;
using namespace MetNoFimex;
namespace fs = boost::filesystem;

#include "testinghelpers.h"

BOOST_AUTO_TEST_CASE( test_feltGrib1Append )
{
    if (!hasTestExtra())
        return;
    const string fileName = pathTestExtra("flth00.dat");
    CDMReader_p feltReader(new FeltCDMReader2(fileName, pathShareEtc("felt2nc_variables.xml")));

    const string outputFile("test_append.grb1");
    fs::remove(outputFile.c_str());
    GribApiCDMWriter(feltReader, outputFile, 1, pathTest("cdmGribWriterConfig_append.xml"));
    fs::path out( outputFile );
    BOOST_CHECK(fs::exists(out));
    BOOST_CHECK(fs::file_size(out) > 5000000);

    const size_t size = fs::file_size(out);
    GribApiCDMWriter(feltReader, outputFile, 1, pathTest("cdmGribWriterConfig_append.xml"));
    BOOST_CHECK(fs::exists(out));
    BOOST_CHECK(fs::file_size(out) == 2*size);
    fs::remove(outputFile.c_str());
}


BOOST_AUTO_TEST_CASE( test_feltGrib1Write )
{
    if (!hasTestExtra())
        return;
    const string fileName = pathTestExtra("flth00.dat");
    CDMReader_p feltReader(new FeltCDMReader2(fileName, pathShareEtc("felt2nc_variables.xml")));

    const string outputFile("test.grb1");
    GribApiCDMWriter(feltReader, outputFile, 1, pathShareEtc("cdmGribWriterConfig.xml"));
    fs::path out( outputFile );
    BOOST_CHECK(fs::exists(out));
    BOOST_CHECK(fs::file_size(out) > 5000000);
}

BOOST_AUTO_TEST_CASE( test_feltGrib2Write )
{
    if (!hasTestExtra())
        return;
    const string fileName = pathTestExtra("flth00.dat");
    CDMReader_p feltReader(new FeltCDMReader2(fileName, pathShareEtc("felt2nc_variables.xml")));
    const string outputFile("test.grb2");
    GribApiCDMWriter(feltReader, outputFile, 2, pathShareEtc("cdmGribWriterConfig.xml"));
    fs::path out( outputFile );
    BOOST_CHECK(fs::exists(out));
    BOOST_CHECK(fs::file_size(out) > 5000000);
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
