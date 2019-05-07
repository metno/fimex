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

#include "testinghelpers.h"

#include "fimex/CDMFileReaderFactory.h"
#include "fimex/GribApiCDMWriter.h"

#include <memory>

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_feltGrib1Append)
{
    if (!hasTestExtra())
        return;
    const string fileName = pathTestExtra("flth00.dat");
    CDMReader_p feltReader = CDMFileReaderFactory::create("felt", fileName, pathShareEtc("felt2nc_variables.xml"));

    const string outputFile("test_append.grb1");
    MetNoFimex::remove(outputFile);
    GribApiCDMWriter(feltReader, outputFile, 1, pathTest("cdmGribWriterConfig_append.xml"));
    TEST4FIMEX_CHECK(MetNoFimex::exists(outputFile));
    TEST4FIMEX_CHECK(MetNoFimex::file_size(outputFile) > 5000000);

    const size_t size = MetNoFimex::file_size(outputFile);
    GribApiCDMWriter(feltReader, outputFile, 1, pathTest("cdmGribWriterConfig_append.xml"));
    TEST4FIMEX_CHECK(::exists(outputFile));
    TEST4FIMEX_CHECK_EQ(MetNoFimex::file_size(outputFile), 2 * size);
    MetNoFimex::remove(outputFile);
}

TEST4FIMEX_TEST_CASE(test_feltGrib1Write)
{
    if (!hasTestExtra())
        return;
    const string fileName = pathTestExtra("flth00.dat");
    CDMReader_p feltReader = CDMFileReaderFactory::create("felt", fileName, pathShareEtc("felt2nc_variables.xml"));

    const string outputFile("test.grb1");
    GribApiCDMWriter(feltReader, outputFile, 1, pathShareEtc("cdmGribWriterConfig.xml"));
    TEST4FIMEX_CHECK(MetNoFimex::exists(outputFile));
    TEST4FIMEX_CHECK(MetNoFimex::file_size(outputFile) > 5000000);
}

TEST4FIMEX_TEST_CASE(test_feltGrib2Write)
{
    if (!hasTestExtra())
        return;
    const string fileName = pathTestExtra("flth00.dat");
    CDMReader_p feltReader = CDMFileReaderFactory::create("felt", fileName, pathShareEtc("felt2nc_variables.xml"));
    const string outputFile("test.grb2");
    GribApiCDMWriter(feltReader, outputFile, 2, pathShareEtc("cdmGribWriterConfig.xml"));
    TEST4FIMEX_CHECK(MetNoFimex::exists(outputFile));
    TEST4FIMEX_CHECK(MetNoFimex::file_size(outputFile) > 5000000);
}
