/*
 * Fimex, testGribWriter.cc
 *
 * (C) Copyright 2008-2024, met.no
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
#include "fimex/XMLUtils.h"

#include "GribApiCDMWriter.h"

#include <memory>

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_feltGrib1Append)
{
    CDMReader_p feltReader = getFLTH00Reader();
    if (!feltReader)
        return;

    const auto config = createXMLInput(pathTest("cdmGribWriterConfig_append.xml"));

    const string outputFile("test_append.grb1");
    MetNoFimex::remove(outputFile);
    GribApiCDMWriter(feltReader, outputFile, 1, config);
    TEST4FIMEX_CHECK(MetNoFimex::exists(outputFile));
    TEST4FIMEX_CHECK(MetNoFimex::file_size(outputFile) > 5000000);

    const size_t size = MetNoFimex::file_size(outputFile);
    GribApiCDMWriter(feltReader, outputFile, 1, config);
    TEST4FIMEX_CHECK(::exists(outputFile));
    TEST4FIMEX_CHECK_EQ(MetNoFimex::file_size(outputFile), 2 * size);
    MetNoFimex::remove(outputFile);
}

TEST4FIMEX_TEST_CASE(test_feltGrib1Write)
{
    CDMReader_p feltReader = getFLTH00Reader();
    if (!feltReader)
        return;

    const auto config = createXMLInput(pathShareEtc("cdmGribWriterConfig.xml"));
    const string outputFile("test.grb1");
    GribApiCDMWriter(feltReader, outputFile, 1, config);
    TEST4FIMEX_CHECK(MetNoFimex::exists(outputFile));
    TEST4FIMEX_CHECK(MetNoFimex::file_size(outputFile) > 5000000);
    // cannot remove file as it is used by other tests
}

TEST4FIMEX_TEST_CASE(test_feltGrib2Write)
{
    CDMReader_p feltReader = getFLTH00Reader();
    if (!feltReader)
        return;

    const auto config = createXMLInput(pathShareEtc("cdmGribWriterConfig.xml"));
    const string outputFile("test.grb2");
    GribApiCDMWriter(feltReader, outputFile, 2, config);
    TEST4FIMEX_CHECK(MetNoFimex::exists(outputFile));
    TEST4FIMEX_CHECK(MetNoFimex::file_size(outputFile) > 5000000);
    // cannot remove file as it is used by other tests
}
