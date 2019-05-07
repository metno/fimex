/*
 * Fimex, testFillWriter.cc
 *
 * (C) Copyright 2013, met.no
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
 *  Created on: Mar 25, 2013
 *      Author: heikok
 */

#include "testinghelpers.h"
#include "fimex/FillWriter.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_fillWriter)
{
    const string fileName = pathTest("coordRefTimeTest.nc");
    const string inFillName = pathTest("fillIn2.nc");
    const string outFile = "fillOut.nc";
    copyFile(fileName, outFile);

    {
        CDMReader_p in = CDMFileReaderFactory::create("netcdf", inFillName);
        CDMReaderWriter_p out = CDMFileReaderFactory::createReaderWriter("netcdf", outFile);

        // sanity check before test
        TEST4FIMEX_CHECK_EQ(out->getCDM().getDimension("refTime").getLength(), 2);
        TEST4FIMEX_CHECK_EQ(out->getCDM().getDimension("sigma").getLength(), 4);

        TEST4FIMEX_CHECK_EQ(in->getCDM().getDimension("refTime").getLength(), 2);
        TEST4FIMEX_CHECK_EQ(in->getCDM().getDimension("sigma").getLength(), 2);

        FillWriter(in, out);
    // close the writer before re-reading it
    }
    {
        CDMReader_p out = CDMFileReaderFactory::create("netcdf", outFile);

        TEST4FIMEX_CHECK_EQ(out->getCDM().getDimension("refTime").getLength(), 3);
        TEST4FIMEX_CHECK_EQ(out->getCDM().getDimension("sigma").getLength(), 4);
        TEST4FIMEX_CHECK_EQ(out->getData("sigma")->asInt()[0], 300);
        TEST4FIMEX_CHECK_EQ(out->getData("sigma")->asInt()[3], 1000);
        TEST4FIMEX_CHECK_EQ(out->getData("refTime")->asInt()[0], 12);
        TEST4FIMEX_CHECK_EQ(out->getData("refTime")->asInt()[2], 36);

        TEST4FIMEX_CHECK_EQ(out->getDataSlice("cloud_area_fraction_in_atmosphere_layer", 0)->asInt()[11 * 11 * 2], -32767); // first number, 3rd level
        TEST4FIMEX_CHECK_EQ(out->getDataSlice("cloud_area_fraction_in_atmosphere_layer", 2)->asInt()[11 * 11 * 4],
                            -32767); // last level, last number, first offsetTime
        TEST4FIMEX_CHECK_EQ(out->getDataSlice("cloud_area_fraction_in_atmosphere_layer", 2)->asInt()[11 * 11 * 4 * 2 - 1], -32767); // last level, last number
    }
}

TEST4FIMEX_TEST_CASE(test_fillWriterConfig)
{
    const string fileName = pathTest("coordRefTimeTest.nc");
    const string inFillName = pathTest("fillIn3.nc");
    const string outFile = "fillOut.nc";
    copyFile(fileName, outFile);
    {
        CDMReader_p in = CDMFileReaderFactory::create("netcdf", inFillName);
        CDMReaderWriter_p out = CDMFileReaderFactory::createReaderWriter("netcdf", outFile);
        FillWriter(in, out, pathTest("fillWriterConfig.xml"));
        // close the writer before re-reading it
    }
    {
        CDMReader_p out = CDMFileReaderFactory::create("netcdf", outFile);
        TEST4FIMEX_CHECK(out->getData("longitude")->asFloat()[2 * 11] + 13 < 1e-5);
        TEST4FIMEX_CHECK(out->getData("latitude")->asFloat()[2 * 11] - 28 < 1e-5);
    }
}
