/*
 * Fimex, testNcmlAggregationReader.cc
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
 *  Created on: Apr 17, 2013
 *      Author: heikok
 */

#include "testinghelpers.h"

#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMconstants.h"
#include "fimex/CDMReader.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/NetCDF_CDMWriter.h"

#include <unistd.h>

using namespace std;
using namespace MetNoFimex;

namespace {
struct TestConfig {
    char oldDir[2048];
    TestConfig();
    ~TestConfig();
};

TestConfig::TestConfig()
{
    char* cwd = getcwd(oldDir, sizeof(oldDir));
    TEST4FIMEX_REQUIRE(cwd == oldDir);
    const string test_data = pathTest("data");
    if (chdir(test_data.c_str()) != 0) {
        TEST4FIMEX_FAIL("cannot chdir to '" << test_data << "'");
    }
}
TestConfig::~TestConfig()
{
    if (chdir(oldDir) != 0) {
        TEST4FIMEX_FAIL("cannot chdir to '" << oldDir << "'");
    }
}
} // namespace

TEST4FIMEX_FIXTURE_TEST_CASE(test_joinExisting, TestConfig)
{
    //defaultLogLevel(Logger::DEBUG);
    const string ncmlName = require("joinExistingAgg.ncml");
    CDMReader_p reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NCML, ncmlName));
    TEST4FIMEX_REQUIRE(reader);
    TEST4FIMEX_REQUIRE(reader->getCDM().getUnlimitedDim());
    TEST4FIMEX_CHECK_EQ(reader->getCDM().getUnlimitedDim()->getLength(), 5);
    TEST4FIMEX_CHECK_EQ(reader->getDataSlice("unlim", 3)->asShort()[0], 4);
    TEST4FIMEX_CHECK_EQ(reader->getDataSlice("multi", 3)->asShort()[1], -4);

    SliceBuilder sb(reader->getCDM(), "multi");
    sb.setStartAndSize("unlim", 3, 1);
    TEST4FIMEX_CHECK_EQ(reader->getDataSlice("multi", sb)->asShort()[1], -4);

    sb = SliceBuilder(reader->getCDM(), "unlim");
    sb.setStartAndSize("unlim", 3, 1);
    TEST4FIMEX_CHECK_EQ(reader->getDataSlice("unlim", sb)->asShort()[0], 4);
}

TEST4FIMEX_FIXTURE_TEST_CASE(test_joinExistingSuffix, TestConfig)
{
    //defaultLogLevel(Logger::DEBUG);
    const string ncmlName = require("joinExistingAggSuffix.ncml");
    CDMReader_p reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NCML, ncmlName));
    const CDMDimension* unlim = reader->getCDM().getUnlimitedDim();
    TEST4FIMEX_REQUIRE(unlim);
    TEST4FIMEX_CHECK_EQ(unlim->getLength(), 5);
    DataPtr slice = reader->getDataSlice("unlim", 3);
    TEST4FIMEX_REQUIRE(slice);
    TEST4FIMEX_REQUIRE_EQ(slice->size(), 1);
    shared_array<short> values = slice->asShort();
    TEST4FIMEX_REQUIRE(values);
    TEST4FIMEX_CHECK_EQ(values[0], 4);

    slice = reader->getDataSlice("multi", 3);
    TEST4FIMEX_REQUIRE(slice);
    TEST4FIMEX_REQUIRE_EQ(slice->size(), 2);
    values = slice->asShort();
    TEST4FIMEX_REQUIRE(values);
    TEST4FIMEX_CHECK_EQ(values[1], -4);
}

TEST4FIMEX_FIXTURE_TEST_CASE(test_aggNothing, TestConfig)
{
    //defaultLogLevel(Logger::DEBUG);
    const string ncmlName = require("aggNothing.ncml");
    CDMReader_p reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NCML, ncmlName));
    TEST4FIMEX_CHECK_EQ(reader->getCDM().getVariables().size(), 0);
}

TEST4FIMEX_FIXTURE_TEST_CASE(test_aggWrong, TestConfig)
{
    //defaultLogLevel(Logger::DEBUG);
    const string ncmlName = require("aggWrong.ncml");
    // suppress ERROR: file unreadable
    defaultLogLevel(Logger::FATAL);
    CDMReader_p reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NCML, ncmlName));
    defaultLogLevel(Logger::INFO);
    TEST4FIMEX_CHECK_EQ(reader->getCDM().getVariables().size(), 0);
}

TEST4FIMEX_FIXTURE_TEST_CASE(test_aggNewDim, TestConfig)
{
    const string ncmlName = require("aggNewDim.ncml");
    defaultLogLevel(Logger::FATAL);
    CDMReader_p reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NCML, ncmlName));
    const std::string test_output = std::string(oldDir) + "/test_aggNewDim.nc";

    NetCDF_CDMWriter(reader, test_output, "", 4); // does not work for netcdf-3
    CDMReader_p reader2(CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, test_output));
    DataPtr slice = reader->getDataSlice("notlimited", 2);
    TEST4FIMEX_REQUIRE(slice);
    TEST4FIMEX_REQUIRE_GT(slice->size(), 0);
    shared_array<int> values = slice->asInt();
    TEST4FIMEX_REQUIRE(values);
    TEST4FIMEX_CHECK_EQ(values[0], 3);
}

TEST4FIMEX_FIXTURE_TEST_CASE(test_aggNewDim2, TestConfig)
{
    const string ncmlName = require("aggNewDim.ncml");
    defaultLogLevel(Logger::FATAL);
    CDMReader_p reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NCML, ncmlName));
    const std::string test_output = std::string(oldDir) + "/test_aggNewDim2.nc";

    SliceBuilder sb(reader->getCDM(), "multi");
    sb.setStartAndSize("notlimited", 2, 1);
    DataPtr slice = reader->getDataSlice("multi", sb);
    TEST4FIMEX_REQUIRE(slice);
    TEST4FIMEX_REQUIRE_GE(slice->size(), 2);
    const shared_array<int> values = slice->asInt();
    TEST4FIMEX_REQUIRE(values);
    TEST4FIMEX_CHECK_EQ(values[0], 4);
    TEST4FIMEX_CHECK_EQ(values[1], -4);
}

TEST4FIMEX_FIXTURE_TEST_CASE(test_union, TestConfig)
{
    //defaultLogLevel(Logger::DEBUG);
    const string ncmlName = require("unionAgg.ncml");
    CDMReader_p reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NCML, ncmlName));
    TEST4FIMEX_CHECK(reader->getCDM().hasVariable("b"));
    TEST4FIMEX_CHECK(reader->getCDM().hasVariable("extra"));
    TEST4FIMEX_CHECK(reader->getCDM().hasVariable("multi"));
    TEST4FIMEX_CHECK(reader->getCDM().hasVariable("unlim"));
    TEST4FIMEX_CHECK_EQ(reader->getDataSlice("extra", 1)->asShort()[0], -1);
    TEST4FIMEX_CHECK_EQ(reader->getDataSlice("multi", 1)->asShort()[1], -2);

    TEST4FIMEX_CHECK(reader->getCDM().hasVariable("multi"));

    SliceBuilder sb(reader->getCDM(), "multi");
    sb.setStartAndSize("unlim", 1, 1);
    TEST4FIMEX_CHECK_EQ(reader->getDataSlice("multi", sb)->asShort()[1], -2);
}
