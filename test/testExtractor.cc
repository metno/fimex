/*
 * Fimex
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
 */

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/CDMExtractor.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/SliceBuilder.h"
#include "fimex/TimeUnit.h"
#include "fimex/interpolation.h"

#include "testinghelpers.h"

#include "testinghelpers.h"

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_extract)
{
    if (!hasTestExtra())
        return;
    const string fileName = pathTestExtra("flth00.dat");
    CDMReader_p feltReader = CDMFileReaderFactory::create("felt", fileName, pathShareEtc("felt2nc_variables.xml"));
    std::shared_ptr<CDMExtractor> extract(new CDMExtractor(feltReader));
    extract->removeVariable("relative_humidity");
    TEST4FIMEX_CHECK_THROW(extract->getCDM().getVariable("relative_humidity"), CDMException);

    extract->reduceDimension("y", 10, 50);
    extract->reduceDimension("x", 80, 50); // spain
    FimexTime startTime(2007,5,16, 9);
    FimexTime endTime(2007,5,16, 20);
    extract->reduceTime(startTime, endTime); // 12 hours 9..20
    TEST4FIMEX_CHECK_EQ(extract->getData("y")->size(), 50);
    TEST4FIMEX_CHECK_EQ(extract->getData("x")->size(), 50);
    TEST4FIMEX_CHECK_EQ(extract->getData("time")->size(), 12);
    TEST4FIMEX_CHECK_EQ(extract->getData("altitude")->size(), 50 * 50);
    TEST4FIMEX_CHECK_EQ(extract->getData("precipitation_amount")->size(), 50 * 50 * 12);
    TEST4FIMEX_CHECK_EQ(extract->getData("air_temperature")->size(), 50 * 50 * 12);
    shared_array<float> precData1 = extract->getData("air_temperature")->asFloat();
    CDMFileReaderFactory::createWriter(extract, "netcdf", "test_extract_1.nc");

    // test chunked reading
    extract = std::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceTime(startTime, endTime); // 12 hours 9..20
    std::set<size_t> slices;
    slices.clear(); slices.insert(10); slices.insert(11); slices.insert(13); slices.insert(16);
    extract->reduceDimension("y", slices);
    slices.clear(); slices.insert(80); slices.insert(83);
    extract->reduceDimension("x", slices);
    TEST4FIMEX_CHECK_EQ(extract->getCDM().getDimension("y").getLength(), 4);
    TEST4FIMEX_CHECK_EQ(extract->getData("y")->size(), 4);
    TEST4FIMEX_CHECK_EQ(extract->getData("x")->size(), 2);
    TEST4FIMEX_CHECK_EQ(extract->getData("time")->size(), 12);
    TEST4FIMEX_CHECK_EQ(extract->getData("precipitation_amount")->size(), 4 * 2 * 12);
    shared_array<float> precData2 = extract->getData("air_temperature")->asFloat();
    for (size_t t = 0; t < 12; t++) {
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(0, 0, t, 50, 50, 12)], precData2[mifi_3d_array_position(0, 0, t, 2, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(3, 0, t, 50, 50, 12)], precData2[mifi_3d_array_position(1, 0, t, 2, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(0, 1, t, 50, 50, 12)], precData2[mifi_3d_array_position(0, 1, t, 2, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(3, 1, t, 50, 50, 12)], precData2[mifi_3d_array_position(1, 1, t, 2, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(0, 3, t, 50, 50, 12)], precData2[mifi_3d_array_position(0, 2, t, 2, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(3, 3, t, 50, 50, 12)], precData2[mifi_3d_array_position(1, 2, t, 2, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(0, 6, t, 50, 50, 12)], precData2[mifi_3d_array_position(0, 3, t, 2, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(3, 6, t, 50, 50, 12)], precData2[mifi_3d_array_position(1, 3, t, 2, 4, 12)]);
    }
    CDMFileReaderFactory::createWriter(extract, "netcdf", "test_extract_2.nc");

    extract = std::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceTime(startTime, endTime); // 12 hours 9..20
    slices.clear(); slices.insert(10); slices.insert(11); slices.insert(13); slices.insert(16);
    extract->reduceDimension("y", slices);
    extract->reduceDimension("x", 80, 50); // spain
    TEST4FIMEX_CHECK_EQ(extract->getCDM().getDimension("y").getLength(), 4);
    TEST4FIMEX_CHECK_EQ(extract->getData("y")->size(), 4);
    TEST4FIMEX_CHECK_EQ(extract->getData("x")->size(), 50);
    TEST4FIMEX_CHECK_EQ(extract->getData("time")->size(), 12);
    TEST4FIMEX_CHECK_EQ(extract->getScaledDataInUnit("time", "hours since 2007-05-16 09:00:00 +0000")->asInt()[0], 0);
    TEST4FIMEX_CHECK_EQ(extract->getScaledDataInUnit("time", "hours since 2007-05-16 09:00:00 +0000")->asInt()[4], 4);
    TEST4FIMEX_CHECK_EQ(extract->getScaledDataInUnit("time", "hours since 2007-05-16 09:00:00 +0000")->asInt()[11], 11);
    TEST4FIMEX_CHECK_EQ(extract->getData("precipitation_amount")->size(), 4 * 50 * 12);
    precData2 = extract->getData("air_temperature")->asFloat();
    for (size_t t = 0; t < 12; t++) {
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(0, 0, t, 50, 50, 12)], precData2[mifi_3d_array_position(0, 0, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(3, 0, t, 50, 50, 12)], precData2[mifi_3d_array_position(3, 0, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(0, 1, t, 50, 50, 12)], precData2[mifi_3d_array_position(0, 1, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(3, 1, t, 50, 50, 12)], precData2[mifi_3d_array_position(3, 1, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(0, 3, t, 50, 50, 12)], precData2[mifi_3d_array_position(0, 2, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(3, 3, t, 50, 50, 12)], precData2[mifi_3d_array_position(3, 2, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(0, 6, t, 50, 50, 12)], precData2[mifi_3d_array_position(0, 3, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(3, 6, t, 50, 50, 12)], precData2[mifi_3d_array_position(3, 3, t, 50, 4, 12)]);
    }

    // slicebuilder along x
    extract = std::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceTime(startTime, endTime); // 12 hours 9..20
    slices.clear(); slices.insert(10); slices.insert(11); slices.insert(13); slices.insert(16);
    extract->reduceDimension("y", slices);
    TEST4FIMEX_CHECK_EQ(extract->getData("y")->size(), 4);
    TEST4FIMEX_CHECK_EQ(extract->getData("time")->size(), 12);
    SliceBuilder sb(extract->getCDM(), "air_temperature");
    sb.setStartAndSize("x", 80, 50);

    DataPtr airTemp = extract->getDataSlice("air_temperature", sb);
    TEST4FIMEX_CHECK_EQ(airTemp->size(), 4 * 50 * 12);
    precData2 = airTemp->asFloat();

    for (size_t t = 0; t < 12; t++) {
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(0, 0, t, 50, 50, 12)], precData2[mifi_3d_array_position(0, 0, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(3, 0, t, 50, 50, 12)], precData2[mifi_3d_array_position(3, 0, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(0, 1, t, 50, 50, 12)], precData2[mifi_3d_array_position(0, 1, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(3, 1, t, 50, 50, 12)], precData2[mifi_3d_array_position(3, 1, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(0, 3, t, 50, 50, 12)], precData2[mifi_3d_array_position(0, 2, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(3, 3, t, 50, 50, 12)], precData2[mifi_3d_array_position(3, 2, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(0, 6, t, 50, 50, 12)], precData2[mifi_3d_array_position(0, 3, t, 50, 4, 12)]);
        TEST4FIMEX_CHECK_EQ(precData1[mifi_3d_array_position(3, 6, t, 50, 50, 12)], precData2[mifi_3d_array_position(3, 3, t, 50, 4, 12)]);
    }

    extract = std::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceTime(FimexTime(FimexTime::min_date_time), FimexTime(FimexTime::max_date_time));
    TEST4FIMEX_CHECK_EQ(extract->getData("time")->size(), 61);

    extract = std::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    // reduce sigma from 4 to 2
    extract->reduceVerticalAxis("", .5, .85);
    TEST4FIMEX_CHECK_EQ(extract->getData("sigma")->size(), 2);

    // reduce time to 0 and sigma to 0
    extract = std::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceVerticalAxis("", -0.1, -0.05);
    extract->reduceTime(FimexTime(2006,1,1), FimexTime(2006,1,2)); // time out of range
    TEST4FIMEX_CHECK_EQ(extract->getData("time")->size(), 0);
    CDMFileReaderFactory::createWriter(extract, "netcdf", "test_extract_0time.nc");

    // test selectVariable
    extract = std::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    std::set<std::string> variables;
    variables.insert("time");
    variables.insert("altitude");
    variables.insert("relative_humidity");
    extract->selectVariables(variables, true);
    TEST4FIMEX_CHECK(extract->getCDM().hasVariable("time"));
    TEST4FIMEX_CHECK(extract->getCDM().hasVariable("altitude"));
    TEST4FIMEX_CHECK(extract->getCDM().hasVariable("relative_humidity"));
    TEST4FIMEX_CHECK(extract->getCDM().hasVariable("x")); // auxiliary
    TEST4FIMEX_CHECK(extract->getCDM().hasVariable("y")); // auxiliary
    TEST4FIMEX_CHECK_EQ(false, extract->getCDM().hasVariable("precipitation_amount"));

    // test reduceLatLonBoundingBox
    extract = std::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceDimension("time", 0, 1);
    extract->reduceLatLonBoundingBox(55.,65., 5, 15);
    TEST4FIMEX_CHECK_EQ(extract->getData("x")->size(), 15);
    TEST4FIMEX_CHECK_EQ(extract->getData("y")->size(), 24);

    // test selection of non existing selectVariable
    extract = std::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    variables.insert("not_there");
    extract->selectVariables(variables);
    TEST4FIMEX_CHECK(extract->getCDM().hasVariable("relative_humidity"));
    TEST4FIMEX_CHECK_EQ(false, extract->getCDM().hasVariable("precipitation_amount"));
}
