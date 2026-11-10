/*
 * Fimex
 *
 * (C) Copyright 2009-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

#include "testinghelpers.h"

#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMQualityExtractor.h"
#include "fimex/Data.h"
#include "fimex/SharedArray.h"
#include "fimex/mifi_constants.h"

#include <memory>

using namespace std;
using namespace MetNoFimex;

#ifdef ENABLE_FELT
TEST4FIMEX_TEST_CASE(test_qualityExtract)
{
    auto feltReader = getFLTH00Reader();
    if (!feltReader)
        return;

    auto extract = std::make_shared<CDMQualityExtractor>(feltReader, "", pathTest("testQualityConfig.xml"));

    TEST4FIMEX_CHECK(extract->getQualityAssignment("air_temperature"));

    const auto qa_bla = extract->getQualityAssignment("relative_humidity");
    TEST4FIMEX_REQUIRE(qa_bla != nullptr);
    TEST4FIMEX_CHECK_EQ(qa_bla->statusVariable, "cloud_area_fraction");
    TEST4FIMEX_CHECK(qa_bla->flags.empty());
    TEST4FIMEX_REQUIRE(qa_bla->values.has_value());
    TEST4FIMEX_CHECK_EQ(qa_bla->values.value().size(), 6);
}

TEST4FIMEX_TEST_CASE(test_qualityExtract_convert)
{
    auto feltReader = getFLTH00Reader();
    if (!feltReader)
        return;
    auto qe = std::make_shared<CDMQualityExtractor>(feltReader, "", pathTest("testQualityConfig.xml"));

    TEST4FIMEX_CHECK(writeToFile(qe, "test_qualityExtract_convert.nc"));
}
#endif // ENABLE_FELT

#ifdef ENABLE_NETCDF
TEST4FIMEX_TEST_CASE(test_qualityExtract_mask)
{
    const string fileNameD = pathTest("testQEmask_data.nc");
    const string fileNameX = require("testQEmask.xml");
    const string fileNameM = pathTest("testQEmask_mask.nc");

    auto readerD = CDMFileReaderFactory::create("netcdf", fileNameD);
    auto mask = std::make_shared<CDMQualityExtractor>(readerD, "", fileNameX);

    auto sliceM = mask->getDataSlice("salt", 0);
    TEST4FIMEX_CHECK(sliceM);
    auto fillValue = mask->getCDM().getFillValue("salt");

    const int NXSI = 21, NETA = 16, N_SRHO=35;
    TEST4FIMEX_REQUIRE_EQ(sliceM->size(), N_SRHO * NXSI * NETA);
    auto valuesM = sliceM->asDouble();
    const int offset0 = 0, offset1 = 12+NXSI*12;
    TEST4FIMEX_CHECK_EQ(valuesM[offset0], fillValue);
    TEST4FIMEX_CHECK(fabs(valuesM[offset1] - 35.114) < 0.001);
}
#endif /* ENABLE_NETCDF */
