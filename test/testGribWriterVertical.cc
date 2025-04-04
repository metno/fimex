/*
 * Fimex, testGribWriterVertical.cc
 *
 * (C) Copyright 2024, met.no
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

#include "testinghelpers.h"

#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/Data.h"
#include "fimex/XMLUtils.h"

#include "GribApiCDMWriter.h"

using namespace MetNoFimex;

namespace {

std::vector<double> getValues(CDMReader_p reader, const std::string& varName)
{
    const auto data = reader->getData(varName);
    const auto values = data->asDouble();
    return std::vector<double>(values.get(), values.get() + data->size());
}

} // namespace

TEST4FIMEX_TEST_CASE(Grib2WriteVertical)
{
    const auto pathInput = pathTest("testdata_grib_writer_vertical/input.nc");
    const auto pathGrib = "grib_writer_vertical.grib";

    CDMReader_p inputReader = CDMFileReaderFactory::create("nc4", pathInput);
    TEST4FIMEX_REQUIRE(inputReader);
    TEST4FIMEX_CHECK(inputReader->getCDM().hasVariable("hybrid"));

    {
        const auto config = createXMLInput(pathTest("testdata_grib_writer_vertical/cdmGribWriterConfig.xml"));
        GribApiCDMWriter gw(inputReader, pathGrib, 2, config);
        TEST4FIMEX_CHECK(MetNoFimex::exists(pathGrib));
    }

    CDMReader_p gribReader = CDMFileReaderFactory::create("grib", pathGrib,
                    pathTest("testdata_grib_writer_vertical/AromeEPSGribReaderConfig.xml"));
    TEST4FIMEX_REQUIRE(gribReader);
    TEST4FIMEX_CHECK(gribReader->getCDM().hasVariable("hybrid"));

    const auto nv = getValues(inputReader, "hybrid");
    const auto gv = getValues(gribReader, "hybrid");
    TEST4FIMEX_REQUIRE_EQ(gv.size(), nv.size());
    for (size_t i = 0; i<gv.size(); ++i) {
        TEST4FIMEX_CHECK_CLOSE(gv[i], nv[i], 1e-5);
    }

    remove(pathGrib);
}
