/*
 * Fimex
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
 */

#include "testinghelpers.h"

#include "fimex/CDMException.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/XMLUtils.h"

#include "NetCDF_CDMWriter.h"

#include <memory>

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_feltNetcdfWrite)
{
    CDMReader_p feltReader = getFLTH00Reader();
    if (!feltReader)
        return;
    CDMFileReaderFactory::createWriter(feltReader, "netcdf", "test_feltNetcdfWrite.nc");
    // cannot remove file as it is used by other tests
}

TEST4FIMEX_TEST_CASE(test_feltNetcdfWriteConfig)
{
    CDMReader_p feltReader = getFLTH00Reader();
    if (!feltReader)
        return;

    const std::string outputFileName = "test_feltNetcdfWriteConfig.nc";
    const auto config = createXMLInput(pathShareEtc("cdmWriterConfigDeprecated.xml"));
    NetCDF_CDMWriter writer(feltReader, outputFileName, config);
    TEST4FIMEX_CHECK_EQ(writer.getVariableName("sea_level_pressure"), "sea_pressure");
    TEST4FIMEX_CHECK_EQ(writer.getDimensionName("x"), "x_c");
    TEST4FIMEX_CHECK_EQ(writer.getVariableName("x"), "x_c");
    TEST4FIMEX_CHECK_EQ(writer.getAttribute("air_temperature", "standard_name").getStringValue(), "temperature");

    TEST4FIMEX_CHECK_THROW(writer.getAttribute(CDM::globalAttributeNS(), "comment"), CDMException);
    // "global attribute '" << att << "' does not exist, expected exception");

    TEST4FIMEX_CHECK_THROW(writer.getAttribute("surface_snow_thickness", "long_name"), CDMException);
    // "variable '" << var << "' has no attribute '" << att << "', expected exception");

    remove(outputFileName);
}
