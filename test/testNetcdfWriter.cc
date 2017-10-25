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

#include "testinghelpers.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#include <boost/shared_ptr.hpp>
#include "FeltCDMReader2.h"

#include "fimex/NetCDF_CDMWriter.h"

using namespace std;
using namespace MetNoFelt;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_feltNetcdfWrite )
{
    if (!hasTestExtra())
        return;
    const string fileName = pathTestExtra("flth00.dat");
    CDMReader_p feltReader(new FeltCDMReader2(fileName, pathShareEtc("felt2nc_variables.xml")));
    NetCDF_CDMWriter(feltReader, "test_feltNetcdfWrite.nc");
}

BOOST_AUTO_TEST_CASE( test_feltNetcdfWriteConfig )
{
    if (!hasTestExtra())
        return;
    const string fileName = pathTestExtra("flth00.dat");
    CDMReader_p feltReader(new FeltCDMReader2(fileName, pathShareEtc("felt2nc_variables.xml")));
    NetCDF_CDMWriter writer(feltReader, "test_feltNetcdfWriteConfig.nc", pathShareEtc("cdmWriterConfigDeprecated.xml"));
    BOOST_CHECK(writer.getVariableName("sea_level_pressure") == "sea_pressure");
    BOOST_CHECK(writer.getDimensionName("x") == "x_c");
    BOOST_CHECK(writer.getVariableName("x") == "x_c");
    BOOST_CHECK(writer.getAttribute("air_temperature", "standard_name").getStringValue() == "temperature");

    try {
        const std::string att = "comment";
        writer.getAttribute(CDM::globalAttributeNS(), att);
        BOOST_FAIL("global attribute '" << att << "' does not exist, expected exception");
    } catch (exception& ex) {
    }
    try {
        const std::string var = "surface_snow_thickness", att = "long_name";
        writer.getAttribute(var, att);
        BOOST_FAIL("variable '" << var << "' has no attribute '" << att << "', expected exception");
    } catch (CDMException& ex) {
    }
}

#endif // HAVE_BOOST_UNIT_TEST_FRAMEWORK
