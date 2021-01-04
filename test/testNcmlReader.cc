/*
 * Fimex, testNcmlReader.cc
 *
 * (C) Copyright 2012-2022, met.no
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
 *  Created on: Mar 13, 2012
 *      Author: Heiko Klein
 */

#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMconstants.h"
#include "fimex/Data.h"
#include "fimex/SliceBuilder.h"
#include "fimex/XMLInputFile.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "testinghelpers.h"

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_ncmlRead)
{
    const string fileName = pathTest("coordTest.nc"), ncmlName = pathTest("test.ncml");
    CDMReader_p reader(CDMFileReaderFactory::create("netcdf", fileName, XMLInputFile(ncmlName)));
    DataPtr data = reader->getDataSlice("sea_surface_temperature", 1);
    SliceBuilder sb(reader->getCDM(), "sea_surface_temperature");
    sb.setStartAndSize("time", 1, 1);
    DataPtr dataSlice = reader->getDataSlice("sea_surface_temperature", sb);
    TEST4FIMEX_CHECK_EQ(data->size(), dataSlice->size());
    auto d = data->asShort();
    auto ds = dataSlice->asShort();
    for (size_t i = 0; i < data->size(); i++) {
        TEST4FIMEX_CHECK_EQ(d[i], ds[i]);
    }

    // proj4-string removed
    CDMAttribute proj4;
    TEST4FIMEX_CHECK(!reader->getCDM().getAttribute("projection_1", "proj4", proj4));

    // find towgs84
    CoordinateSystem_cp_v coordSys = listCoordinateSystems(reader);
    CoordinateSystem_cp_v::iterator varSysIt = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator("sea_surface_temperature"));
    string s = (*varSysIt)->getProjection()->getProj4String();
    TEST4FIMEX_CHECK(s.find("+towgs84=") != string::npos);

    TEST4FIMEX_CHECK(reader->getCDM().hasDimension("Sigma"));

    // check attributes of renamed variable
    CDMAttribute attr;
    TEST4FIMEX_CHECK(reader->getCDM().getAttribute("snow_thickness", "metno_name", attr));
    TEST4FIMEX_CHECK(!reader->getCDM().getAttribute("snow_thickness", "long_name", attr));

    // check of new variable
    TEST4FIMEX_CHECK(reader->getCDM().hasVariable("new_var"));
    data = reader->getDataSlice("new_var", 1);
    TEST4FIMEX_CHECK_EQ(data->size(), 0);
    sb = SliceBuilder(reader->getCDM(), "new_var");
    sb.setStartAndSize("time", 1, 1);
    dataSlice = reader->getDataSlice("new_var", sb);
    TEST4FIMEX_CHECK_EQ(dataSlice->size(), 0);
}

TEST4FIMEX_TEST_CASE(cdm_ncml)
{
    const string fileName = pathTest("coordTest.nc");
    const string ncmlName = pathTest("test.ncml");
    CDMReader_p reader(CDMFileReaderFactory::create("netcdf", fileName, XMLInputFile(ncmlName)));
    TEST4FIMEX_REQUIRE(reader);

    const CDM& cdm = reader->getCDM();

    TEST4FIMEX_REQUIRE(cdm.hasVariable("x_wind"));
    CDMVariable var = cdm.getVariable("x_wind");
    TEST4FIMEX_CHECK(var.isSpatialVector());
    TEST4FIMEX_CHECK_EQ(var.getSpatialVectorCounterpart(), "y_wind");

    TEST4FIMEX_REQUIRE(cdm.hasVariable("y_wind"));
    var = cdm.getVariable("y_wind");
    TEST4FIMEX_CHECK(var.isSpatialVector());
    TEST4FIMEX_CHECK_EQ(var.getSpatialVectorCounterpart(), "x_wind");

    {
        const CDMAttribute& att = cdm.getAttribute("dummy", "important");
        TEST4FIMEX_CHECK_EQ(att.getDataType(), CDM_SHORT);
        DataPtr att_data = att.getData();
        TEST4FIMEX_REQUIRE(att_data);
        TEST4FIMEX_CHECK_EQ(att_data->getDataType(), CDM_SHORT);
        TEST4FIMEX_REQUIRE_EQ(att_data->size(), 4);
        shared_array<short> att_values = att_data->asShort();
        TEST4FIMEX_CHECK_EQ(att_values[2], 4);
    }
    {
        DataPtr data = reader->getData("dummy");
        TEST4FIMEX_REQUIRE(data);
        TEST4FIMEX_CHECK_EQ(data->getDataType(), CDM_FLOAT);
        TEST4FIMEX_REQUIRE_EQ(data->size(), 3);
        shared_array<float> values = data->asFloat();
        TEST4FIMEX_CHECK_EQ(values[2], -17);
    }
}
