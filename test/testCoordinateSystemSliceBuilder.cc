/*
 * Fimex, testCoordinateSystemSliceBuilder.cc
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Jun 7, 2011
 *      Author: Heiko Klein
 */

#include "testinghelpers.h"
#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/CoordinateSystemSliceBuilder.h"
#include "fimex/Data.h"
#include "fimex/CDMReader.h"

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_cs_slicebuilder_simple)
{
    CDMReader_p reader = CDMFileReaderFactory::create("netcdf", pathTest("coordTest.nc"));
    TEST4FIMEX_CHECK(reader);

    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    CoordinateSystem_cp_v coordSys = listCoordinateSystems(reader);
    const CDM& cdm = reader->getCDM();

    // find an appropriate coordinate system for a variable
    const string varName = "air_temperature";
    CoordinateSystem_cp_v::iterator csIt = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(varName));
    TEST4FIMEX_REQUIRE(csIt != coordSys.end());

    CoordinateSystemSliceBuilder sb(cdm, *csIt);
    sb.setReferenceTimePos(1);
    sb.setTimeStartAndSize(0, 4);

    TEST4FIMEX_CHECK_EQ(sb.getUnsetDimensionNames()[0], "x");
    TEST4FIMEX_CHECK_EQ(sb.getUnsetDimensionNames()[1], "y");

    DataPtr data = reader->getDataSlice((*csIt)->getTimeAxis()->getName(), sb.getTimeVariableSliceBuilder());
    TEST4FIMEX_CHECK_EQ(data->size(), 4);
    TEST4FIMEX_CHECK_EQ(data->asInt()[1], 1179313200);
}

TEST4FIMEX_TEST_CASE(test_cs_slicebuilder_reftime)
{
    CDMReader_p reader = CDMFileReaderFactory::create("netcdf", pathTest("coordRefTimeTest.nc"));
    TEST4FIMEX_CHECK(reader);

    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    CoordinateSystem_cp_v coordSys = listCoordinateSystems(reader);
    TEST4FIMEX_CHECK(coordSys.size() > 0);
    const CDM& cdm = reader->getCDM();

    // find an appropriate coordinate system for a variable
    const string varName = "air_temperature";
    CoordinateSystem_cp_v::iterator csIt = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(varName));
    TEST4FIMEX_REQUIRE(csIt != coordSys.end());

    CoordinateSystemSliceBuilder sb(cdm, *csIt);
    sb.setReferenceTimePos(1);
    sb.setTimeStartAndSize(0, 2);

    TEST4FIMEX_CHECK_EQ(sb.getUnsetDimensionNames()[0], "x");
    TEST4FIMEX_CHECK_EQ(sb.getUnsetDimensionNames()[1], "y");

    DataPtr data = reader->getDataSlice((*csIt)->getTimeAxis()->getName(), sb.getTimeVariableSliceBuilder());
    TEST4FIMEX_CHECK_EQ(data->size(), 2);
    TEST4FIMEX_CHECK_EQ(data->asShort()[1], 27);
}
