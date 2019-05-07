/*
 * Fimex, testMetgmReader.cc
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
 *  Created on: Jun 13, 2011
 *      Author: Aleksandar Babic
 */

#include "testinghelpers.h"

#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMReader.h"
#include "fimex/CoordinateSystemSliceBuilder.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/XMLInput.h"
#include "fimex/coordSys/CoordinateSystem.h"

#include <memory>
#include <vector>

using namespace std;
using namespace MetNoFimex;

#ifdef HAVE_NETCDF_H
static const std::string writertype = "netcdf";
#else
static const std::string writertype = "null";
#endif

TEST4FIMEX_TEST_CASE(test_read_sample_ed1)
{
    const string fileName = pathTest("sample_ed1.gm");

    defaultLogLevel(Logger::INFO);
    CDMReader_p metgmReaderEd1 = CDMFileReaderFactory::create("", fileName, XMLInputFile(pathShareEtc("cdmMetGmReaderConfig.xml")));
    CDMFileReaderFactory::createWriter(metgmReaderEd1, writertype, "testMetgmReadEd1.nc");
}

TEST4FIMEX_TEST_CASE(test_read_metgm2)
{
    const string fileName = pathTest("sample_ed2.gm");

    defaultLogLevel(Logger::INFO);
    CDMReader_p metgmReaderEd2 = CDMFileReaderFactory::create("", fileName, XMLInputFile(pathShareEtc("cdmMetGmReaderConfig.xml")));
    CDMFileReaderFactory::createWriter(metgmReaderEd2, writertype, "testMetgmReadEd2.nc");
}

TEST4FIMEX_TEST_CASE(test_slicebuilder_metgm1)
{
    const string fileName = pathTest("sample_ed1.gm");
    CDMReader_p metgmReaderEd1 = CDMFileReaderFactory::create("", fileName, XMLInputFile(pathShareEtc("cdmMetGmReaderConfig.xml")));
    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    CoordinateSystem_cp_v coordSys = listCoordinateSystems(metgmReaderEd1);
    const CDM& cdm = metgmReaderEd1->getCDM();
    // find an appropriate coordinate system for a variable
    string varName = "air_temperature_GND";
    CoordinateSystem_cp_v::iterator csIt = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(varName));
    TEST4FIMEX_REQUIRE(csIt != coordSys.end());
    CoordinateSystemSliceBuilder sb(cdm, *csIt);
    vector<string> dimNames = sb.getDimensionNames();
    sb.setReferenceTimePos(0);
    sb.setTimeStartAndSize(0, 2);

    TEST4FIMEX_CHECK_EQ(sb.getUnsetDimensionNames()[0], "height_in_meters_above_ground_level_pid_5");
    TEST4FIMEX_CHECK_EQ(sb.getUnsetDimensionNames()[1], "latitude");
    TEST4FIMEX_CHECK_EQ(sb.getUnsetDimensionNames()[2], "longitude");
    sb.setAll("height_in_meters_above_ground_level_pid_5");
    sb.setAll("latitude");
    sb.setAll("longitude");

    DataPtr data = metgmReaderEd1->getDataSlice("air_temperature_GND", sb);
    TEST4FIMEX_CHECK_EQ(data->size(), (7072 * 2));
}
