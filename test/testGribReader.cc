/*
 * Fimex, testGribReader.cc
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Oct 7, 2009
 *      Author: Heiko Klein
 */

#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMReader.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/Null_CDMWriter.h"
#include "fimex/SliceBuilder.h"
#include "fimex/XMLInput.h"

#include "testinghelpers.h"

#include <memory>
#include <vector>

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_read_grb1)
{
    if (!hasTestExtra())
        return;
    const string fileName = require("test.grb1"); // this is written by testGribWriter.cc

    defaultLogLevel(Logger::INFO);
    CDMReader_p grbReader = CDMFileReaderFactory::create("grib", fileName, XMLInputFile(pathTest("cdmGribReaderConfig_newEarth.xml")));
    //grbReader->getCDM().toXMLStream(cout);
    TEST4FIMEX_CHECK(grbReader->getCDM().hasVariable("x_wind_10m"));
    DataPtr data = grbReader->getDataSlice("x_wind_10m", 0);
    shared_array<float> dataFlt = data->asFloat();
    for (int i = 85*229+50; i < 85*229+150; i++) {
        // check data-range
        TEST4FIMEX_CHECK(fabs(dataFlt[i]) < 15);
        // check data-precision (0.1 according to setup-file)
        TEST4FIMEX_CHECK(1e-5 > fabs(10 * dataFlt[i] - MetNoFimex::round(10 * dataFlt[i])));
    }

    // check grib has new earth-radius
    CDMAttribute attr;
    TEST4FIMEX_CHECK(grbReader->getCDM().getAttribute("projection_polar_stereographic", "earth_radius", attr));
    TEST4FIMEX_CHECK(fabs(attr.getData()->asFloat()[0] - 6372000) < 1);
    // the following test has a tendency to fail when not all tests are run in correct order
    // a remaining grbml-file uses the wrong earth radius
    const float ac_x0 = grbReader->getData("x")->asFloat()[0], ex_x0 = -5719440,
            diff_x0 = std::abs(ac_x0 - ex_x0);
    TEST4FIMEX_CHECK_MESSAGE(diff_x0 < 1, "x_0 expected " << ex_x0 << " got " << ac_x0 << " difference " << diff_x0);

    // slicebuilder
    SliceBuilder sbX(grbReader->getCDM(), "x");
    sbX.setStartAndSize("x", 4, 10);
    TEST4FIMEX_CHECK_EQ(grbReader->getDataSlice("x", sbX)->size(), 10);

    SliceBuilder sb(grbReader->getCDM(), "x_wind_10m");
    sb.setStartAndSize("time", 0, 1);
    sb.setStartAndSize("x", 4, 10);
    sb.setStartAndSize("y", 2, 2);
    vector<size_t> dimStart = sb.getDimensionStartPositions();
    DataPtr dataS = grbReader->getDataSlice("x_wind_10m", sb);
    TEST4FIMEX_CHECK_EQ(20, dataS->size());

    writeToFile(grbReader, "test_read_grb1.nc");
}

TEST4FIMEX_TEST_CASE(test_read_grb2)
{
    if (!hasTestExtra())
        return;
    const string fileName = require("test.grb2"); // this is written by testGribWriter.cc

    defaultLogLevel(Logger::INFO);
    CDMReader_p grbReader = CDMFileReaderFactory::create("grib", fileName, XMLInputFile(pathShareEtc("cdmGribReaderConfig.xml")));
    writeToFile(grbReader, "test_grb2_out.nc");
}
