/*
 * Fimex, testVerticalVelocity.cc
 *
 * (C) Copyright 2014, met.no
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
 *  Created on: Nov 25, 2014
 *      Author: heikok
 */

#include "testinghelpers.h"

#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMProcessor.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMconstants.h"
#include "fimex/Data.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/interpolation.h"
#include "fimex/min_max.h"

#include <cassert>

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_mifi_compute_vertical_velocity)
{
    CDMReader_p reader(CDMFileReaderFactory::create("netcdf", pathTest("verticalVelocity.nc")));
    if (reader.get() == 0) return; // no support for netcdf4

    CoordinateSystem_cp_v coordSys = listCoordinateSystems(reader);
    const CDM& cdm = reader->getCDM();

    DataPtr apD = reader->getScaledDataInUnit("ap0", "Pa");
    DataPtr bD = reader->getScaledData("b0");

    CoordinateSystem_cp_v::iterator varSysIt = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator("air_temperature_ml"));
    TEST4FIMEX_REQUIRE(varSysIt != coordSys.end());
    CoordinateAxis_cp xAxis = (*varSysIt)->getGeoXAxis(); // X or Lon
    CoordinateAxis_cp yAxis = (*varSysIt)->getGeoYAxis(); // Y or Lat
    CoordinateAxis_cp zAxis = (*varSysIt)->getGeoZAxis(); // Z
    CoordinateAxis_cp tAxis = (*varSysIt)->getTimeAxis(); // time

    //cerr << "found axes" << endl;

    size_t nx = reader->getData(xAxis->getName())->size();
    size_t ny = reader->getData(yAxis->getName())->size();
    size_t nz = reader->getData(zAxis->getName())->size();

    //cerr << "(x,y,z) = " << "(" << nx << "," << ny << "," << nz << ")" << endl;

    assert (nx*ny*nz > 0);

    size_t timePos = 0;

    DataPtr zsD = reader->getScaledDataSliceInUnit("geopotential_pl", "m^2/s^2", timePos); // need to devide by g=9.81m/s2
    TEST4FIMEX_CHECK_EQ(zsD->size(), nx * ny);
    DataPtr psD = reader->getScaledDataSliceInUnit("surface_air_pressure", "Pa", timePos);
    TEST4FIMEX_CHECK_EQ(psD->size(), nx * ny);
    DataPtr uD = reader->getScaledDataSliceInUnit("x_wind_ml", "m/s", timePos);
    TEST4FIMEX_CHECK_EQ(uD->size(), nx * ny * nz);
    DataPtr vD = reader->getScaledDataSliceInUnit("y_wind_ml", "m/s", timePos);
    TEST4FIMEX_CHECK_EQ(vD->size(), nx * ny * nz);
    DataPtr tD = reader->getScaledDataSliceInUnit("air_temperature_ml", "K", timePos);
    TEST4FIMEX_CHECK_EQ(tD->size(), nx * ny * nz);

    //cerr << "got data" << endl;

    float gInv = 1 / 9.806;
    shared_array<float> zs = zsD->asFloat();
    transform(&zs[0], &zs[0]+(nx*ny), &zs[0], std::bind1st(multiplies<float>(), gInv));

    //cerr << "transformed zs" << endl;

    // output
    double dx = fabs(reader->getData(xAxis->getName())->asDouble()[1] - reader->getData(xAxis->getName())->asDouble()[0]);
    double dy = fabs(reader->getData(yAxis->getName())->asDouble()[1] - reader->getData(yAxis->getName())->asDouble()[0]);
    string lat, lon;
    cdm.getLatitudeLongitude("air_temperature_ml", lat, lon);
    //cerr << lat << " " << lon << endl;
    DataPtr lonVals = reader->getScaledDataInUnit(lon, "degree");
    DataPtr latVals = reader->getScaledDataInUnit(lat, "degree");
    shared_array<float> gridDistX(new float[nx * ny]());
    shared_array<float> gridDistY(new float[nx * ny]());
    TEST4FIMEX_CHECK_EQ(MIFI_OK, mifi_griddistance(nx, ny, lonVals->asDouble().get(), latVals->asDouble().get(), gridDistX.get(), gridDistY.get()));
    //cerr << "got gridDistance" << endl;
    shared_array<float> w(new float[nx * ny * nz]());
    TEST4FIMEX_CHECK_EQ(MIFI_OK,
                        mifi_compute_vertical_velocity(nx, ny, nz, dx, dy, gridDistX.get(), gridDistY.get(), apD->asDouble().get(), bD->asDouble().get(),
                                                       zs.get(), psD->asFloat().get(), uD->asFloat().get(), vD->asFloat().get(), tD->asFloat().get(), w.get()));

    DataPtr hyD = reader->getScaledData("hybrid0");
    shared_array<float> hy = hyD->asFloat();
    for (size_t k = 0; k < nz; ++k) {
        pair<float*, float*> minMax = minmax_element(&w[k * nx * ny], &w[k * nx * ny] + nx * ny);
        TEST4FIMEX_CHECK(*minMax.first > -4);
        TEST4FIMEX_CHECK(*minMax.second < 4);
        //printf("k=%d, eta=%f: min = %f, max = %f m/s\n", k, hy[k], *minMax.first, *minMax.second);
    }
}

TEST4FIMEX_TEST_CASE(test_cdmprocessor_addverticalvelocity)
{
    CDMReader_p reader(CDMFileReaderFactory::create("netcdf", pathTest("verticalVelocity.nc")));
    if (reader.get() == 0) return; // no support for netcdf4

    std::shared_ptr<CDMProcessor> proc(new CDMProcessor(reader));
    proc->addVerticalVelocity();
    TEST4FIMEX_CHECK(proc->getCDM().hasVariable("upward_air_velocity_ml"));
    CoordinateSystem_cp_v coordSys = listCoordinateSystems(proc);

    CoordinateSystem_cp_v::iterator varSysIt = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator("upward_air_velocity_ml"));
    TEST4FIMEX_REQUIRE(varSysIt != coordSys.end());

    CoordinateAxis_cp xAxis = (*varSysIt)->getGeoXAxis(); // X or Lon
    CoordinateAxis_cp yAxis = (*varSysIt)->getGeoYAxis(); // Y or Lat
    CoordinateAxis_cp zAxis = (*varSysIt)->getGeoZAxis(); // Z
    CoordinateAxis_cp tAxis = (*varSysIt)->getTimeAxis(); // time

    //cerr << "found axes" << endl;

    size_t nx = proc->getData(xAxis->getName())->size();
    size_t ny = proc->getData(yAxis->getName())->size();
    size_t nz = proc->getData(zAxis->getName())->size();
    size_t nt = proc->getData(tAxis->getName())->size();

    DataPtr hyD = proc->getScaledData("hybrid0");
    shared_array<float> hy = hyD->asFloat();
    for (size_t t = 0; t < nt; ++t) {
        shared_array<float> w = proc->getScaledDataSliceInUnit("upward_air_velocity_ml", "m/s", t)->asFloat();
        for (size_t k = 0; k < nz; ++k) {
            pair<float*, float*> minMax = minmax_element(&w[k * nx * ny], &w[k * nx * ny] + nx * ny);
            TEST4FIMEX_CHECK(*minMax.first > -4);
            TEST4FIMEX_CHECK(*minMax.second < 4);
            //printf("k=%d, eta=%f: min = %f, max = %f m/s\n", k, hy[k], *minMax.first, *minMax.second);
        }
    }
}
