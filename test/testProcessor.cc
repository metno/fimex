/*
 * Fimex, testProcessor.cc
 *
 * (C) Copyright 2012, met.no
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
 *  Created on: Mar 19, 2012
 *      Author: Heiko Klein
 */

#include "testinghelpers.h"
#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMProcessor.h"
#include "fimex/CDMconstants.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include <boost/shared_array.hpp>
#include <memory>

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_accumulate)
{
//    defaultLogLevel(Logger::DEBUG);
    const string fileName = pathTest("coordTest.nc");
    CDMReader_p nc = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileName);
    double t0 = 1179309600.;
    {
        std::shared_ptr<CDMProcessor> proc(new CDMProcessor(nc));
        proc->deAccumulate("time");

        DataPtr data = proc->getData("time");
        boost::shared_array<double> time = data->asDouble();
        TEST4FIMEX_CHECK_CLOSE(time[0], t0, 1e-5); // unchanged
        TEST4FIMEX_CHECK_CLOSE(time[1], 3600., 1e-5);
        TEST4FIMEX_CHECK_CLOSE(time[2], 3600., 1e-5);
        TEST4FIMEX_CHECK_CLOSE(time[3], 3600., 1e-5);
    }
    {
        std::shared_ptr<CDMProcessor> proc(new CDMProcessor(nc));
        proc->accumulate("time");
        DataPtr data = proc->getData("time");
        boost::shared_array<double> time = data->asDouble();
        TEST4FIMEX_CHECK_CLOSE(time[0], t0, 1e-5); // unchanged
        TEST4FIMEX_CHECK_CLOSE(time[1], time[0] + t0 + 3600., 1e-5);
        TEST4FIMEX_CHECK_CLOSE(time[2], time[1] + t0 + 2 * 3600., 1e-5);
        TEST4FIMEX_CHECK_CLOSE(time[3], time[2] + t0 + 3 * 3600., 1e-5);
    }
}

TEST4FIMEX_TEST_CASE(test_rotate)
{
    //    defaultLogLevel(Logger::DEBUG);
        const string fileName = pathTest("coordTest.nc");

        CDMReader_p nc = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileName);
        std::shared_ptr<CDMProcessor> proc(new CDMProcessor(nc));
        proc->rotateAllVectorsToLatLon(true);
        const CDMAttribute& attrx = proc->getCDM().getAttribute("x_wind_10m", "standard_name");
        TEST4FIMEX_CHECK_EQ(attrx.getStringValue(), "LATLON_ROTATED_x_wind");
        const CDMAttribute& attry = proc->getCDM().getAttribute("y_wind_10m", "standard_name");
        TEST4FIMEX_CHECK_EQ(attry.getStringValue(), "LATLON_ROTATED_y_wind");

        float xn = proc->getDataSlice("x_wind_10m", 0)->asFloat()[3];
        float yn = proc->getDataSlice("y_wind_10m", 0)->asFloat()[3];

        float xo = nc->getDataSlice("x_wind_10m", 0)->asFloat()[3];
        float yo = nc->getDataSlice("y_wind_10m", 0)->asFloat()[3];

        TEST4FIMEX_CHECK_NE(xn, xo);
        TEST4FIMEX_CHECK_NE(yn, yo);
        TEST4FIMEX_CHECK_CLOSE(xn * xn + yn * yn, xo * xo + yo * yo, 1e-4);
}

TEST4FIMEX_TEST_CASE(interpolator_vectorlatlon)
{
    if (!hasTestExtra())
        return;
    const string flth00_dat = pathTestExtra("flth00.dat");
    CDMReader_p feltReader = CDMFileReaderFactory::create(MIFI_FILETYPE_FELT, flth00_dat, pathShareEtc("felt2nc_variables.xml"));
    std::shared_ptr<CDMProcessor> processor = std::make_shared<CDMProcessor>(feltReader);
    vector<string> x(1, "x_wind_10m");
    vector<string> y(1, "y_wind_10m");
    processor->rotateVectorToLatLon(true, x, y);
    SliceBuilder sbX0(feltReader->getCDM(), x[0]);
    SliceBuilder sbY0(feltReader->getCDM(), y[0]);
    {
        // 0deg longitude
        sbX0.setStartAndSize("x", 114, 1);
        sbX0.setStartAndSize("y", 85, 1);
        sbY0.setStartAndSize("x", 114, 1);
        sbY0.setStartAndSize("y", 85, 1);
        DataPtr xDataOrg = feltReader->getScaledDataSlice(x[0], sbX0);
        DataPtr xDataRot = processor->getScaledDataSlice(x[0], sbX0);
        DataPtr yDataOrg = feltReader->getScaledDataSlice(y[0], sbY0);
        DataPtr yDataRot = processor->getScaledDataSlice(y[0], sbY0);
        for (size_t i = 0; i < xDataOrg->size(); i++) {
            // no change in x
            TEST4FIMEX_CHECK_CLOSE((xDataOrg->asFloat())[i], (xDataRot->asFloat())[i], 1e-2);
            TEST4FIMEX_CHECK_CLOSE((yDataOrg->asFloat())[i], (yDataRot->asFloat())[i], 1e-2);
        }
    }
    {
        // 90deg longitude
        sbX0.setStartAndSize("x", 182, 1);
        sbX0.setStartAndSize("y", 147, 1);
        sbY0.setStartAndSize("x", 182, 1);
        sbY0.setStartAndSize("y", 147, 1);
        DataPtr xDataOrg = feltReader->getScaledDataSlice(x[0], sbX0);
        DataPtr xDataRot = processor->getScaledDataSlice(x[0], sbX0);
        DataPtr yDataOrg = feltReader->getScaledDataSlice(y[0], sbY0);
        DataPtr yDataRot = processor->getScaledDataSlice(y[0], sbY0);
        for (size_t i = 0; i < xDataOrg->size(); i++) {
            TEST4FIMEX_CHECK_CLOSE((xDataOrg->asFloat())[i], -1. * (yDataRot->asFloat())[i], 1e-1);
            TEST4FIMEX_CHECK_CLOSE((yDataOrg->asFloat())[i], (xDataRot->asFloat())[i], 1e-1);
        }
    }
    {
        // ~0deg longitude (no org data at 180)
        sbX0.setStartAndSize("x", 113, 1);
        sbX0.setStartAndSize("y", 10, 1);
        sbY0.setStartAndSize("x", 113, 1);
        sbY0.setStartAndSize("y", 10, 1);
        DataPtr xDataOrg = feltReader->getScaledDataSlice(x[0], sbX0);
        DataPtr xDataRot = processor->getScaledDataSlice(x[0], sbX0);
        DataPtr yDataOrg = feltReader->getScaledDataSlice(y[0], sbY0);
        DataPtr yDataRot = processor->getScaledDataSlice(y[0], sbY0);
        for (size_t i = 0; i < xDataOrg->size(); i++) {
            float error = ((xDataOrg->asFloat())[i] < 1) ? 50 : 3;
            TEST4FIMEX_CHECK_CLOSE((xDataOrg->asFloat())[i], (xDataRot->asFloat())[i], error);
            error = ((yDataOrg->asFloat())[i] < 1) ? 50 : 3;
            TEST4FIMEX_CHECK_CLOSE((yDataOrg->asFloat())[i], (yDataRot->asFloat())[i], error);
        }
    }
    {
        // -90deg longitude
        sbX0.setStartAndSize("x", 38, 1);
        sbX0.setStartAndSize("y", 147, 1);
        sbY0.setStartAndSize("x", 38, 1);
        sbY0.setStartAndSize("y", 147, 1);
        DataPtr xDataOrg = feltReader->getScaledDataSlice(x[0], sbX0);
        DataPtr xDataRot = processor->getScaledDataSlice(x[0], sbX0);
        DataPtr yDataOrg = feltReader->getScaledDataSlice(y[0], sbY0);
        DataPtr yDataRot = processor->getScaledDataSlice(y[0], sbY0);
        for (size_t i = 0; i < xDataOrg->size(); i++) {
            float error = ((xDataOrg->asFloat())[i] < 1) ? 1 : .1;
            TEST4FIMEX_CHECK_CLOSE((xDataOrg->asFloat())[i], (yDataRot->asFloat())[i], error);
            error = ((yDataOrg->asFloat())[i] < 1) ? 1 : .1;
            TEST4FIMEX_CHECK_CLOSE((yDataOrg->asFloat())[i], -1 * (xDataRot->asFloat())[i], error);
        }
    }
}
