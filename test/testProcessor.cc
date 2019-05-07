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
