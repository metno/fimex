/*
 * Fimex, timeInterpolator.cc
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
 *
 *  Created on: Jan 6, 2009
 *      Author: Heiko Klein
 */

#include "testinghelpers.h"

#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMTimeInterpolator.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_timeInterpolator)
{
    //defaultLogLevel(Logger::DEBUG);
    const string outputName = "test_timeInterpolator.nc";
    MetNoFimex::remove(outputName);

    if (!hasTestExtra())
        return;
    {
        const string fileName = pathTestExtra("flth00.dat");
        CDMReader_p feltReader = CDMFileReaderFactory::create("felt", fileName, pathShareEtc("felt2nc_variables.xml"));
        std::shared_ptr<CDMTimeInterpolator> timeInterpol(new CDMTimeInterpolator(feltReader));
        timeInterpol->changeTimeAxis("2007-05-16 10:00:00,2007-05-16 13:00:00,...,2007-05-16 22:00:00;unit=hours since 2007-05-16 00:00:00");
        DataPtr times = timeInterpol->getCDM().getVariable("time").getData();
        TEST4FIMEX_CHECK_EQ(times->size(), 5);
        shared_array<float> timeAry = times->asFloat();
        TEST4FIMEX_CHECK_EQ(timeAry[0], 10);
        TEST4FIMEX_CHECK_EQ(timeAry[4], 10 + 12);
        string airTemp = "air_temperature";
        TEST4FIMEX_CHECK_EQ(feltReader->getCDM().getVariable(airTemp).getName(), airTemp);
        TEST4FIMEX_CHECK_EQ(timeInterpol->getCDM().getVariable(airTemp).getName(), airTemp);
        MetNoFimex::createWriter(timeInterpol, "netcdf", outputName);
    }
    {
        // check that the correct data is written
        CDMReader_p ncReader = CDMFileReaderFactory::create("netcdf", outputName);
        DataPtr ncTimes = ncReader->getData("time");
        TEST4FIMEX_CHECK_EQ(ncTimes->size(), 5);
        shared_array<float> ncTimeAry = ncTimes->asFloat();
        TEST4FIMEX_CHECK_EQ(ncTimeAry[0], 10);
        TEST4FIMEX_CHECK_EQ(ncTimeAry[4], 10 + 12);
    }
}

TEST4FIMEX_TEST_CASE(test_timeInterpolatorRelative)
{
    const string outputName = "test_timeInterpolatorRelative.nc";
    MetNoFimex::remove(outputName);

    if (!hasTestExtra())
        return;
    {
        const string fileName = pathTestExtra("flth00.dat");
        CDMReader_p feltReader = CDMFileReaderFactory::create("felt", fileName, pathShareEtc("felt2nc_variables.xml"));
        std::shared_ptr<CDMTimeInterpolator> timeInterpol(new CDMTimeInterpolator(feltReader));
        timeInterpol->changeTimeAxis("0,3,...,x;relativeUnit=hours since 2001-01-01 10:00:00;unit=hours since 2007-05-16 00:00:00");
        DataPtr times = timeInterpol->getCDM().getVariable("time").getData();
        TEST4FIMEX_CHECK_EQ(times->size(), 21);
        shared_array<float> timeAry = times->asFloat();
        TEST4FIMEX_CHECK_EQ(timeAry[0], -2);
        TEST4FIMEX_CHECK_EQ(timeAry[4], 10);
        string airTemp = "air_temperature";
        TEST4FIMEX_CHECK_EQ(feltReader->getCDM().getVariable(airTemp).getName(), airTemp);
        TEST4FIMEX_CHECK_EQ(timeInterpol->getCDM().getVariable(airTemp).getName(), airTemp);
        MetNoFimex::createWriter(timeInterpol, "netcdf", outputName);
    }
    {
    // check that the correct data is written
    CDMReader_p ncReader = CDMFileReaderFactory::create("netcdf", outputName);
    DataPtr ncTimes = ncReader->getData("time");
    TEST4FIMEX_CHECK_EQ(ncTimes->size(), 21);
    shared_array<float> ncTimeAry = ncTimes->asFloat();
    TEST4FIMEX_CHECK_EQ(ncTimeAry[0], -2);
    TEST4FIMEX_CHECK_EQ(ncTimeAry[4], 10);
    }
}
