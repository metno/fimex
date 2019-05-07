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
#include "fimex/TimeUnit.h"
#include "fimex/Units.h"
#include "fimex/UnitsException.h"
#include "fimex/Utils.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <cmath>

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_Units)
{
    double slope, offset;
    Units units;
    units.convert("km", "m", slope, offset);
    TEST4FIMEX_CHECK_EQ(slope, 1000);
    TEST4FIMEX_CHECK_EQ(offset, 0);

    units.convert("K", "Celsius", slope, offset);
    TEST4FIMEX_CHECK_CLOSE(slope, 1, 1e6);
    TEST4FIMEX_CHECK_CLOSE(offset, 273.15, 1e6);

    UnitsConverter_p conv = units.getConverter("K", "Celsius");
    TEST4FIMEX_CHECK(conv->isLinear());
    TEST4FIMEX_CHECK_CLOSE(conv->convert(273.15), 0, 1e6);
}

TEST4FIMEX_TEST_CASE(test_UnitsError)
{
    double slope, offset;
    Units units;
    TEST4FIMEX_CHECK_THROW(units.convert("km", "s", slope, offset), UnitException);
}

TEST4FIMEX_TEST_CASE(test_UnitsConvertible)
{
    Units units;
    TEST4FIMEX_CHECK(!units.areConvertible("km", "s"));
    TEST4FIMEX_CHECK(units.areConvertible("hours since 2000-01-01 19:30:00", "seconds since 1970-01-01"));
}

TEST4FIMEX_TEST_CASE(test_UnitsTime)
{
    Units units;
    TEST4FIMEX_CHECK(!units.isTime("km"));
    TEST4FIMEX_CHECK(units.isTime("hours since 2000-01-01 19:30:00"));
}

TEST4FIMEX_TEST_CASE(test_LogUnit)
{
    Units units;
    TEST4FIMEX_CHECK(units.areConvertible("hPa", "ln(re 1Pa)"));
    UnitsConverter_p conv = units.getConverter("hPa", "ln(re 1Pa)");
    TEST4FIMEX_CHECK(!conv->isLinear());
    TEST4FIMEX_CHECK_CLOSE(conv->convert(1000.), 11.512925, 1e-5);
}

TEST4FIMEX_TEST_CASE(test_TimeUnit)
{
    TimeUnit tu("seconds since 1970-01-01 01:00:00");
    double epoch = tu.unitTime2epochSeconds(0);
    TEST4FIMEX_CHECK(fabs(epoch - 3600) < 1e-5);
    TEST4FIMEX_CHECK(fabs(tu.epochSeconds2unitTime(epoch)) < 1e-5);

    FimexTime ft = tu.unitTime2fimexTime(3600*24*33); // 03.02.1970
    TEST4FIMEX_CHECK_EQ(ft.getYear(), 1970);
    TEST4FIMEX_CHECK_EQ(ft.getMonth(), 2);
    TEST4FIMEX_CHECK_EQ(ft.getMDay(), 3);
    TEST4FIMEX_CHECK_EQ(ft.getHour(), 1);
    TEST4FIMEX_CHECK_EQ(ft.getMinute(), 0);
    TEST4FIMEX_CHECK_EQ(ft.getSecond(), 0);

    ft.setMSecond(11);
    // std::cerr << type2string(ft) << std::endl;
    TEST4FIMEX_CHECK_EQ(type2string(ft), "1970-02-03 01:00:00.011");
    // std::cerr << type2string(string2FimexTime(type2string(ft))) << std::endl;
    TEST4FIMEX_CHECK_EQ(type2string(string2FimexTime(type2string(ft))), "1970-02-03 01:00:00.011");

    // check comparison
    FimexTime ft2 = string2FimexTime(type2string(ft));
    TEST4FIMEX_CHECK_EQ(ft2, ft);
    ft2.setMSecond(0);
    TEST4FIMEX_CHECK(ft2 != ft);
    TEST4FIMEX_CHECK(ft2 < ft);
    TEST4FIMEX_CHECK(ft2 <= ft);
    TEST4FIMEX_CHECK(ft > ft2);
    TEST4FIMEX_CHECK(ft >= ft2);

    ft.setMonth(1);
    ft.setMDay(1);
    ft.setMSecond(0);
    TEST4FIMEX_CHECK(fabs(tu.fimexTime2unitTime(ft)) < 1e-5);

    boost::posix_time::ptime ptime(boost::gregorian::date(1970,1,1), boost::posix_time::time_duration(2,0,0));
    double unitTime = tu.posixTime2unitTime(ptime);
    TEST4FIMEX_CHECK(abs(unitTime - (60 * 60)) < 1e-5);

    FimexTime minTime(FimexTime::min_date_time);
    FimexTime maxTime(FimexTime::max_date_time);
    TEST4FIMEX_CHECK(ft > minTime);
    TEST4FIMEX_CHECK(ft2 > minTime);
    TEST4FIMEX_CHECK(ft < maxTime);
    TEST4FIMEX_CHECK(ft2 < maxTime);
}
