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

#include "fimex/TimeUnit.h"
#include "fimex/Units.h"
#include "fimex/UnitsException.h"
#include "fimex/Utils.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <cmath>

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_Units )
{
    double slope, offset;
    Units units;
    units.convert("km", "m", slope, offset);
    BOOST_CHECK(slope == 1000);
    BOOST_CHECK(offset == 0);

    units.convert("K", "Celsius", slope, offset);
    BOOST_CHECK_CLOSE(slope, 1, 1e6);
    BOOST_CHECK_CLOSE(offset, 273.15, 1e6);

    boost::shared_ptr<UnitsConverter> conv = units.getConverter("K", "Celsius");
    BOOST_CHECK(conv->isLinear());
    BOOST_CHECK_CLOSE(conv->convert(273.15), 0, 1e6);
}

BOOST_AUTO_TEST_CASE( test_UnitsError )
{
    double slope, offset;
    Units units;
    try {
        units.convert("km", "s", slope, offset);
        BOOST_CHECK(false);
    } catch (UnitException& ex) {
        BOOST_CHECK(true);
    }
}

BOOST_AUTO_TEST_CASE( test_UnitsConvertible )
{
    Units units;
    BOOST_CHECK(!units.areConvertible("km", "s"));
    BOOST_CHECK(units.areConvertible("hours since 2000-01-01 19:30:00", "seconds since 1970-01-01"));
}

BOOST_AUTO_TEST_CASE( test_UnitsTime )
{
    Units units;
    BOOST_CHECK(!units.isTime("km"));
    BOOST_CHECK(units.isTime("hours since 2000-01-01 19:30:00"));
}

BOOST_AUTO_TEST_CASE( test_LogUnit )
{
    Units units;
    BOOST_CHECK(units.areConvertible("hPa", "ln(re 1Pa)"));
    boost::shared_ptr<UnitsConverter> conv = units.getConverter("hPa", "ln(re 1Pa)");
    BOOST_CHECK(!conv->isLinear());
    BOOST_CHECK_CLOSE(conv->convert(1000.), 11.512925, 1e-5);
}

BOOST_AUTO_TEST_CASE( test_TimeUnit )
{
    TimeUnit tu("seconds since 1970-01-01 01:00:00");
    BOOST_CHECK(true);
    double epoch = tu.unitTime2epochSeconds(0);
    BOOST_CHECK(fabs(epoch-3600) < 1e-5);
    BOOST_CHECK(fabs(tu.epochSeconds2unitTime(epoch)) < 1e-5);

    FimexTime ft = tu.unitTime2fimexTime(3600*24*33); // 03.02.1970
    BOOST_CHECK(ft.getYear() == 1970);
    BOOST_CHECK(ft.getMonth() == 2);
    BOOST_CHECK(ft.getMDay() == 3);
    BOOST_CHECK(ft.getHour() == 1);
    BOOST_CHECK(ft.getMinute() == 0);
    BOOST_CHECK(ft.getSecond() == 0);

    ft.setMSecond(11);
    // std::cerr << type2string(ft) << std::endl;
    BOOST_CHECK(type2string(ft) == "1970-02-03 01:00:00.011");
    // std::cerr << type2string(string2FimexTime(type2string(ft))) << std::endl;
    BOOST_CHECK(type2string(string2FimexTime(type2string(ft))) == "1970-02-03 01:00:00.011");

    // check comparison
    FimexTime ft2 = string2FimexTime(type2string(ft));
    BOOST_CHECK(ft2 == ft);
    ft2.setMSecond(0);
    BOOST_CHECK(ft2 != ft);
    BOOST_CHECK(ft2 < ft);
    BOOST_CHECK(ft2 <= ft);
    BOOST_CHECK(ft > ft2);
    BOOST_CHECK(ft >= ft2);



    ft.setMonth(1);
    ft.setMDay(1);
    ft.setMSecond(0);
    BOOST_CHECK(fabs(tu.fimexTime2unitTime(ft)) < 1e-5);

    boost::posix_time::ptime ptime(boost::gregorian::date(1970,1,1), boost::posix_time::time_duration(2,0,0));
    double unitTime = tu.posixTime2unitTime(ptime);
    BOOST_CHECK(abs(unitTime-(60*60)) < 1e-5);

    FimexTime minTime(FimexTime::min_date_time);
    FimexTime maxTime(FimexTime::max_date_time);
    BOOST_CHECK(ft > minTime);
    BOOST_CHECK(ft2 > minTime);
    BOOST_CHECK(ft < maxTime);
    BOOST_CHECK(ft2 < maxTime);
}

#endif // HAVE_BOOST_UNIT_TEST_FRAMEWORK
