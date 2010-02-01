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

#include "fimex/config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include "fimex/Utils.h"
#include "fimex/Units.h"
#include "fimex/TimeUnit.h"
#include <cmath>

using namespace std;
using namespace MetNoFimex;

#if HAVE_UDUNITS
BOOST_AUTO_TEST_CASE( test_Units )
{
	double slope, offset;
	Units units;
	units.convert("km", "m", slope, offset);
	BOOST_CHECK(slope == 1000);
	BOOST_CHECK(offset == 0);
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

BOOST_AUTO_TEST_CASE( test_TimeUnit )
{
	TimeUnit tu("seconds since 1970-01-01 01:00:00");
	BOOST_CHECK(true);
	double epoch = tu.unitTime2epochSeconds(0);
	BOOST_CHECK(fabs(epoch-3600) < 1e-5);
	BOOST_CHECK(fabs(tu.epochSeconds2unitTime(epoch)) < 1e-5);

	FimexTime ft = tu.unitTime2fimexTime(3600*24*33); // 03.02.1970
	BOOST_CHECK(ft.year == 1970);
	BOOST_CHECK(ft.month == 2);
	BOOST_CHECK(ft.mday == 3);
	BOOST_CHECK(ft.hour == 1);
	BOOST_CHECK(ft.minute == 0);
	BOOST_CHECK(ft.second == 0);

	ft.msecond = 11;
	// std::cerr << type2string(ft) << std::endl;
	BOOST_CHECK(type2string(ft) == "1970-02-03 01:00:00.011");
	// std::cerr << type2string(string2FimexTime(type2string(ft))) << std::endl;
	BOOST_CHECK(type2string(string2FimexTime(type2string(ft))) == "1970-02-03 01:00:00.011");

	// check comparison
	FimexTime ft2 = string2FimexTime(type2string(ft));
	BOOST_CHECK(ft2 == ft);
	ft2.msecond = 0;
	BOOST_CHECK(ft2 != ft);
	BOOST_CHECK(ft2 < ft);
	BOOST_CHECK(ft2 <= ft);
	BOOST_CHECK(ft > ft2);
	BOOST_CHECK(ft >= ft2);



	ft.month = 1;
	ft.mday = 1;
	ft.msecond = 0;
	BOOST_CHECK(fabs(tu.fimexTime2unitTime(ft)) < 1e-5);

}

#endif // UDUNITS

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
