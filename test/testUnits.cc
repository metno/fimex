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
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;


#include "fimex/Units.h"
#include "fimex/TimeUnit.h"
#include <cmath>

using namespace std;
using namespace MetNoFimex;

#if HAVE_UDUNITS
void test_Units() {
	double slope, offset;
	Units units;
	units.convert("km", "m", slope, offset);
	BOOST_CHECK(slope == 1000);
	BOOST_CHECK(offset == 0);
}

void test_UnitsError() {
	double slope, offset;
	Units units;
	try {
		units.convert("km", "s", slope, offset);
		BOOST_CHECK(false);
	} catch (UnitException& ex) {
		BOOST_CHECK(true);
	}
}

void test_UnitsConvertible() {
	Units units;
	BOOST_CHECK(!units.areConvertible("km", "s"));
	BOOST_CHECK(units.areConvertible("hours since 2000-01-01 19:30:00", "seconds since 1970-01-01"));
}

void test_UnitsTime() {
	Units units;
	BOOST_CHECK(!units.isTime("km"));
	BOOST_CHECK(units.isTime("hours since 2000-01-01 19:30:00"));
}

void test_TimeUnit() {
	TimeUnit tu("seconds since 1970-01-01 01:00:00");
	BOOST_CHECK(true);
	double epoch = tu.unitTime2epochSeconds(0);
	BOOST_CHECK(fabs(epoch-3600) < 1e-5);
	BOOST_CHECK(fabs(tu.epochSeconds2unitTime(epoch) < 1e-5));

	FimexTime ft = tu.unitTime2fimexTime(3600*24*33); // 03.02.1970
	BOOST_CHECK(ft.year == 1970);
	BOOST_CHECK(ft.month == 2);
	BOOST_CHECK(ft.mday == 3);
	BOOST_CHECK(ft.hour == 1);
	BOOST_CHECK(ft.minute == 0);
	BOOST_CHECK(ft.second == 0);


	ft.month = 1;
	ft.mday = 1;
	BOOST_CHECK(fabs(tu.fimexTime2unitTime(ft)) < 1e-5);

}

#endif // UDUNITS

test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );

#if HAVE_UDUNITS
    test->add( BOOST_TEST_CASE( &test_Units ) );
	test->add( BOOST_TEST_CASE( &test_UnitsError ) );
	test->add( BOOST_TEST_CASE( &test_UnitsConvertible ) );
	test->add( BOOST_TEST_CASE( &test_UnitsTime ) );
	test->add( BOOST_TEST_CASE( &test_TimeUnit ) );
#endif
    return test;
}
#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
