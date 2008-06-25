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

using namespace std;
using namespace MetNoFimex;

void test_Units() {
	double slope, offset;
	Units units;
#if HAVE_UDUNITS
	units.convert("km", "m", &slope, &offset);
	BOOST_CHECK(slope == 1000);
	BOOST_CHECK(offset == 0);
#else
	BOOST_CHECK(true);
#endif
}

void test_UnitsError() {
	double slope, offset;
	Units units;
	try {
		units.convert("km", "s", &slope, &offset);
		BOOST_CHECK(false);
	} catch (UnitException& ex) {
		BOOST_CHECK(true);
	}
}

void test_UnitsConvertible() {
	Units units;
#if HAVE_UDUNITS
	BOOST_CHECK(!units.areConvertible("km", "s"));
	BOOST_CHECK(units.areConvertible("hours since 2000-01-01 19:30:00", "seconds since 1970-01-01"));
#else
	BOOST_CHECK(true);
#endif	
}

void test_UnitsTime() {
	Units units;
#if HAVE_UDUNITS
	BOOST_CHECK(!units.isTime("km"));
	BOOST_CHECK(units.isTime("hours since 2000-01-01 19:30:00"));
#else
	BOOST_CHECK(true);
#endif
}


test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Master test suite" );
    
	test->add( BOOST_TEST_CASE( &test_Units ) );
	test->add( BOOST_TEST_CASE( &test_UnitsError ) );
	test->add( BOOST_TEST_CASE( &test_UnitsConvertible ) );
	test->add( BOOST_TEST_CASE( &test_UnitsTime ) );
    return test;
}
#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
