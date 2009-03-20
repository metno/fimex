/*
 * Fimex, testTimeSpec.cc
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
 *  Created on: Mar 18, 2009
 *      Author: Heiko Klein
 */

#include "fimex/config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>
#include "fimex/TimeSpec.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_TimeSpec )
{
	string tspec("2000-01-01 00:00:00,2000-01-01 04:00:00,...,2000-01-02 08:00:00");
	FimexTime start;
	FimexTime end;
	TimeSpec ts(tspec, start, end); // start and end don't play a role for absolute times
	const vector<FimexTime>& fTimes = ts.getTimeSteps();
	//cerr << fTimes.size() << endl;
	BOOST_CHECK(fTimes.size() == 9);
	//for (size_t i = 0; i < fTimes.size(); i++)
	//	cerr << "time["<<i<<"]: " << fTimes[i] << endl;
	BOOST_CHECK(fTimes[2].hour == 8);
	BOOST_CHECK(fTimes[8].hour == 8);
	BOOST_CHECK(fTimes[7].mday == 2);

	start.year = 2001;
	start.month = 1;
	start.mday = 2;
	start.hour = 0;
	start.minute = 0;
	start.second = 0;
	start.msecond = 0;
	end = start;
	end.mday = 3;
	end.hour = 8;
	string tspec2("0,3,...,x;relativeUnit=hours since 2000-01-01 00:00:00");
	TimeSpec ts2(tspec2, start, end);
	const vector<FimexTime>& fTimes2 = ts2.getTimeSteps();
	//cerr << fTimes2.size() << endl;
	BOOST_CHECK(fTimes2.size() == 11);
	//for (size_t i = 0; i < fTimes2.size(); i++)
	//	cerr << "time["<<i<<"]: " << fTimes2[i] << endl;
	BOOST_CHECK(fTimes2[0].hour == 0);
	BOOST_CHECK(fTimes2[0].year == 2001);
	BOOST_CHECK(fTimes2[10].mday == 3);
	BOOST_CHECK(fTimes2[10].hour == 6);

}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif

