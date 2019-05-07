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

#include "testinghelpers.h"
#include "fimex/TimeSpec.h"
#include "fimex/Logger.h"

using namespace std;
using namespace MetNoFimex;
//#define TEST_DEBUG

TEST4FIMEX_TEST_CASE(test_TimeSpec)
{
#ifdef TEST_DEBUG
    defaultLogLevel(Logger::DEBUG);
#endif
	string tspec("2000-01-01 00:00:00,2000-01-01 04:00:00,...,2000-01-02 08:00:00");
	FimexTime start;
	FimexTime end;
	TimeSpec ts(tspec, start, end); // start and end don't play a role for absolute times
	const vector<FimexTime>& fTimes = ts.getTimeSteps();
        TEST4FIMEX_REQUIRE_EQ(fTimes.size(), 9);
        TEST4FIMEX_CHECK_EQ(fTimes[2].getHour(), 8);
        TEST4FIMEX_CHECK_EQ(fTimes[8].getHour(), 8);
        TEST4FIMEX_CHECK_EQ(fTimes[7].getMDay(), 2);

        start.setYear(2001);
	start.setMonth(1);
	start.setMDay(2);
	start.setHour(0);
	start.setMinute(0);
	start.setSecond(0);
	start.setMSecond(0);
	end = start;
	end.setMDay(3);
	end.setHour(8);

	string tspec2("0,3,...,x,x+3;relativeUnit=hours since 2000-01-01 00:00:00");
	TimeSpec ts2(tspec2, start, end);
	const vector<FimexTime>& fTimes2 = ts2.getTimeSteps();
        TEST4FIMEX_REQUIRE_EQ(fTimes2.size(), 12);
        TEST4FIMEX_CHECK_EQ(fTimes2[0].getHour(), 0);
        TEST4FIMEX_CHECK_EQ(fTimes2[0].getYear(), 2001);
        TEST4FIMEX_CHECK_EQ(fTimes2[10].getMDay(), 3);
        TEST4FIMEX_CHECK_EQ(fTimes2[10].getHour(), 6);
        TEST4FIMEX_CHECK_EQ(fTimes2[11].getMDay(), 3);
        TEST4FIMEX_CHECK_EQ(fTimes2[11].getHour(), 9);
}
