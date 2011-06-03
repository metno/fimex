/*
    fimex

    Copyright (C) 2011 met.no

    Contact information:
    Norwegian Meteorological Institute
    Box 43 Blindern
    0313 OSLO
    NORWAY
    E-mail: post@met.no

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
    MA  02110-1301, USA
*/

#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#include <boost/test/unit_test.hpp>

#include "GridDataFactory.h"
#include <wdb/TimeHandler.h>
#include <wdb/WdbIndex.h>
#include <fimex/CDM.h>
#include <fimex/CDMException.h>
#include <fimex/Data.h>

#include <iostream>

BOOST_AUTO_TEST_SUITE(TimeHandlerTest)

using namespace MetNoFimex;
using namespace MetNoFimex::wdb;

class TimeHandlerFixture : public GridDataFactory
{
public:
	MetNoFimex::CDM cdm;
};

namespace
{
GridData::Time timeFromData(const boost::shared_ptr<Data> & data, int index = 0)
{
	double time = data->asConstDouble()[index];
	return boost::posix_time::from_time_t(time);
}
}

BOOST_FIXTURE_TEST_CASE(timeIndexLoopkup, TimeHandlerFixture)
{
	add("2011-03-31 06:00:00");
	add("2011-03-31 18:00:00");
	add("2011-03-31 12:00:00");
	add("2011-04-01 06:00:00");

	WdbIndex index(gridData());
	TimeHandler timeHandler(index);
	timeHandler.addToCdm(cdm);

	GridData::Time t = boost::posix_time::time_from_string("2011-03-31 06:00:00");
	boost::posix_time::time_duration offset(6, 0, 0);

	const CDMVariable & timeVar = cdm.getVariable("time");

	BOOST_CHECK_EQUAL(t,                timeFromData(timeHandler.getData(timeVar, 0)));
	BOOST_CHECK_EQUAL(t + offset,       timeFromData(timeHandler.getData(timeVar, 1)));
	BOOST_CHECK_EQUAL(t + (offset * 2), timeFromData(timeHandler.getData(timeVar, 2)));
	BOOST_CHECK_EQUAL(t + (offset * 4), timeFromData(timeHandler.getData(timeVar, 3)));
}

BOOST_FIXTURE_TEST_CASE(throwOnInvalidTimeIndexLoopkup, TimeHandlerFixture)
{
	add("2011-03-31 06:00:00");
	add("2011-03-31 18:00:00");

	WdbIndex index(gridData());
	TimeHandler timeHandler(index);
	timeHandler.addToCdm(cdm);

	const CDMVariable & timeVar = cdm.getVariable("time");
	timeHandler.getData(timeVar, 0);
	timeHandler.getData(timeVar, 1);
	BOOST_CHECK_THROW(timeHandler.getData(timeVar, 3), CDMException);
}

BOOST_FIXTURE_TEST_CASE(manyReferenceTimes, TimeHandlerFixture)
{
	add("2011-05-29 00:00:00", "2011-05-29 00:00:00");
	add("2011-05-30 00:00:00", "2011-05-30 00:00:00");

	WdbIndex index(gridData());
	TimeHandler timeHandler(index);
	timeHandler.addToCdm(cdm);

	GridData::Time t1 = boost::posix_time::time_from_string("2011-05-29 00:00:00");
	GridData::Time t2 = boost::posix_time::time_from_string("2011-05-30 00:00:00");

	const CDMVariable & timeVar = cdm.getVariable("forecast_reference_time");
	BOOST_CHECK_EQUAL(t1, timeFromData(timeHandler.getData(timeVar, 0)));
	BOOST_CHECK_EQUAL(t2, timeFromData(timeHandler.getData(timeVar, 1)));
	BOOST_CHECK_THROW(timeHandler.getData(timeVar, 2), CDMException);
}

BOOST_FIXTURE_TEST_CASE(manyReferenceTimesCreatesReferenceTimeDimension, TimeHandlerFixture)
{
	add("2011-05-29 00:00:00", "2011-05-29 00:00:00");
	add("2011-05-30 00:00:00", "2011-05-30 00:00:00");

	WdbIndex index(gridData());
	TimeHandler timeHandler(index);
	timeHandler.addToCdm(cdm);

	try
	{
		const CDMDimension & dim = cdm.getDimension("forecast_reference_time");
		BOOST_CHECK_EQUAL(2, dim.getLength());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}

}

BOOST_FIXTURE_TEST_CASE(manyReferenceTimesAndValidTimes, TimeHandlerFixture)
{
	add("2011-05-29 00:00:00", "2011-05-29 00:00:00");
	add("2011-05-30 00:00:00", "2011-05-29 00:00:00");
	add("2011-05-30 00:00:00", "2011-05-30 00:00:00");
	add("2011-05-31 00:00:00", "2011-05-30 00:00:00");

	WdbIndex index(gridData());
	TimeHandler timeHandler(index);
	timeHandler.addToCdm(cdm);

	GridData::Time referenceTime1 = boost::posix_time::time_from_string("2011-05-29 00:00:00");

	const CDMVariable & refTimeVar = cdm.getVariable("forecast_reference_time");
	BOOST_CHECK_EQUAL(referenceTime1, timeFromData(timeHandler.getData(refTimeVar, 0)));

	const CDMVariable & timeVar = cdm.getVariable("time");
	boost::shared_ptr<Data> times = timeHandler.getData(timeVar, 0);


	GridData::Time validTime1 = boost::posix_time::time_from_string("2011-05-29 00:00:00");
	GridData::Time validTime2 = boost::posix_time::time_from_string("2011-05-30 00:00:00");

	BOOST_REQUIRE_EQUAL(2, times->size());
	BOOST_CHECK_EQUAL(validTime1, timeFromData(times, 0));
	BOOST_CHECK_EQUAL(validTime2, timeFromData(times, 1));
}


BOOST_AUTO_TEST_SUITE_END()

#endif
