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

#include "GridDataFactory.h"
#include <wdb/WdbIndex.h>
#include <wdb/gridInformation/GridInformation.h>
#include <fimex/CDMException.h>

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(WdbIndexTest)

using namespace MetNoFimex::wdb;
using MetNoFimex::CDMException;

class WdbIndexTestFixture : public GridDataFactory {};


BOOST_FIXTURE_TEST_CASE(simpleCase, WdbIndexTestFixture)
{
	add("2011-04-06 06:00:00");
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 0);

	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(0, gids.front());
}

BOOST_FIXTURE_TEST_CASE(requestFirstTimeEntry, WdbIndexTestFixture)
{
	add("2011-04-06 06:00:00");
	add("2011-04-06 07:00:00");
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 0);

	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(0, gids.front());
}

BOOST_FIXTURE_TEST_CASE(requestSecondTimeEntry, WdbIndexTestFixture)
{
	add("2011-04-06 06:00:00");
	add("2011-04-06 07:00:00");
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 1);

	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(1, gids.front());
}

BOOST_FIXTURE_TEST_CASE(throwOnSameData, WdbIndexTestFixture)
{
	add("2011-04-06 06:00:00");
	add("2011-04-06 06:00:00");
	BOOST_CHECK_THROW(WdbIndex index(gridData()), CDMException);
}


BOOST_FIXTURE_TEST_CASE(throwsOnInvalidTimeIndex, WdbIndexTestFixture)
{
	add("2011-04-06 06:00:00");
	WdbIndex index(gridData());
	BOOST_CHECK_THROW(index.getData(defaultParameter.name(), 1), CDMException);
}

BOOST_FIXTURE_TEST_CASE(throwOnRequestForNonexistingParameter, WdbIndexTestFixture)
{
	add(Parameter("pressure", "hp"));
	WdbIndex index(gridData());

	BOOST_CHECK_THROW(index.getData("no such parameter", 0), CDMException);
}

BOOST_FIXTURE_TEST_CASE(selectSingleParameter, WdbIndexTestFixture)
{
	add(Parameter("temperature", "c"));
	add(Parameter("pressure", "hp"));
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData("temperature", 0);
	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(0, gids.front());

	gids = index.getData("pressure", 0);
	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(1, gids.front());
}

BOOST_FIXTURE_TEST_CASE(selectParameterWithMissingTimeEntry, WdbIndexTestFixture)
{
	add(Parameter("temperature", "c"), "2011-04-06 06:00:00");
	add(Parameter("temperature", "c"), "2011-04-06 07:00:00");
	add(Parameter("temperature", "c"), "2011-04-06 08:00:00");
	add(Parameter("pressure", "hp"), "2011-04-06 06:00:00");
	//add(Parameter("pressure", "hp"), "2011-04-06 07:00:00");
	add(Parameter("pressure", "hp"), "2011-04-06 08:00:00");
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData("pressure", 1);
	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids.front());
}

BOOST_FIXTURE_TEST_CASE(parameterWithSingleTime, WdbIndexTestFixture)
{
	add(Parameter("temperature", "c"), "2011-04-06 06:00:00");
	add(Parameter("temperature", "c"), "2011-04-06 07:00:00");
	add(Parameter("temperature", "c"), "2011-04-06 08:00:00");
	add(Parameter("terrain height", "m"), "2011-04-06 07:00:00");
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData("terrain height", 0);
	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(3, gids.front());
}

BOOST_FIXTURE_TEST_CASE(parameterWithUniqueTime, WdbIndexTestFixture)
{
	// do we want this?

//	add(Parameter("temperature", "c"), "2011-04-06 06:00:00");
//	add(Parameter("temperature", "c"), "2011-04-06 07:00:00");
//	add(Parameter("temperature", "c"), "2011-04-06 08:00:00");
//	add(Parameter("terrain height", "m"), "1900-01-01 00:00:00");
//	WdbIndex index(gridData());
//
//	WdbIndex::GidList gids = index.getData("temperature", 0);
//	BOOST_REQUIRE_EQUAL(1, gids.size());
//	BOOST_CHECK_EQUAL(0, gids.front());
}


BOOST_FIXTURE_TEST_CASE(getLevels, WdbIndexTestFixture)
{
	add(Level("height", "m", 0, 0));
	add(Level("height", "m", 1, 1));
	add(Level("height", "m", 2, 2));
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 0);

	BOOST_REQUIRE_EQUAL(3, gids.size());
	BOOST_CHECK_EQUAL(0, gids[0]);
	BOOST_CHECK_EQUAL(1, gids[1]);
	BOOST_CHECK_EQUAL(2, gids[2]);
}

BOOST_FIXTURE_TEST_CASE(twoLevelsComingInWrongOrder, WdbIndexTestFixture)
{
	add(Level("height", "m", 1, 1));
	add(Level("height", "m", 0, 0));
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 0);

	BOOST_REQUIRE_EQUAL(2, gids.size());
	BOOST_CHECK_EQUAL(1, gids[0]);
	BOOST_CHECK_EQUAL(0, gids[1]);
}


BOOST_FIXTURE_TEST_CASE(getMissingLevels, WdbIndexTestFixture)
{
	add(Level("height", "m", 0, 0), "2011-04-06 06:00:00");
	//add(Level("height", "m", 1, 1), "2011-04-06 06:00:00");
	add(Level("height", "m", 2, 2), "2011-04-06 06:00:00");
	add(Level("height", "m", 0, 0), "2011-04-06 07:00:00");
	add(Level("height", "m", 1, 1), "2011-04-06 07:00:00");
	add(Level("height", "m", 2, 2), "2011-04-06 07:00:00");
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 0);

	BOOST_REQUIRE_EQUAL(3, gids.size());
	BOOST_CHECK_EQUAL(0, gids[0]);
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[1]);
	BOOST_CHECK_EQUAL(1, gids[2]);
}

BOOST_FIXTURE_TEST_CASE(getMissingLevelsForMissingTimes, WdbIndexTestFixture)
{
	add(Parameter("pressure", "pa"), "2011-04-06 07:00:00");
	add(Parameter("pressure", "pa"), "2011-04-06 06:00:00");
	add(Parameter("pressure", "pa"), "2011-04-06 08:00:00");

	add(Level("height", "m", 0, 0), "2011-04-06 07:00:00");
	add(Level("height", "m", 1, 1), "2011-04-06 07:00:00");
	add(Level("height", "m", 2, 2), "2011-04-06 07:00:00");
	add(Level("height", "m", 0, 0), "2011-04-06 08:00:00");
	add(Level("height", "m", 1, 1), "2011-04-06 08:00:00");
	add(Level("height", "m", 2, 2), "2011-04-06 08:00:00");
	// But not 2011-04-06 06:00:00


	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 0);

	BOOST_REQUIRE_EQUAL(3, gids.size());
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[0]);
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[1]);
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[2]);
}

BOOST_FIXTURE_TEST_CASE(separatesSeveralTypesOfLevels, WdbIndexTestFixture)
{
	add(Parameter("pressure", "pa"),   Level("height", "m", 0, 0));
	add(Parameter("temperature", "C"), Level("whatever", "m", 1, 1));
	add(Parameter("temperature", "C"), Level("whatever", "m", 0, 0));
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData("pressure", 0);
	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(0, gids.front());
}


BOOST_FIXTURE_TEST_CASE(throwOnManyLevelTypesForSameParameterInData, WdbIndexTestFixture)
{
	add(Level("height", "m", 0, 0));
	add(Level("something", "x", 0, 0));

	BOOST_CHECK_THROW(WdbIndex index(gridData()), CDMException);
}

BOOST_FIXTURE_TEST_CASE(onlyOneLevelWhenAllEntriesHaveOneLevel, WdbIndexTestFixture)
{
	add(Parameter("temeperature", "c"), Level("height", "m", 0, 0));
	add(Parameter("temeperature", "c"), Level("height", "m", 2, 2));
	add(Parameter("temeperature", "c"), Level("height", "m", 10, 10));
	add(Parameter("wind speed", "m/s"), Level("height", "m", 10, 10));
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData("wind speed", 0);

	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(3, gids.front());
}

BOOST_FIXTURE_TEST_CASE(onlyOneMissingLevelWhenAllEntriesHaveOneLevel, WdbIndexTestFixture)
{
	add(Parameter("temeperature", "c"), Level("height", "m", 2, 2), "2011-04-01 06:00:00");
	add(Parameter("temeperature", "c"), Level("height", "m", 10, 10), "2011-04-01 06:00:00");
	add(Parameter("temeperature", "c"), Level("height", "m", 2, 2), "2011-04-02 06:00:00");
	add(Parameter("temeperature", "c"), Level("height", "m", 10, 10), "2011-04-02 06:00:00");
	add(Parameter("temeperature", "c"), Level("height", "m", 2, 2), "2011-04-03 06:00:00");
	add(Parameter("temeperature", "c"), Level("height", "m", 10, 10), "2011-04-03 06:00:00");
	add(Parameter("wind speed", "m/s"), Level("height", "m", 10, 10), "2011-04-02 06:00:00");
	add(Parameter("wind speed", "m/s"), Level("height", "m", 10, 10), "2011-04-03 06:00:00");
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData("wind speed", 0);

	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids.front());
}

BOOST_FIXTURE_TEST_CASE(multipleVersions, WdbIndexTestFixture)
{
	add(0);
	add(1);
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 0);
	BOOST_REQUIRE_EQUAL(2, gids.size());
	BOOST_CHECK_EQUAL(0, gids[0]);
	BOOST_CHECK_EQUAL(1, gids[1]);
}

BOOST_FIXTURE_TEST_CASE(missingVersions, WdbIndexTestFixture)
{
	add(0, "2011-04-07 06:00:00");
	add(1, "2011-04-07 06:00:00");
	add(0, "2011-04-07 07:00:00");
	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 1);
	BOOST_REQUIRE_EQUAL(2, gids.size());
	BOOST_CHECK_EQUAL(2, gids[0]);
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[1]);
}

BOOST_FIXTURE_TEST_CASE(missingTimeStepWithManyVersions, WdbIndexTestFixture)
{
	add(Parameter("temperature", "C"), "2011-04-07 06:00:00");
	add(Parameter("temperature", "C"), "2011-04-07 07:00:00");
	add(Parameter("temperature", "C"), "2011-04-07 08:00:00");
	add(Parameter("pressure", "pa"), "2011-04-07 06:00:00", 0);
	add(Parameter("pressure", "pa"), "2011-04-07 06:00:00", 1);
	add(Parameter("pressure", "pa"), "2011-04-07 07:00:00", 0);

	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData("pressure", 2);
	BOOST_REQUIRE_EQUAL(2, gids.size());
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[0]);
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[1]);
}


BOOST_FIXTURE_TEST_CASE(manyReferenceTimes, WdbIndexTestFixture)
{
	add("2011-04-06 06:00:00", "2011-04-06 06:00:00");
	add("2011-04-07 06:00:00", "2011-04-07 06:00:00");

	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 0);
	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(0, gids[0]);

	gids = index.getData(defaultParameter.name(), 1);
	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(1, gids[0]);
}

BOOST_FIXTURE_TEST_CASE(manyReferenceTimesAndValidTimes, WdbIndexTestFixture)
{
	add("2011-04-06 06:00:00", "2011-04-06 06:00:00");
	add("2011-04-07 06:00:00", "2011-04-06 06:00:00");
	add("2011-04-07 06:00:00", "2011-04-07 06:00:00");
	add("2011-04-08 06:00:00", "2011-04-07 06:00:00");

	WdbIndex index(gridData());

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 0);
	BOOST_REQUIRE_EQUAL(2, gids.size());
	BOOST_CHECK_EQUAL(0, gids[0]);
	BOOST_CHECK_EQUAL(1, gids[1]);

	gids = index.getData(defaultParameter.name(), 1);
	BOOST_REQUIRE_EQUAL(2, gids.size());
	BOOST_CHECK_EQUAL(2, gids[0]);
	BOOST_CHECK_EQUAL(3, gids[1]);
}


BOOST_AUTO_TEST_SUITE_END()

#endif
