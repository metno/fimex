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


#include <wdb/WdbIndex.h>
#include <wdb/GridInformation.h>
#include <fimex/CDMException.h>

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(WdbIndexTest)

using namespace MetNoFimex::wdb;
using MetNoFimex::CDMException;

class WdbIndexTestFixture
{
	struct TestingGridData : public GridData
	{
		TestingGridData(const Parameter & param,
				const Level & lvl,
				int version,
				const Time & validTo,
				const GridData::GridInformationPtr & grid,
				gid gridId) :
			GridData(param, lvl, version, validTo, gridId)
		{
			setGridInformation(grid);
		}
	};

	GridData::gid nextGid_;
public:
	WdbIndexTestFixture() : nextGid_(0) {}

	GridData::gid nextGid()
	{
		return nextGid_ ++;
	}

	static boost::posix_time::ptime t(const std::string & time)
	{
		return boost::posix_time::time_from_string(time);
	}

	std::vector<GridData> gridData;

	static const Parameter defaultParameter;
	static const Level defaultLevel;
	static const std::string defaultTime;
	static const GridData::GridInformationPtr defaultGrid;

	void addGridData(const Parameter & parameter = defaultParameter, const std::string & time = defaultTime, int version = 0)
	{
		gridData.push_back(TestingGridData(parameter, defaultLevel, version, t(time), defaultGrid, nextGid()));
	}

	void addGridData(const Level & lvl, const std::string & time = defaultTime)
	{
		gridData.push_back(TestingGridData(defaultParameter, lvl, 0, t(time), defaultGrid, nextGid()));
	}

	void addGridData(const Parameter & parameter, const Level & lvl, std::string time = defaultTime)
	{
		gridData.push_back(TestingGridData(parameter, lvl, 0, t(time), defaultGrid, nextGid()));
	}

	void addGridData(const std::string & time)
	{
		gridData.push_back(TestingGridData(defaultParameter, defaultLevel, 0, t(time), defaultGrid, nextGid()));
	}

	void addGridData(int version, const std::string & time = defaultTime)
	{
		gridData.push_back(TestingGridData(defaultParameter, defaultLevel, version, t(time), defaultGrid, nextGid()));
	}
};
const Parameter WdbIndexTestFixture::defaultParameter("air temperature", "C");
const Level WdbIndexTestFixture::defaultLevel("distance above ground", "m", 0, 0);
const std::string WdbIndexTestFixture::defaultTime = "2011-03-18 06:00:00";
const GridData::GridInformationPtr WdbIndexTestFixture::defaultGrid(new GridInformation("+proj=longlat +a=6367470.0 +towgs84=0,0,0 +no_defs", 30, 20));





BOOST_FIXTURE_TEST_CASE(simpleCase, WdbIndexTestFixture)
{
	addGridData("2011-04-06 06:00:00");
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 0);

	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(0, gids.front());
}

BOOST_FIXTURE_TEST_CASE(requestFirstTimeEntry, WdbIndexTestFixture)
{
	addGridData("2011-04-06 06:00:00");
	addGridData("2011-04-06 07:00:00");
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 1);

	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(0, gids.front());
}

BOOST_FIXTURE_TEST_CASE(requestSecondTimeEntry, WdbIndexTestFixture)
{
	addGridData("2011-04-06 06:00:00");
	addGridData("2011-04-06 07:00:00");
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 2);

	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(1, gids.front());
}

BOOST_FIXTURE_TEST_CASE(throwsOnInvalidTimeIndex, WdbIndexTestFixture)
{
	addGridData("2011-04-06 06:00:00");
	WdbIndex index(gridData);
	BOOST_CHECK_THROW(index.getData(defaultParameter.name(), 2), CDMException);
}

BOOST_FIXTURE_TEST_CASE(throwOnRequestForNonexistingParameter, WdbIndexTestFixture)
{
	addGridData(Parameter("pressure", "hp"));
	WdbIndex index(gridData);

	BOOST_CHECK_THROW(index.getData("no such parameter", 1), CDMException);
}

BOOST_FIXTURE_TEST_CASE(selectSingleParameter, WdbIndexTestFixture)
{
	addGridData(Parameter("temperature", "c"));
	addGridData(Parameter("pressure", "hp"));
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData("temperature", 1);
	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(0, gids.front());

	gids = index.getData("pressure", 1);
	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(1, gids.front());
}

BOOST_FIXTURE_TEST_CASE(selectParameterWithMissingTimeEntry, WdbIndexTestFixture)
{
	addGridData(Parameter("temperature", "c"), "2011-04-06 06:00:00");
	addGridData(Parameter("temperature", "c"), "2011-04-06 07:00:00");
	addGridData(Parameter("temperature", "c"), "2011-04-06 08:00:00");
	addGridData(Parameter("pressure", "hp"), "2011-04-06 06:00:00");
	//addGridData(Parameter("pressure", "hp"), "2011-04-06 07:00:00");
	addGridData(Parameter("pressure", "hp"), "2011-04-06 08:00:00");
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData("pressure", 2);
	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids.front());
}

BOOST_FIXTURE_TEST_CASE(parameterWithSingleTime, WdbIndexTestFixture)
{
	addGridData(Parameter("temperature", "c"), "2011-04-06 06:00:00");
	addGridData(Parameter("temperature", "c"), "2011-04-06 07:00:00");
	addGridData(Parameter("temperature", "c"), "2011-04-06 08:00:00");
	addGridData(Parameter("terrain height", "m"), "2011-04-06 07:00:00");
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData("terrain height", 0);
	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(3, gids.front());
}

BOOST_FIXTURE_TEST_CASE(getLevels, WdbIndexTestFixture)
{
	addGridData(Level("height", "m", 0, 0));
	addGridData(Level("height", "m", 1, 1));
	addGridData(Level("height", "m", 2, 2));
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 1);

	BOOST_REQUIRE_EQUAL(3, gids.size());
	BOOST_CHECK_EQUAL(0, gids[0]);
	BOOST_CHECK_EQUAL(1, gids[1]);
	BOOST_CHECK_EQUAL(2, gids[2]);
}

BOOST_FIXTURE_TEST_CASE(getMissingLevels, WdbIndexTestFixture)
{
	addGridData(Level("height", "m", 0, 0), "2011-04-06 06:00:00");
	//addGridData(Level("height", "m", 1, 1), "2011-04-06 06:00:00");
	addGridData(Level("height", "m", 2, 2), "2011-04-06 06:00:00");
	addGridData(Level("height", "m", 0, 0), "2011-04-06 07:00:00");
	addGridData(Level("height", "m", 1, 1), "2011-04-06 07:00:00");
	addGridData(Level("height", "m", 2, 2), "2011-04-06 07:00:00");
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 1);

	BOOST_REQUIRE_EQUAL(3, gids.size());
	BOOST_CHECK_EQUAL(0, gids[0]);
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[1]);
	BOOST_CHECK_EQUAL(1, gids[2]);
}

BOOST_FIXTURE_TEST_CASE(getMissingLevelsForMissingTimes, WdbIndexTestFixture)
{
	addGridData(Parameter("pressure", "pa"), "2011-04-06 07:00:00");
	addGridData(Parameter("pressure", "pa"), "2011-04-06 06:00:00");
	addGridData(Parameter("pressure", "pa"), "2011-04-06 08:00:00");

	addGridData(Level("height", "m", 0, 0), "2011-04-06 07:00:00");
	addGridData(Level("height", "m", 1, 1), "2011-04-06 07:00:00");
	addGridData(Level("height", "m", 2, 2), "2011-04-06 07:00:00");
	addGridData(Level("height", "m", 0, 0), "2011-04-06 08:00:00");
	addGridData(Level("height", "m", 1, 1), "2011-04-06 08:00:00");
	addGridData(Level("height", "m", 2, 2), "2011-04-06 08:00:00");


	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 1);

	BOOST_REQUIRE_EQUAL(3, gids.size());
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[0]);
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[1]);
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[2]);
}

BOOST_FIXTURE_TEST_CASE(separatesSeveralTypesOfLevels, WdbIndexTestFixture)
{
	addGridData(Parameter("pressure", "pa"),   Level("height", "m", 0, 0));
	addGridData(Parameter("temperature", "C"), Level("whatever", "m", 1, 1));
	addGridData(Parameter("temperature", "C"), Level("whatever", "m", 0, 0));
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData("pressure", 1);
	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(0, gids.front());
}


BOOST_FIXTURE_TEST_CASE(throwOnManyLevelTypesForSameParameterInData, WdbIndexTestFixture)
{
	addGridData(Level("height", "m", 0, 0));
	addGridData(Level("something", "x", 0, 0));

	BOOST_CHECK_THROW(WdbIndex index(gridData), CDMException);
}

BOOST_FIXTURE_TEST_CASE(onlyOneLevelWhenAllEntriesHaveOneLevel, WdbIndexTestFixture)
{
	addGridData(Parameter("temeperature", "c"), Level("height", "m", 0, 0));
	addGridData(Parameter("temeperature", "c"), Level("height", "m", 2, 2));
	addGridData(Parameter("temeperature", "c"), Level("height", "m", 10, 10));
	addGridData(Parameter("wind speed", "m/s"), Level("height", "m", 10, 10));
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData("wind speed", 1);

	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(3, gids.front());
}

BOOST_FIXTURE_TEST_CASE(onlyOneMissingLevelWhenAllEntriesHaveOneLevel, WdbIndexTestFixture)
{
	addGridData(Parameter("temeperature", "c"), Level("height", "m", 2, 2), "2011-04-01 06:00:00");
	addGridData(Parameter("temeperature", "c"), Level("height", "m", 10, 10), "2011-04-01 06:00:00");
	addGridData(Parameter("temeperature", "c"), Level("height", "m", 2, 2), "2011-04-02 06:00:00");
	addGridData(Parameter("temeperature", "c"), Level("height", "m", 10, 10), "2011-04-02 06:00:00");
	addGridData(Parameter("temeperature", "c"), Level("height", "m", 2, 2), "2011-04-03 06:00:00");
	addGridData(Parameter("temeperature", "c"), Level("height", "m", 10, 10), "2011-04-03 06:00:00");
	addGridData(Parameter("wind speed", "m/s"), Level("height", "m", 10, 10), "2011-04-02 06:00:00");
	addGridData(Parameter("wind speed", "m/s"), Level("height", "m", 10, 10), "2011-04-03 06:00:00");
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData("wind speed", 1);

	BOOST_REQUIRE_EQUAL(1, gids.size());
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids.front());
}

BOOST_FIXTURE_TEST_CASE(multipleVersions, WdbIndexTestFixture)
{
	addGridData(0);
	addGridData(1);
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 0);
	BOOST_REQUIRE_EQUAL(2, gids.size());
	BOOST_CHECK_EQUAL(0, gids[0]);
	BOOST_CHECK_EQUAL(1, gids[1]);
}

BOOST_FIXTURE_TEST_CASE(missingVersions, WdbIndexTestFixture)
{
	addGridData(0, "2011-04-07 06:00:00");
	addGridData(1, "2011-04-07 06:00:00");
	addGridData(0, "2011-04-07 07:00:00");
	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData(defaultParameter.name(), 2);
	BOOST_REQUIRE_EQUAL(2, gids.size());
	BOOST_CHECK_EQUAL(2, gids[0]);
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[1]);
}

BOOST_FIXTURE_TEST_CASE(missingTimeStepWithManyVersions, WdbIndexTestFixture)
{
	addGridData(Parameter("temperature", "C"), "2011-04-07 06:00:00");
	addGridData(Parameter("temperature", "C"), "2011-04-07 07:00:00");
	addGridData(Parameter("temperature", "C"), "2011-04-07 08:00:00");
	addGridData(Parameter("pressure", "pa"), "2011-04-07 06:00:00", 0);
	addGridData(Parameter("pressure", "pa"), "2011-04-07 06:00:00", 1);
	addGridData(Parameter("pressure", "pa"), "2011-04-07 07:00:00", 0);

	WdbIndex index(gridData);

	WdbIndex::GidList gids = index.getData("pressure", 3);
	BOOST_REQUIRE_EQUAL(2, gids.size());
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[0]);
	BOOST_CHECK_EQUAL(WdbIndex::UNDEFINED_GID, gids[1]);
}



BOOST_AUTO_TEST_SUITE_END()

#endif
