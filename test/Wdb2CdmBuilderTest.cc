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

#include <wdb/Wdb2CdmBuilder.h>
#include <wdb/CdmNameTranslator.h>
#include <wdb/gridInformation/GridInformation.h>
#include <fimex/CDM.h>
#include <iostream>
#include <algorithm>
#include <boost/assign.hpp>


#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE WDB Interface Test
#include <boost/test/unit_test.hpp>

using namespace MetNoFimex;

class Wdb2CdmBuilderFixture
{
	struct TestingGridData : public wdb::GridData
	{
		TestingGridData(const wdb::Parameter & param,
				const wdb::Level & lvl,
				int version,
				const Time & validTo,
				const wdb::GridData::GridInformationPtr & grid,
				gid gridId) :
			wdb::GridData(param, lvl, version, validTo, gridId)
		{
			setGridInformation(grid);
		}
	};

	wdb::GridData::gid nextGid_;
public:
	Wdb2CdmBuilderFixture() : nextGid_(0) {}

	wdb::GridData::gid nextGid()
	{
		return nextGid_ ++;
	}

	static boost::posix_time::ptime t(const std::string & time)
	{
		return boost::posix_time::time_from_string(time);
	}

	static std::string cdmId()
	{
		return "air_temperature";
	}



	std::vector<wdb::GridData> gridData;
	CDM cdm;

	static const wdb::Parameter defaultParameter;
	static const wdb::Level defaultLevel;
	static const std::string defaultTime;
	static const wdb::GridData::GridInformationPtr defaultGrid;
	wdb::CdmNameTranslator tr;

	void addGridData(const wdb::Parameter & parameter = defaultParameter, const std::string & time = defaultTime, int version = 0)
	{
		gridData.push_back(TestingGridData(parameter, defaultLevel, version, t(time), defaultGrid, nextGid()));
	}

	void addGridData(const wdb::Parameter & parameter, const wdb::Level & lvl)
	{
		gridData.push_back(TestingGridData(parameter, lvl, 0, t(defaultTime), defaultGrid, nextGid()));
	}

	void addGridData(const wdb::Level & lvl, const std::string & time = defaultTime)
	{
		gridData.push_back(TestingGridData(defaultParameter, lvl, 0, t(time), defaultGrid, nextGid()));
	}

	void addGridData(const std::string & time)
	{
		gridData.push_back(TestingGridData(defaultParameter, defaultLevel, 0, t(time), defaultGrid, nextGid()));
	}

	void addGridData(int dataVersion, const std::string & time = defaultTime)
	{
		gridData.push_back(TestingGridData(defaultParameter, defaultLevel, dataVersion, t(time), defaultGrid, nextGid()));
	}

	void addGridData(const wdb::GridData::GridInformationPtr & gridInfo)
	{
		gridData.push_back(TestingGridData(defaultParameter, defaultLevel, 0, t(defaultTime), gridInfo, nextGid()));
	}
};
const wdb::Parameter Wdb2CdmBuilderFixture::defaultParameter("air temperature", "C");
const wdb::Level Wdb2CdmBuilderFixture::defaultLevel("distance above ground", "m", 0, 0);
const std::string Wdb2CdmBuilderFixture::defaultTime = "2011-03-18 06:00:00";
const wdb::GridData::GridInformationPtr Wdb2CdmBuilderFixture::defaultGrid(wdb::GridInformation::get("+proj=longlat +a=6367470.0 +towgs84=0,0,0 +no_defs", 30, 20));

struct same_entity
{
	const std::string name_;
	same_entity(const std::string & name) : name_(name) {}

	bool operator() (const CDMNamedEntity & entity)
	{
		return entity.getName() == name_;
	}
};


BOOST_AUTO_TEST_SUITE(Wdb2CdmBuilderTest)

BOOST_FIXTURE_TEST_CASE(setsBaseDimensions, Wdb2CdmBuilderFixture)
{
	addGridData(wdb::Level("lvl", "m", 1, 1));

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("latitude")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("longitude")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("time")) != dims.end());
	BOOST_CHECK_EQUAL(3, dims.size());
}

BOOST_FIXTURE_TEST_CASE(setsDimensionSizes, Wdb2CdmBuilderFixture)
{
	addGridData(wdb::Level("lvl", "m", 0, 0));
	addGridData(wdb::Level("lvl", "m", 1, 1));
	addGridData(wdb::Level("lvl", "m", 2, 2));

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("latitude")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("longitude")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("time")) != dims.end());

	BOOST_CHECK_EQUAL(4, dims.size());

	CDM::DimVec::const_iterator lvlDimension = std::find_if(dims.begin(), dims.end(), same_entity("lvl"));
	BOOST_REQUIRE(lvlDimension != dims.end());
	BOOST_CHECK_EQUAL(3, lvlDimension->getLength());
}

BOOST_FIXTURE_TEST_CASE(setsCorrectTimeDimensionSizeWithOneTimeStep, Wdb2CdmBuilderFixture)
{
	// Even if there is no need for valid time as a dimension for parameters,
	// we still add it, in order to give information that _all_ data has the
	// specified valid time.

	addGridData(wdb::Level("lvl", "m", 2, 2));

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();

	CDM::DimVec::const_iterator timeDimension = std::find_if(dims.begin(), dims.end(), same_entity("time"));
	BOOST_REQUIRE(timeDimension != dims.end());
	BOOST_CHECK_EQUAL(1, timeDimension->getLength());
}

BOOST_FIXTURE_TEST_CASE(setsCorrectTimeDimensionSize, Wdb2CdmBuilderFixture)
{
	addGridData(wdb::Level("lvl", "m", 2, 2), "2010-03-18 06:00:00");
	addGridData(wdb::Level("lvl", "m", 2, 2), "2010-03-18 07:00:00");
	addGridData(wdb::Level("lvl", "m", 0, 0), "2010-03-18 07:00:00");

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();

	CDM::DimVec::const_iterator timeDimension = std::find_if(dims.begin(), dims.end(), same_entity("time"));
	BOOST_REQUIRE(timeDimension != dims.end());
	BOOST_CHECK_EQUAL(2, timeDimension->getLength());
}

BOOST_FIXTURE_TEST_CASE(picksUpAllTimesFromSameParameter, Wdb2CdmBuilderFixture)
{
	// All times for varying levels should be picked up
	addGridData(wdb::Level("lvl", "m", 2, 2), "2010-03-18 06:00:00");
	addGridData(wdb::Level("lvl", "m", 2, 2), "2010-03-18 07:00:00");
	addGridData(wdb::Level("lvl", "m", 0, 0), "2000-01-01 00:00:00");

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();

	CDM::DimVec::const_iterator timeDimension = std::find_if(dims.begin(), dims.end(), same_entity("time"));
	BOOST_REQUIRE(timeDimension != dims.end());
	BOOST_CHECK_EQUAL(3, timeDimension->getLength());
}

BOOST_FIXTURE_TEST_CASE(ignoresIrrelevantTimeDimensions, Wdb2CdmBuilderFixture)
{
	// maybe implement this later..

//	// Any parameters which only have one time step will not get a time dimension
//	// This makes sense for such fields as topography
//	addGridData(wdb::Level("lvl", "m", 2, 2), "2010-03-18 06:00:00");
//	addGridData(wdb::Level("lvl", "m", 2, 2), "2010-03-18 07:00:00");
//	addGridData(wdb::Parameter("timeless", "stuff"), "2000-01-01 00:00:00");
//
//	const wdb::Wdb2CdmBuilder di(gridData, tr);
//	di.populate(cdm);
//
//	const CDM::DimVec & dims = cdm.getDimensions();
//
//	CDM::DimVec::const_iterator timeDimension = std::find_if(dims.begin(), dims.end(), same_entity("time"));
//	BOOST_REQUIRE(timeDimension != dims.end());
//	BOOST_CHECK_EQUAL(2, timeDimension->getLength());
}

BOOST_FIXTURE_TEST_CASE(setsVersionDimensions, Wdb2CdmBuilderFixture)
{
	addGridData(0);
	addGridData(1);
	addGridData(2);

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	try
	{
		const CDMDimension & dim = cdm.getDimension("version");
		BOOST_CHECK_EQUAL(3, dim.getLength());
	}
	catch ( CDMException & )
	{
		BOOST_FAIL("Unable to find dimension 'version'");
	}
}

BOOST_FIXTURE_TEST_CASE(versionsAddThemselvesAsVariables, Wdb2CdmBuilderFixture)
{
	addGridData(0);
	addGridData(1);

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	const std::string variable = "version";
	try
	{
		cdm.getVariable(variable); // will throw if variable does no exist
		BOOST_CHECK_EQUAL("data version", cdm.getAttribute(variable, "long_name").getStringValue());
		BOOST_CHECK_EQUAL("version", cdm.getAttribute(variable, "standard_name").getStringValue());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(zDimensionsAddThemselvesAsVariables, Wdb2CdmBuilderFixture)
{
	addGridData(wdb::Level("lvl", "m", 0, 0));
	addGridData(wdb::Level("lvl", "m", 1, 1));
	addGridData(wdb::Level("lvl", "m", 2, 2));

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	const std::string variable = "lvl";
	try
	{
		cdm.getVariable(variable); // will throw if variable does no exist
		BOOST_CHECK_EQUAL("m", cdm.getAttribute(variable, "units").getStringValue());
		BOOST_CHECK_EQUAL("lvl", cdm.getAttribute(variable, "long_name").getStringValue());
		BOOST_CHECK_EQUAL("lvl", cdm.getAttribute(variable, "standard_name").getStringValue());
		BOOST_CHECK_EQUAL("z", cdm.getAttribute(variable, "axis").getStringValue());

	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(manyZDimensions, Wdb2CdmBuilderFixture)
{
	addGridData(wdb::Parameter("temperature", "c"), wdb::Level("lvl", "m", 0, 0));
	addGridData(wdb::Parameter("temperature", "c"), wdb::Level("lvl", "m", 1, 1));
	addGridData(wdb::Parameter("pressure", "pa"), wdb::Level("foo bar", "x", 0, 0));
	addGridData(wdb::Parameter("pressure", "pa"), wdb::Level("foo bar", "x", 1, 1));

	try
	{
		const wdb::Wdb2CdmBuilder di(gridData, tr);
		di.populate(cdm);

		const std::string variableA = "lvl";
		const std::string variableB = "foo_bar";
		cdm.getVariable(variableA); // will throw if variableA does not exist
		BOOST_CHECK_EQUAL("m", cdm.getAttribute(variableA, "units").getStringValue());
		BOOST_CHECK_EQUAL("lvl", cdm.getAttribute(variableA, "long_name").getStringValue());
		BOOST_CHECK_EQUAL("lvl", cdm.getAttribute(variableA, "standard_name").getStringValue());
		BOOST_CHECK_EQUAL("z", cdm.getAttribute(variableA, "axis").getStringValue());

		cdm.getVariable(variableB); // will throw if variableB does not exist
		BOOST_CHECK_EQUAL("x", cdm.getAttribute(variableB, "units").getStringValue());
		BOOST_CHECK_EQUAL("foo bar", cdm.getAttribute(variableB, "long_name").getStringValue());
		BOOST_CHECK_EQUAL("foo_bar", cdm.getAttribute(variableB, "standard_name").getStringValue());
		BOOST_CHECK_EQUAL("z", cdm.getAttribute(variableB, "axis").getStringValue());

	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(addsTimeVariable, Wdb2CdmBuilderFixture)
{
	addGridData("2011-03-21 06:00:00");
	addGridData("2011-03-21 12:00:00");
	addGridData("2011-03-21 18:00:00");

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	try
	{
		cdm.getVariable("time");
		BOOST_CHECK_EQUAL("seconds since 1970-01-01 00:00:00 +00:00", cdm.getAttribute("time", "units").getStringValue());
		BOOST_CHECK_EQUAL("time", cdm.getAttribute("time", "long_name").getStringValue());
		BOOST_CHECK_EQUAL("time", cdm.getAttribute("time", "standard_name").getStringValue());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}


BOOST_FIXTURE_TEST_CASE(addsTimeToRelevantVariables, Wdb2CdmBuilderFixture)
{
	addGridData(defaultLevel, "2010-03-18 06:00:00");
	addGridData(defaultLevel, "2010-03-18 07:00:00");
	addGridData(defaultLevel, "2010-03-18 08:00:00");

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	try
	{
		const CDMVariable & var = cdm.getVariable(cdmId());
		const std::vector<std::string> & shape = var.getShape();
		BOOST_REQUIRE_LE(3, shape.size());
		BOOST_CHECK_EQUAL("longitude", shape[0]);
		BOOST_CHECK_EQUAL("latitude", shape[1]);
		BOOST_CHECK_EQUAL("time", shape[2]);
		BOOST_CHECK_EQUAL(3, shape.size());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(singleLevelInData, Wdb2CdmBuilderFixture)
{
	addGridData(wdb::Level("lvl", "m", 0, 0));

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	try
	{
		const CDMVariable & var = cdm.getVariable(cdmId());
		const std::vector<std::string> & shape = var.getShape();
		BOOST_REQUIRE_LE(2, shape.size());
		BOOST_CHECK_EQUAL("longitude", shape[0]);
		BOOST_CHECK_EQUAL("latitude", shape[1]);
		BOOST_CHECK_EQUAL(2, shape.size());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(severalLevelsInData, Wdb2CdmBuilderFixture)
{
	addGridData(wdb::Level("lvl", "m", 0, 0));
	addGridData(wdb::Level("lvl", "m", 1, 1));

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	try
	{
		const CDMVariable & var = cdm.getVariable(cdmId());
		const std::vector<std::string> & shape = var.getShape();
		BOOST_REQUIRE_LE(3, shape.size());
		BOOST_CHECK_EQUAL("longitude", shape[0]);
		BOOST_CHECK_EQUAL("latitude", shape[1]);
		BOOST_CHECK_EQUAL("lvl", shape[2]);
		BOOST_CHECK_EQUAL(3, shape.size());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(severalDataVersions, Wdb2CdmBuilderFixture)
{
	addGridData(0);
	addGridData(1);
	addGridData(2);

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	const CDMVariable & var = cdm.getVariable(cdmId());

	const std::vector<std::string> & shape = var.getShape();
	BOOST_REQUIRE_LE(3, shape.size());
	BOOST_CHECK_EQUAL("longitude", shape[0]);
	BOOST_CHECK_EQUAL("latitude", shape[1]);
	BOOST_CHECK_EQUAL("version", shape[2]);
	BOOST_CHECK_EQUAL(3, shape.size());
}

BOOST_FIXTURE_TEST_CASE(onlyOneTimeDimensionInVaraiableShape, Wdb2CdmBuilderFixture)
{
	addGridData(0, "2011-03-22 06:00:00");
	addGridData(1, "2011-03-22 06:00:00");
	addGridData(0, "2011-03-22 07:00:00");
	addGridData(1, "2011-03-22 07:00:00");
	addGridData(0, "2011-03-22 08:00:00");
	addGridData(1, "2011-03-22 08:00:00");

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	const CDMVariable & var = cdm.getVariable(cdmId());

	const std::vector<std::string> & shape = var.getShape();

	BOOST_REQUIRE_LE(4, shape.size());
	BOOST_CHECK_EQUAL("longitude", shape[0]);
	BOOST_CHECK_EQUAL("latitude", shape[1]);
	BOOST_CHECK_EQUAL("version", shape[2]);
	BOOST_CHECK_EQUAL("time", shape[3]);
	BOOST_CHECK_EQUAL(4, shape.size());
}

BOOST_FIXTURE_TEST_CASE(createsProjectionVariableLatLon, Wdb2CdmBuilderFixture)
{
	addGridData(defaultGrid); // +proj=longlat +a=6367470.0 +towgs84=0,0,0 +no_defs

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	try
	{
		const CDMVariable & var = cdm.getVariable("projection_latitude_longitude");
		BOOST_CHECK(var.getShape().empty());

		const CDMAttribute & attr = cdm.getAttribute("projection_latitude_longitude", "grid_mapping_name");
		BOOST_CHECK_EQUAL("latitude_longitude", attr.getStringValue());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(createsProjectionVariableRotatedLatLon, Wdb2CdmBuilderFixture)
{
	const std::string projDefinition = "+proj=ob_tran +o_proj=longlat +lon_0=-24 +o_lat_p=23.5 +a=6367470.0 +no_defs";
	wdb::GridData::GridInformationPtr grid(wdb::GridInformation::get(projDefinition, 30, 20));
	addGridData(grid);

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	try
	{
		std::string projectionName = "projection_rotated_latitude_longitude"; // change to projection_ob_tran?
		const CDMVariable & var = cdm.getVariable(projectionName);
		BOOST_CHECK(var.getShape().empty());

		const CDMAttribute & attr = cdm.getAttribute(projectionName, "proj4");
		BOOST_CHECK_EQUAL(projDefinition, attr.getStringValue());
}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(createsVariablesForLatLonProjection, Wdb2CdmBuilderFixture)
{
	addGridData();

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	BOOST_CHECK_THROW(cdm.getDimension("x"), CDMException);
	BOOST_CHECK_THROW(cdm.getVariable("x"), CDMException);
	BOOST_CHECK_THROW(cdm.getDimension("y"), CDMException);
	BOOST_CHECK_THROW(cdm.getVariable("y"), CDMException);

	try
	{
		const CDMVariable & latitude = cdm.getVariable("latitude");
		std::vector<std::string> shape = latitude.getShape();
		if ( not shape.empty() )
			BOOST_CHECK_EQUAL("latitude", shape[0]);
		BOOST_CHECK_EQUAL(1, shape.size());
		BOOST_CHECK_EQUAL("degree_north", cdm.getAttribute("latitude", "units").getStringValue());
		BOOST_CHECK_EQUAL("latitude", cdm.getAttribute("latitude", "long_name").getStringValue());
		BOOST_CHECK_EQUAL("latitude", cdm.getAttribute("latitude", "standard_name").getStringValue());

		const CDMVariable & longitude = cdm.getVariable("longitude");
		shape = longitude.getShape();
		if ( not shape.empty() )
			BOOST_CHECK_EQUAL("longitude", shape[0]);
		BOOST_CHECK_EQUAL(1, shape.size());
		BOOST_CHECK_EQUAL("degree_east", cdm.getAttribute("longitude", "units").getStringValue());
		BOOST_CHECK_EQUAL("longitude", cdm.getAttribute("longitude", "long_name").getStringValue());
		BOOST_CHECK_EQUAL("longitude", cdm.getAttribute("longitude", "standard_name").getStringValue());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(createsVariablesForRotatedLatLonProjection, Wdb2CdmBuilderFixture)
{
	const std::string projDefinition = "+proj=ob_tran +o_proj=longlat +lon_0=-24 +o_lat_p=23.5 +a=6367470.0 +no_defs";
	wdb::GridData::GridInformationPtr grid(wdb::GridInformation::get(projDefinition, 30, 20));
	addGridData(grid);

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	BOOST_CHECK_THROW(cdm.getDimension("x"), CDMException);
	BOOST_CHECK_THROW(cdm.getVariable("x"), CDMException);
	BOOST_CHECK_THROW(cdm.getDimension("y"), CDMException);
	BOOST_CHECK_THROW(cdm.getVariable("y"), CDMException);

	try
	{
		{
			const CDMVariable & rlon = cdm.getVariable("rlon");
			std::vector<std::string> shape = rlon.getShape();
			if ( not shape.empty() )
				BOOST_CHECK_EQUAL("rlon", shape[0]);
			BOOST_CHECK_EQUAL(1, shape.size());
			BOOST_CHECK_EQUAL("degrees", cdm.getAttribute("rlon", "units").getStringValue());
			BOOST_CHECK_EQUAL("rotated longitude", cdm.getAttribute("rlon", "long_name").getStringValue());
			BOOST_CHECK_EQUAL("grid_longitude", cdm.getAttribute("rlon", "standard_name").getStringValue());
		}
		{
			const CDMVariable & rlat = cdm.getVariable("rlat");
			std::vector<std::string> shape = rlat.getShape();
			if ( not shape.empty() )
				BOOST_CHECK_EQUAL("rlat", shape[0]);
			BOOST_CHECK_EQUAL(1, shape.size());
			BOOST_CHECK_EQUAL("degrees", cdm.getAttribute("rlat", "units").getStringValue());
			BOOST_CHECK_EQUAL("rotated latitude", cdm.getAttribute("rlat", "long_name").getStringValue());
			BOOST_CHECK_EQUAL("grid_latitude", cdm.getAttribute("rlat", "standard_name").getStringValue());
		}
		{
			const CDMVariable & latitude = cdm.getVariable("latitude");
			std::vector<std::string> shape = latitude.getShape();
			if ( not shape.empty() )
				BOOST_CHECK_EQUAL("rlat", shape[0]);
			if ( shape.size() > 1 )
				BOOST_CHECK_EQUAL("rlon", shape[1]);
			BOOST_CHECK_EQUAL(2, shape.size());
			BOOST_CHECK_EQUAL("degree_north", cdm.getAttribute("latitude", "units").getStringValue());
			BOOST_CHECK_EQUAL("latitude", cdm.getAttribute("latitude", "long_name").getStringValue());
			BOOST_CHECK_EQUAL("latitude", cdm.getAttribute("latitude", "standard_name").getStringValue());
		}
		{
			const CDMVariable & longitude = cdm.getVariable("longitude");
			std::vector<std::string> shape = longitude.getShape();
			if ( not shape.empty() )
				BOOST_CHECK_EQUAL("rlat", shape[0]);
			if ( shape.size() > 1 )
				BOOST_CHECK_EQUAL("rlon", shape[1]);
			BOOST_CHECK_EQUAL(2, shape.size());
			BOOST_CHECK_EQUAL("degree_east", cdm.getAttribute("longitude", "units").getStringValue());
			BOOST_CHECK_EQUAL("longitude", cdm.getAttribute("longitude", "long_name").getStringValue());
			BOOST_CHECK_EQUAL("longitude", cdm.getAttribute("longitude", "standard_name").getStringValue());
		}
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(createsVariablesForMetricProjection, Wdb2CdmBuilderFixture)
{
	const std::string projDefinition = "+proj=stere +lat_0=90 +lon_0=0 +lat_ts=60 +elips=sphere +a=6371000 +e=0";
	wdb::GridData::GridInformationPtr grid(wdb::GridInformation::get(projDefinition, 30, 20));
	addGridData(grid);

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	BOOST_CHECK_THROW(cdm.getDimension("rlat"), CDMException);
	BOOST_CHECK_THROW(cdm.getVariable("rlat"), CDMException);
	BOOST_CHECK_THROW(cdm.getDimension("rlon"), CDMException);
	BOOST_CHECK_THROW(cdm.getVariable("rlon"), CDMException);

	try
	{
		{
			const CDMVariable & xc = cdm.getVariable("xc");
			std::vector<std::string> shape = xc.getShape();
			if ( not shape.empty() )
				BOOST_CHECK_EQUAL("xc", shape[0]);
			BOOST_CHECK_EQUAL(1, shape.size());
			BOOST_CHECK_EQUAL("m", cdm.getAttribute("xc", "units").getStringValue());
			BOOST_CHECK_EQUAL("projection_x_coordinate", cdm.getAttribute("xc", "standard_name").getStringValue());
		}
		{
			const CDMVariable & yc = cdm.getVariable("yc");
			std::vector<std::string> shape = yc.getShape();
			if ( not shape.empty() )
				BOOST_CHECK_EQUAL("yc", shape[0]);
			BOOST_CHECK_EQUAL(1, shape.size());
			BOOST_CHECK_EQUAL("m", cdm.getAttribute("yc", "units").getStringValue());
			BOOST_CHECK_EQUAL("projection_y_coordinate", cdm.getAttribute("yc", "standard_name").getStringValue());
		}
		{
			const CDMVariable & lat1 = cdm.getVariable("lat1");
			std::vector<std::string> shape = lat1.getShape();
			if ( not shape.empty() )
				BOOST_CHECK_EQUAL("yc", shape[0]);
			if ( shape.size() > 1 )
				BOOST_CHECK_EQUAL("xc", shape[1]);
			BOOST_CHECK_EQUAL(2, shape.size());
			BOOST_CHECK_EQUAL("degree_north", cdm.getAttribute("lat1", "units").getStringValue());
			BOOST_CHECK_EQUAL("latitude", cdm.getAttribute("lat1", "long_name").getStringValue());
			BOOST_CHECK_EQUAL("latitude", cdm.getAttribute("lat1", "standard_name").getStringValue());
		}
		{
			const CDMVariable & lon1 = cdm.getVariable("lon1");
			std::vector<std::string> shape = lon1.getShape();
			if ( not shape.empty() )
				BOOST_CHECK_EQUAL("yc", shape[0]);
			if ( shape.size() > 1 )
				BOOST_CHECK_EQUAL("xc", shape[1]);
			BOOST_CHECK_EQUAL(2, shape.size());
			BOOST_CHECK_EQUAL("degree_east", cdm.getAttribute("lon1", "units").getStringValue());
			BOOST_CHECK_EQUAL("longitude", cdm.getAttribute("lon1", "long_name").getStringValue());
			BOOST_CHECK_EQUAL("longitude", cdm.getAttribute("lon1", "standard_name").getStringValue());
		}
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}


BOOST_FIXTURE_TEST_CASE(setsReferenceTimeVariable, Wdb2CdmBuilderFixture)
{
	addGridData();

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	try
	{
		const std::string reftime = "forecast_reference_time";
		const CDMVariable & reftimeVariable = cdm.getVariable(reftime);
		BOOST_CHECK_EQUAL(CDM_DOUBLE, reftimeVariable.getDataType());
		BOOST_CHECK_EQUAL("seconds since 1970-01-01 00:00:00 +00:00", cdm.getAttribute(reftime, "units").getStringValue());
		BOOST_CHECK_EQUAL("forecast_reference_time", cdm.getAttribute(reftime, "long_name").getStringValue());
		BOOST_CHECK_EQUAL("forecast_reference_time", cdm.getAttribute(reftime, "standard_name").getStringValue());
	}
	catch( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(setsCorrectParameterAttributes, Wdb2CdmBuilderFixture)
{
	addGridData();

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	try
	{
		BOOST_CHECK_EQUAL("projection_latitude_longitude", cdm.getAttribute(cdmId(), "grid_mapping").getStringValue());
		BOOST_CHECK_EQUAL("C", cdm.getAttribute(cdmId(), "units").getStringValue());
		BOOST_CHECK_EQUAL("nan", cdm.getAttribute(cdmId(), "_FillValue").getStringValue());
		BOOST_CHECK_EQUAL("longitude latitude", cdm.getAttribute(cdmId(), "coordinates").getStringValue());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(setsCorrectParameterAttributes_2, Wdb2CdmBuilderFixture)
{
	const std::string projDefinition = "+proj=ob_tran +o_proj=longlat +lon_0=-24 +o_lat_p=23.5 +a=6367470.0 +no_defs";
	wdb::GridData::GridInformationPtr grid(wdb::GridInformation::get(projDefinition, 30, 20));
	addGridData(grid);

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	try
	{
		BOOST_CHECK_EQUAL("projection_rotated_latitude_longitude", cdm.getAttribute(cdmId(), "grid_mapping").getStringValue());
		BOOST_CHECK_EQUAL("C", cdm.getAttribute(cdmId(), "units").getStringValue());
		BOOST_CHECK_EQUAL("nan", cdm.getAttribute(cdmId(), "_FillValue").getStringValue());
		BOOST_CHECK_EQUAL("longitude latitude", cdm.getAttribute(cdmId(), "coordinates").getStringValue());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

//BOOST_FIXTURE_TEST_CASE(timeIndexLoopkup, Wdb2CdmBuilderFixture)
//{
//	addGridData("2011-03-31 06:00:00");
//	addGridData("2011-03-31 18:00:00");
//	addGridData("2011-03-31 12:00:00");
//	addGridData("2011-04-01 06:00:00");
//
//	const wdb::Wdb2CdmBuilder di(gridData, tr);
//	di.populate(cdm);
//
//	wdb::Wdb2CdmBuilder::Time t = boost::posix_time::time_from_string("2011-03-31 06:00:00");
//	boost::posix_time::time_duration offset(6, 0, 0);
//	BOOST_CHECK_EQUAL(t, di.timeFromIndex(1));
//	BOOST_CHECK_EQUAL(t + offset, di.timeFromIndex(2));
//	BOOST_CHECK_EQUAL(t + (offset * 2), di.timeFromIndex(3));
//	BOOST_CHECK_EQUAL(t + (offset * 4), di.timeFromIndex(4));
//}
//
//BOOST_FIXTURE_TEST_CASE(throwOnInvalidTimeIndexLoopkup, Wdb2CdmBuilderFixture)
//{
//	addGridData("2011-03-31 06:00:00");
//	addGridData("2011-03-31 18:00:00");
//
//	const wdb::Wdb2CdmBuilder di(gridData, tr);
//	di.populate(cdm);
//
//	BOOST_CHECK_THROW(di.timeFromIndex(0), CDMException);
//	di.timeFromIndex(1);
//	di.timeFromIndex(2);
//	BOOST_CHECK_THROW(di.timeFromIndex(3), CDMException);
//}

BOOST_FIXTURE_TEST_CASE(insertsEntriesForMissingTime, Wdb2CdmBuilderFixture)
{
	addGridData(wdb::Parameter("temperature", "C"), "2011-03-31 06:00:00", 0);
	addGridData(wdb::Parameter("temperature", "C"), "2011-03-31 06:00:00", 1);
	addGridData(wdb::Parameter("temperature", "C"), "2011-03-31 07:00:00", 0);
	addGridData(wdb::Parameter("temperature", "C"), "2011-03-31 07:00:00", 1);
	addGridData(wdb::Parameter("temperature", "C"), "2011-03-31 08:00:00", 0);
	addGridData(wdb::Parameter("temperature", "C"), "2011-03-31 08:00:00", 1);
	addGridData(wdb::Parameter("pressure", "pa"), "2011-03-31 06:00:00", 0);
	addGridData(wdb::Parameter("pressure", "pa"), "2011-03-31 06:00:00", 1);
	// addGridData(wdb::Parameter("pressure", "pa"), "2011-03-31 07:00:00", 0); // missing, but a missing gid entry should be added
	addGridData(wdb::Parameter("pressure", "pa"), "2011-03-31 07:00:00", 1);
	addGridData(wdb::Parameter("pressure", "pa"), "2011-03-31 08:00:00", 0);
	addGridData(wdb::Parameter("pressure", "pa"), "2011-03-31 08:00:00", 1);

	try
	{
		const wdb::Wdb2CdmBuilder di(gridData, tr);
		di.populate(cdm);

		const CDMDimension & timeDimension = cdm.getDimension("time");
		BOOST_CHECK_EQUAL(3, timeDimension.getLength());

		std::vector<wdb::Wdb2CdmBuilder::gid> gids = di.getGridIdentifiers("pressure", 2 /*t("2011-03-31 07:00:00")*/);

		BOOST_REQUIRE(gids.size() >= 1);
		BOOST_CHECK_EQUAL(wdb::WdbIndex::UNDEFINED_GID, gids.front());
		BOOST_CHECK_EQUAL(8, gids.back());
		BOOST_CHECK_EQUAL(2, gids.size());
	}
	catch ( std::exception & e )
	{
		BOOST_FAIL(e.what());
	}
}


BOOST_FIXTURE_TEST_CASE(insertsEntriesForMissingVersion, Wdb2CdmBuilderFixture)
{
	addGridData(0, "2011-03-31 06:00:00");
	addGridData(1, "2011-03-31 06:00:00");
	addGridData(2, "2011-03-31 06:00:00");
	addGridData(0, "2011-03-31 18:00:00");
	// addGridData(1, "2011-03-31 06:00:00"); // missing, but a missing gid entry should be added
	addGridData(2, "2011-03-31 18:00:00");

	const wdb::Wdb2CdmBuilder di(gridData, tr);
	di.populate(cdm);

	const CDMDimension & timeDimension = cdm.getDimension("time");
	BOOST_CHECK_EQUAL(2, timeDimension.getLength());

	const CDMDimension & versionDimension = cdm.getDimension("version");
	BOOST_CHECK_EQUAL(3, versionDimension.getLength());

	std::vector<wdb::Wdb2CdmBuilder::gid> gids = di.getGridIdentifiers(defaultParameter.name(), 2/*t("2011-03-31 18:00:00")*/);

	BOOST_CHECK_EQUAL(3, gids.size());
	BOOST_REQUIRE(gids.size() >= 3);
	BOOST_CHECK_EQUAL(3, gids[0]);
	BOOST_CHECK_EQUAL(wdb::WdbIndex::UNDEFINED_GID, gids[1]);
	BOOST_CHECK_EQUAL(4, gids[2]);
}


BOOST_AUTO_TEST_SUITE_END()

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
