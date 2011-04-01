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

#include <wdb/DataIndex.h>
#include <wdb/CdmNameTranslator.h>
#include <wdb/GridInformation.h>
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

class DataIndexFixture
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
	DataIndexFixture() : nextGid_(0) {}

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

	void addGridData(const wdb::Parameter & parameter = defaultParameter, const std::string & time = defaultTime)
	{
		gridData.push_back(TestingGridData(parameter, defaultLevel, 0, t(time), defaultGrid, nextGid()));
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
const wdb::Parameter DataIndexFixture::defaultParameter("air temperature", "C");
const wdb::Level DataIndexFixture::defaultLevel("distance above ground", "m", 0, 0);
const std::string DataIndexFixture::defaultTime = "2011-03-18 06:00:00";
const wdb::GridData::GridInformationPtr DataIndexFixture::defaultGrid(new wdb::GridInformation("+proj=longlat +a=6367470.0 +towgs84=0,0,0 +no_defs", 30, 20));

struct same_entity
{
	const std::string name_;
	same_entity(const std::string & name) : name_(name) {}

	bool operator() (const CDMNamedEntity & entity)
	{
		return entity.getName() == name_;
	}
};


BOOST_AUTO_TEST_SUITE(DataIndexTest)

BOOST_FIXTURE_TEST_CASE(setsBaseDimensions, DataIndexFixture)
{
	addGridData(wdb::Level("lvl", "m", 1, 1));

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("x")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("y")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("time")) != dims.end());
	BOOST_CHECK_EQUAL(3, dims.size());
}

BOOST_FIXTURE_TEST_CASE(setsDimensionSizes, DataIndexFixture)
{
	addGridData(wdb::Level("lvl", "m", 0, 0));
	addGridData(wdb::Level("lvl", "m", 1, 1));
	addGridData(wdb::Level("lvl", "m", 2, 2));

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("x")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("y")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("time")) != dims.end());

	BOOST_CHECK_EQUAL(4, dims.size());

	CDM::DimVec::const_iterator lvlDimension = std::find_if(dims.begin(), dims.end(), same_entity("lvl"));
	BOOST_REQUIRE(lvlDimension != dims.end());
	BOOST_CHECK_EQUAL(3, lvlDimension->getLength());
}

BOOST_FIXTURE_TEST_CASE(setsCorrectTimeDimensionSizeWithOneTimeStep, DataIndexFixture)
{
	// since there is only one time here, we don't add time as a dimension.
	// but we still add time as an unlimited dimension (of size 0).
	//
	// (todo: is there any problems with this?)

	addGridData(wdb::Level("lvl", "m", 2, 2));

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();

	CDM::DimVec::const_iterator timeDimension = std::find_if(dims.begin(), dims.end(), same_entity("time"));
	BOOST_REQUIRE(timeDimension != dims.end());
	BOOST_CHECK_EQUAL(0, timeDimension->getLength());
}

BOOST_FIXTURE_TEST_CASE(setsCorrectTimeDimensionSize, DataIndexFixture)
{
	addGridData(wdb::Level("lvl", "m", 2, 2), "2010-03-18 06:00:00");
	addGridData(wdb::Level("lvl", "m", 2, 2), "2010-03-18 07:00:00");
	addGridData(wdb::Level("lvl", "m", 0, 0), "2010-03-18 07:00:00");

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();

	CDM::DimVec::const_iterator timeDimension = std::find_if(dims.begin(), dims.end(), same_entity("time"));
	BOOST_REQUIRE(timeDimension != dims.end());
	BOOST_CHECK_EQUAL(2, timeDimension->getLength());
}

BOOST_FIXTURE_TEST_CASE(picksUpAllTimesFromSameParameter, DataIndexFixture)
{
	// All times for varying levels should be picked up
	addGridData(wdb::Level("lvl", "m", 2, 2), "2010-03-18 06:00:00");
	addGridData(wdb::Level("lvl", "m", 2, 2), "2010-03-18 07:00:00");
	addGridData(wdb::Level("lvl", "m", 0, 0), "2000-01-01 00:00:00");

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();

	CDM::DimVec::const_iterator timeDimension = std::find_if(dims.begin(), dims.end(), same_entity("time"));
	BOOST_REQUIRE(timeDimension != dims.end());
	BOOST_CHECK_EQUAL(3, timeDimension->getLength());
}

BOOST_FIXTURE_TEST_CASE(ignoresIrrelevantTimeDimensions, DataIndexFixture)
{
	// Any parameters which only have one time step will not get a time dimension
	// This makes sense for such fields as topography
	addGridData(wdb::Level("lvl", "m", 2, 2), "2010-03-18 06:00:00");
	addGridData(wdb::Level("lvl", "m", 2, 2), "2010-03-18 07:00:00");
	addGridData(wdb::Parameter("timeless", "stuff"), "2000-01-01 00:00:00");

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();

	CDM::DimVec::const_iterator timeDimension = std::find_if(dims.begin(), dims.end(), same_entity("time"));
	BOOST_REQUIRE(timeDimension != dims.end());
	BOOST_CHECK_EQUAL(2, timeDimension->getLength());
}

BOOST_FIXTURE_TEST_CASE(setsVersionDimensions, DataIndexFixture)
{
	addGridData(0);
	addGridData(1);
	addGridData(2);

	const wdb::DataIndex di(gridData, tr);
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

BOOST_FIXTURE_TEST_CASE(versionsAddThemselvesAsVariables, DataIndexFixture)
{
	addGridData(0);
	addGridData(1);

	const wdb::DataIndex di(gridData, tr);
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

BOOST_FIXTURE_TEST_CASE(zDimensionsAddThemselvesAsVariables, DataIndexFixture)
{
	addGridData(wdb::Level("lvl", "m", 0, 0));
	addGridData(wdb::Level("lvl", "m", 1, 1));
	addGridData(wdb::Level("lvl", "m", 2, 2));

	const wdb::DataIndex di(gridData, tr);
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

BOOST_FIXTURE_TEST_CASE(manyZDimensions, DataIndexFixture)
{
	addGridData(wdb::Level("lvl", "m", 0, 0));
	addGridData(wdb::Level("lvl", "m", 1, 1));
	addGridData(wdb::Level("foo bar", "x", 0, 0));
	addGridData(wdb::Level("foo bar", "x", 1, 1));

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	const std::string variableA = "lvl";
	const std::string variableB = "foo_bar";
	try
	{
		cdm.getVariable(variableA); // will throw if variableA does no exist
		BOOST_CHECK_EQUAL("m", cdm.getAttribute(variableA, "units").getStringValue());
		BOOST_CHECK_EQUAL("lvl", cdm.getAttribute(variableA, "long_name").getStringValue());
		BOOST_CHECK_EQUAL("lvl", cdm.getAttribute(variableA, "standard_name").getStringValue());
		BOOST_CHECK_EQUAL("z", cdm.getAttribute(variableA, "axis").getStringValue());

		cdm.getVariable(variableB); // will throw if variableB does no exist
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

BOOST_FIXTURE_TEST_CASE(addsTimeVariable, DataIndexFixture)
{
	addGridData("2011-03-21 06:00:00");
	addGridData("2011-03-21 12:00:00");
	addGridData("2011-03-21 18:00:00");

	const wdb::DataIndex di(gridData, tr);
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


BOOST_FIXTURE_TEST_CASE(addsTimeToRelevantVariables, DataIndexFixture)
{
	addGridData(defaultLevel, "2010-03-18 06:00:00");
	addGridData(defaultLevel, "2010-03-18 07:00:00");
	addGridData(defaultLevel, "2010-03-18 08:00:00");

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	try
	{
		const CDMVariable & var = cdm.getVariable(cdmId());
		const std::vector<std::string> & shape = var.getShape();
		BOOST_REQUIRE_LE(3, shape.size());
		BOOST_CHECK_EQUAL("x", shape[0]);
		BOOST_CHECK_EQUAL("y", shape[1]);
		BOOST_CHECK_EQUAL("time", shape[2]);
		BOOST_CHECK_EQUAL(3, shape.size());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(singleLevelInData, DataIndexFixture)
{
	addGridData(wdb::Level("lvl", "m", 0, 0));

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	try
	{
		const CDMVariable & var = cdm.getVariable(cdmId());
		const std::vector<std::string> & shape = var.getShape();
		BOOST_REQUIRE_LE(2, shape.size());
		BOOST_CHECK_EQUAL("x", shape[0]);
		BOOST_CHECK_EQUAL("y", shape[1]);
		BOOST_CHECK_EQUAL(2, shape.size());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(severalLevelsInData, DataIndexFixture)
{
	addGridData(wdb::Level("lvl", "m", 0, 0));
	addGridData(wdb::Level("lvl", "m", 1, 1));

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	try
	{
		const CDMVariable & var = cdm.getVariable(cdmId());
		const std::vector<std::string> & shape = var.getShape();
		BOOST_REQUIRE_LE(3, shape.size());
		BOOST_CHECK_EQUAL("x", shape[0]);
		BOOST_CHECK_EQUAL("y", shape[1]);
		BOOST_CHECK_EQUAL("lvl", shape[2]);
		BOOST_CHECK_EQUAL(3, shape.size());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(severalDataVersions, DataIndexFixture)
{
	addGridData(0);
	addGridData(1);
	addGridData(2);

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	const CDMVariable & var = cdm.getVariable(cdmId());

	const std::vector<std::string> & shape = var.getShape();
	BOOST_REQUIRE_LE(3, shape.size());
	BOOST_CHECK_EQUAL("x", shape[0]);
	BOOST_CHECK_EQUAL("y", shape[1]);
	BOOST_CHECK_EQUAL("version", shape[2]);
	BOOST_CHECK_EQUAL(3, shape.size());
}

BOOST_FIXTURE_TEST_CASE(onlyOneTimeDimensionInVaraiableShape, DataIndexFixture)
{
	addGridData(0, "2011-03-22 06:00:00");
	addGridData(1, "2011-03-22 06:00:00");
	addGridData(0, "2011-03-22 07:00:00");
	addGridData(1, "2011-03-22 07:00:00");
	addGridData(0, "2011-03-22 08:00:00");
	addGridData(1, "2011-03-22 08:00:00");

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	const CDMVariable & var = cdm.getVariable(cdmId());

	const std::vector<std::string> & shape = var.getShape();

	BOOST_REQUIRE_LE(4, shape.size());
	BOOST_CHECK_EQUAL("x", shape[0]);
	BOOST_CHECK_EQUAL("y", shape[1]);
	BOOST_CHECK_EQUAL("version", shape[2]);
	BOOST_CHECK_EQUAL("time", shape[3]);
	BOOST_CHECK_EQUAL(4, shape.size());
}

BOOST_FIXTURE_TEST_CASE(createsProjectionVariable, DataIndexFixture)
{
	addGridData(defaultGrid);

	const wdb::DataIndex di(gridData, tr);
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

BOOST_FIXTURE_TEST_CASE(createsProjectionVariable_2, DataIndexFixture)
{
	const std::string projDefinition = "+proj=ob_tran +o_proj=longlat +lon_0=-24 +o_lat_p=23.5 +a=6367470.0 +no_defs";
	wdb::GridData::GridInformationPtr grid(new wdb::GridInformation(projDefinition, 30, 20));
	addGridData(grid);

	const wdb::DataIndex di(gridData, tr);
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

BOOST_FIXTURE_TEST_CASE(createsTranslatedLatLonVariables, DataIndexFixture)
{
	addGridData();

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	try
	{
		const CDMVariable & var = cdm.getVariable("projection_latitude_longitude");
		BOOST_CHECK(var.getShape().empty());

		const CDMAttribute & attr = cdm.getAttribute("projection_latitude_longitude", "grid_mapping_name");
		BOOST_CHECK_EQUAL("latitude_longitude", attr.getStringValue());

		const CDMDimension & xDimension = cdm.getDimension("x");
		BOOST_CHECK_EQUAL(30, xDimension.getLength());

		const CDMDimension & yDimension = cdm.getDimension("y");
		BOOST_CHECK_EQUAL(20, yDimension.getLength());

		const CDMVariable & x = cdm.getVariable("x");
		const CDMVariable & y = cdm.getVariable("y");

		const CDMVariable & latitude = cdm.getVariable("latitude");
		std::vector<std::string> shape = latitude.getShape();
		BOOST_CHECK_EQUAL(2, shape.size());
		BOOST_CHECK(std::find(shape.begin(), shape.end(), "x") != shape.end());
		BOOST_CHECK(std::find(shape.begin(), shape.end(), "y") != shape.end());

		const CDMVariable & longitude = cdm.getVariable("longitude");
		shape = longitude.getShape();
		BOOST_CHECK_EQUAL(2, shape.size());
		BOOST_CHECK(std::find(shape.begin(), shape.end(), "x") != shape.end());
		BOOST_CHECK(std::find(shape.begin(), shape.end(), "y") != shape.end());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_FIXTURE_TEST_CASE(setsReferenceTimeVariable, DataIndexFixture)
{
	addGridData();

	const wdb::DataIndex di(gridData, tr);
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

BOOST_FIXTURE_TEST_CASE(setsCorrectParameterAttributes, DataIndexFixture)
{
	addGridData();

	const wdb::DataIndex di(gridData, tr);
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

BOOST_FIXTURE_TEST_CASE(setsCorrectParameterAttributes_2, DataIndexFixture)
{
	const std::string projDefinition = "+proj=ob_tran +o_proj=longlat +lon_0=-24 +o_lat_p=23.5 +a=6367470.0 +no_defs";
	wdb::GridData::GridInformationPtr grid(new wdb::GridInformation(projDefinition, 30, 20));
	addGridData(grid);

	const wdb::DataIndex di(gridData, tr);
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

BOOST_FIXTURE_TEST_CASE(timeIndexLoopkup, DataIndexFixture)
{
	addGridData("2011-03-31 06:00:00");
	addGridData("2011-03-31 18:00:00");
	addGridData("2011-03-31 12:00:00");
	addGridData("2011-04-01 06:00:00");

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	wdb::DataIndex::Time t = boost::posix_time::time_from_string("2011-03-31 06:00:00");
	boost::posix_time::time_duration offset(6, 0, 0);
	BOOST_CHECK_EQUAL(t, di.timeFromIndex(1));
	BOOST_CHECK_EQUAL(t + offset, di.timeFromIndex(2));
	BOOST_CHECK_EQUAL(t + (offset * 2), di.timeFromIndex(3));
	BOOST_CHECK_EQUAL(t + (offset * 4), di.timeFromIndex(4));
}

BOOST_FIXTURE_TEST_CASE(throwOnInvalidTimeIndexLoopkup, DataIndexFixture)
{
	addGridData("2011-03-31 06:00:00");
	addGridData("2011-03-31 18:00:00");

	const wdb::DataIndex di(gridData, tr);
	di.populate(cdm);

	BOOST_CHECK_THROW(di.timeFromIndex(0), CDMException);
	di.timeFromIndex(1);
	di.timeFromIndex(2);
	BOOST_CHECK_THROW(di.timeFromIndex(3), CDMException);
}

BOOST_AUTO_TEST_SUITE_END()

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
