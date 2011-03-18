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

/// Do not add any fields to this class they will disappear
class TestingGridData : public wdb::GridData
{
	static boost::posix_time::ptime t(const std::string & time)
	{
		return boost::posix_time::time_from_string(time);
	}
public:
	TestingGridData(const wdb::Level & lvl) :
		wdb::GridData(wdb::Parameter("air temperature", "C"), lvl, 0, t("2011-03-18 06:00:00"), 0)
	{}
};

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

BOOST_AUTO_TEST_CASE(singleLevelInData)
{
	std::vector<wdb::GridData> gridData;
	gridData.push_back(TestingGridData(wdb::Level("lvl", 0, 0)));

	const wdb::DataIndex di(gridData);
	CDM cdm;
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("latitude")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("longitude")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("time")) != dims.end());
	BOOST_CHECK_EQUAL(3, dims.size());

	try
	{
		const CDMVariable & var = cdm.getVariable("air_temperature");
		const std::vector<std::string> & shape = var.getShape();
		BOOST_REQUIRE(shape.size() >= 2);
		BOOST_CHECK_EQUAL("longitude", shape[0]);
		BOOST_CHECK_EQUAL("latitude", shape[1]);
		BOOST_CHECK_EQUAL(2, shape.size());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_AUTO_TEST_CASE(severalLevelsInData)
{
	std::vector<wdb::GridData> gridData;
	gridData.push_back(TestingGridData(wdb::Level("lvl", 0, 0)));
	gridData.push_back(TestingGridData(wdb::Level("lvl", 1, 1)));

	const wdb::DataIndex di(gridData);

	CDM cdm;
	di.populate(cdm);

	const CDM::DimVec & dims = cdm.getDimensions();
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("latitude")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("longitude")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("time")) != dims.end());
	BOOST_CHECK(std::find_if(dims.begin(), dims.end(), same_entity("lvl")) != dims.end());
	BOOST_CHECK_EQUAL(4, dims.size());

	try
	{
		const CDMVariable & var = cdm.getVariable("air_temperature");
		const std::vector<std::string> & shape = var.getShape();
		BOOST_REQUIRE(shape.size() >= 3);
		BOOST_CHECK_EQUAL("lvl", shape[0]);
		BOOST_CHECK_EQUAL("longitude", shape[1]);
		BOOST_CHECK_EQUAL("latitude", shape[2]);
		BOOST_CHECK_EQUAL(3, shape.size());
	}
	catch ( CDMException & e )
	{
		BOOST_FAIL(e.what());
	}
}

BOOST_AUTO_TEST_SUITE_END()


#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
