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
#include <wdb/config/WdbConfiguration.h>
#include <fimex/CDMException.h>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <string>
#include <sstream>
#include <set>
#include <iostream>

BOOST_AUTO_TEST_SUITE(WdbConfigurationTest)

using namespace MetNoFimex::wdb;

class Fixture
{
};
namespace
{
std::set<std::string> extractPqConnectString(const std::string & s)
{
	std::istringstream config(s);
	WdbConfiguration conf(config);

	std::string pqConnect = conf.pqDatabaseConnectString();
	std::set<std::string> items;
	boost::algorithm::split(items, pqConnect, boost::algorithm::is_space());

	return items;
}
bool hasKey(const std::set<std::string> & config, const std::string & key)
{
	std::string toFind = key + "=";

	std::string::size_type size = toFind.size();

	for ( std::set<std::string>::const_iterator find = config.begin(); find != config.end(); ++ find )
		if ( find->substr(0,size) == toFind)
			return true;
	return false;
}
}

BOOST_FIXTURE_TEST_CASE(invalidOption, Fixture)
{
	std::istringstream config("invalid_option = foo");
	WdbConfiguration conf(config);
}

BOOST_FIXTURE_TEST_CASE(garbageInConfig, Fixture)
{
	std::istringstream config("database = foo\nhallo?");
	BOOST_CHECK_THROW(WdbConfiguration conf(config), MetNoFimex::CDMException);
}



BOOST_FIXTURE_TEST_CASE(test, Fixture)
{
	std::set<std::string> items = extractPqConnectString(
			"host = wdb\n"
			"port = 5433\n"
			"database = my_wdb\n"
			"user = me"
			);

	BOOST_CHECK_EQUAL(4, items.size());
	BOOST_CHECK(items.find("dbname=my_wdb") != items.end());
	BOOST_CHECK(items.find("host=wdb") != items.end());
	BOOST_CHECK(items.find("port=5433") != items.end());
	BOOST_CHECK(items.find("user=me") != items.end());
}

BOOST_FIXTURE_TEST_CASE(commentsInConfig, Fixture)
{
	std::set<std::string> items = extractPqConnectString(
			"# This is a comment\n"
			" # leading spaces are allowed\n"
			"host = wdb2 # and so is this\n"
			"port = 5434\n"
			"database = my_other_wdb\n"
			"user = you");

	BOOST_CHECK_EQUAL(4, items.size());
	BOOST_CHECK(items.find("dbname=my_other_wdb") != items.end());
	BOOST_CHECK(items.find("host=wdb2") != items.end());
	BOOST_CHECK(items.find("port=5434") != items.end());
	BOOST_CHECK(items.find("user=you") != items.end());
}

BOOST_FIXTURE_TEST_CASE(defaultHost, Fixture)
{
	std::set<std::string> items = extractPqConnectString(
			"port = 5433\n"
			"database = my_wdb\n"
			"user = me");

	BOOST_CHECK_EQUAL(3, items.size());
	BOOST_CHECK(items.find("dbname=my_wdb") != items.end());
	//BOOST_CHECK(items.find("host=wdb") != items.end());
	BOOST_CHECK(items.find("port=5433") != items.end());
	BOOST_CHECK(items.find("user=me") != items.end());
}

BOOST_FIXTURE_TEST_CASE(defaultPort, Fixture)
{
	std::set<std::string> items = extractPqConnectString(
			"host = wdb\n"
			"database = my_wdb\n"
			"user = me"
			);

	BOOST_CHECK_EQUAL(4, items.size());
	BOOST_CHECK(items.find("port=5432") != items.end());
}

BOOST_FIXTURE_TEST_CASE(defaultDatabase, Fixture)
{
	std::set<std::string> items = extractPqConnectString(
			"host = wdb\n"
			"port = 5433\n"
			"user = me"
			);

	BOOST_CHECK_EQUAL(4, items.size());
	BOOST_CHECK(items.find("dbname=wdb") != items.end());
}

BOOST_FIXTURE_TEST_CASE(defaultUser, Fixture)
{
	std::set<std::string> items = extractPqConnectString(
			"host = wdb\n"
			"port = 5433\n"
			"database = my_wdb\n"
			);

	BOOST_CHECK_EQUAL(4, items.size());

	// since user name is dependent on PGUSER or USER environment variable, we
	// don't check the value of user:
	BOOST_CHECK(hasKey(items, "user"));
}

BOOST_FIXTURE_TEST_CASE(emptyConfig, Fixture)
{
	std::set<std::string> items = extractPqConnectString("");

	BOOST_CHECK_EQUAL(3, items.size());
	BOOST_CHECK(items.find("dbname=wdb") != items.end());
	BOOST_CHECK(items.find("port=5432") != items.end());
	BOOST_CHECK(hasKey(items, "user")); // see defaultUser test
}

BOOST_FIXTURE_TEST_CASE(implicitWciUser, Fixture)
{
	std::istringstream config("user = foo");
	WdbConfiguration conf(config);

	BOOST_CHECK_EQUAL("foo", conf.wciUser());
}

BOOST_FIXTURE_TEST_CASE(explicitWciUser, Fixture)
{
	std::istringstream config("user = foo\nwci.user = bar");
	WdbConfiguration conf(config);

	BOOST_CHECK_EQUAL("bar", conf.wciUser());
}

BOOST_FIXTURE_TEST_CASE(emptyWciReadSection, Fixture)
{
	std::istringstream config("database = whatever\n");
	WdbConfiguration conf(config);

	const WciReadQuerySpecification & query = conf.query();

	BOOST_CHECK( ! query.dataProvider());
	BOOST_CHECK( ! query.location() );
	BOOST_CHECK( ! query.referenceTime() );
	BOOST_CHECK( ! query.parameter() );
	BOOST_CHECK( ! query.dataVersion() );
}


BOOST_FIXTURE_TEST_CASE(dataproviders, Fixture)
{
	std::istringstream config(
			"[wci.read]\n"
			"dataprovider = foo\n"
			"dataprovider = bar\n");
	WdbConfiguration conf(config);

	const std::set<std::string> * providers = conf.query().dataProvider();
	BOOST_REQUIRE(providers);
	BOOST_CHECK_EQUAL(2, providers->size());
	BOOST_CHECK(providers->find("foo") != providers->end());
	BOOST_CHECK(providers->find("bar") != providers->end());
}

BOOST_FIXTURE_TEST_CASE(location, Fixture)
{
	std::istringstream config(
			"[wci.read]\n"
			"location = some where\n");
	WdbConfiguration conf(config);

	const std::string * loc = conf.query().location();
	BOOST_REQUIRE(loc);
	BOOST_CHECK_EQUAL("some where", * loc);
}

BOOST_FIXTURE_TEST_CASE(manyLocations, Fixture)
{
	std::istringstream config(
			"[wci.read]\n"
			"location = oslo\n"
			"location = bergen\n");

	BOOST_CHECK_THROW(WdbConfiguration conf(config), std::runtime_error);
}

BOOST_FIXTURE_TEST_CASE(referenceTime, Fixture)
{
	std::istringstream config(
			"[wci]\n"
			"read.referencetime = 2011-05-16 06:00:00Z\n");
	WdbConfiguration conf(config);

	const std::string * t = conf.query().referenceTime();
	BOOST_REQUIRE(t);
	BOOST_CHECK_EQUAL("2011-05-16 06:00:00Z", * t);
}

BOOST_FIXTURE_TEST_CASE(manyReferenceTimes, Fixture)
{
	std::istringstream config(
			"[wci]\n"
			"read.referencetime = 2011-05-15 06:00:00Z\n"
			"read.referencetime = 2011-05-16 06:00:00Z\n");

	BOOST_CHECK_THROW(WdbConfiguration conf(config), std::runtime_error);
}

BOOST_FIXTURE_TEST_CASE(valueParameters, Fixture)
{
	std::istringstream config(
			"wci.read.valueparameter = air temperature\n"
			"wci.read.valueparameter = lwe precipitation rate\n"
			"wci.read.valueparameter = cloud area fraction\n");
	WdbConfiguration conf(config);

	const std::set<std::string> * params = conf.query().parameter();
	BOOST_REQUIRE(params);
	BOOST_CHECK_EQUAL(3, params->size());
	BOOST_CHECK(params->find("air temperature") != params->end());
	BOOST_CHECK(params->find("lwe precipitation rate") != params->end());
	BOOST_CHECK(params->find("cloud area fraction") != params->end());
}

BOOST_FIXTURE_TEST_CASE(dataVersions, Fixture)
{
	std::istringstream config(
			"wci.read.dataversion = -3\n"
			"wci.read.dataversion = 0\n"
			"wci.read.dataversion = 14\n");
	WdbConfiguration conf(config);

	const std::set<int> * v = conf.query().dataVersion();
	BOOST_REQUIRE(v);
	BOOST_CHECK_EQUAL(3, v->size());
	BOOST_CHECK(v->find(-3) != v->end());
	BOOST_CHECK(v->find(0) != v->end());
	BOOST_CHECK(v->find(14) != v->end());
}

BOOST_FIXTURE_TEST_CASE(includeConfigFiles, Fixture)
{
	std::istringstream config(
			"user = me\n"
			"include = "TEST_DIR"/wdb_test.conf\n");

	WdbConfiguration conf(config);

	BOOST_CHECK_EQUAL("testuser", conf.wciUser());
}

BOOST_FIXTURE_TEST_CASE(includeNonExistingConfigFiles, Fixture)
{
	std::istringstream config(
			"user = me\n"
			"include = no_such_file.conf\n");

	BOOST_CHECK_THROW(WdbConfiguration conf(config), std::runtime_error);
}

BOOST_FIXTURE_TEST_CASE(includeDirectory, Fixture)
{
	std::istringstream config(
			"user = me\n"
			"include = .\n");

	BOOST_CHECK_THROW(WdbConfiguration conf(config), std::runtime_error);
}


BOOST_AUTO_TEST_SUITE_END()

#endif
