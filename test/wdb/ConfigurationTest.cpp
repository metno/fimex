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
#include <boost/assign/list_of.hpp>
#include <wdb/config/WdbConfiguration.h>


using namespace MetNoFimex::wdb;

class ConfigurationTest
{
	WdbConfiguration config_;
public:
	ConfigurationTest() :
		config_(TEST_DIR"/local.wdb.xml")
	{}

	const WdbConfiguration & config() const
	{
		return config_;
	}
};

BOOST_FIXTURE_TEST_CASE(testConnectionSpecification, ConfigurationTest)
{
	BOOST_CHECK_EQUAL("dbname=wdb host=localhost port=5432 user=vegardb", config().pqDatabaseConnectString());
}

BOOST_FIXTURE_TEST_CASE(testImplicitWciUser, ConfigurationTest)
{
	BOOST_CHECK_EQUAL("vegardb", config().wciUser());
}

BOOST_FIXTURE_TEST_CASE(testReadQuery, ConfigurationTest)
{
	std::set<std::string> dataProvider = boost::assign::list_of("met.no eceps modification");
	std::string referenceTime = "2011-05-12 02:00:00+02";
	std::set<int> dataVersion = boost::assign::list_of(0);

	WciReadQuerySpecification expected(& dataProvider, 0, & referenceTime, 0, 0, & dataVersion);

	BOOST_CHECK_EQUAL(expected, config().query());
}


#endif
