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

    explicit ConfigurationTest(const std::string & init) :
        config_(init)
    {}

    /// Get correct database connecion string, provided that the zero-argument constructor was called.
    std::string connectString() const
    {
        return "dbname=wdb host=localhost port=5432 user=vegardb";
    }

    /// Get correct query specification, provided that the zero-argument constructor was called.
    WciReadQuerySpecification expected() const
    {
        std::set<std::string> dataProvider = boost::assign::list_of("met.no eceps modification");
        std::string referenceTime = "2011-05-12 02:00:00+02";
        std::set<int> dataVersion = boost::assign::list_of(0);

        return WciReadQuerySpecification(& dataProvider, 0, & referenceTime, 0, 0, & dataVersion);
    }

    const WdbConfiguration & config() const
    {
        return config_;
    }
};

BOOST_FIXTURE_TEST_CASE(testFileInitialization, ConfigurationTest)
{
    BOOST_CHECK_EQUAL(connectString(), config().pqDatabaseConnectString());
    BOOST_CHECK_EQUAL("vegardb", config().wciUser());
    BOOST_CHECK_EQUAL(expected(), config().query());
}


BOOST_FIXTURE_TEST_CASE(indirectSpecifictionOfConfigFile, ConfigurationTest)
{
    WdbConfiguration config("file="TEST_DIR"/local.wdb.xml");
    BOOST_CHECK_EQUAL("dbname=wdb host=localhost port=5432 user=vegardb", config.pqDatabaseConnectString());
    BOOST_CHECK_EQUAL("vegardb", config.wciUser());
    BOOST_CHECK_EQUAL(expected(), config.query());
}

BOOST_FIXTURE_TEST_CASE(dataProviderAdditionToConfigFile, ConfigurationTest)
{
    WdbConfiguration config("file="TEST_DIR"/local.wdb.xml;dataprovider=someone");
    BOOST_CHECK_EQUAL("dbname=wdb host=localhost port=5432 user=vegardb", config.pqDatabaseConnectString());
    BOOST_CHECK_EQUAL("vegardb", config.wciUser());

    WciReadQuerySpecification spec = expected();
    spec.addDataProvider("someone");
    BOOST_CHECK_EQUAL(spec, config.query());
}

BOOST_FIXTURE_TEST_CASE(removeDataProviderFromConfigFile, ConfigurationTest)
{
    WdbConfiguration config("file="TEST_DIR"/local.wdb.xml;dataprovider=-;dataprovider=someone");

    WciReadQuerySpecification s = expected();
    WciReadQuerySpecification spec(0, s.location(), s.referenceTime(), s.validTime(), s.parameter(), s.dataVersion());
    spec.addDataProvider("someone");
    BOOST_CHECK_EQUAL(spec, config.query());
}

BOOST_FIXTURE_TEST_CASE(locationInExtraSpec, ConfigurationTest)
{
    WdbConfiguration config("dbname=foo;user=bar;location=somewhere");
    WciReadQuerySpecification spec;
    spec.setLocation("somewhere");
    BOOST_CHECK_EQUAL(spec, config.query());
}

BOOST_FIXTURE_TEST_CASE(referencetimeInExtraSpec, ConfigurationTest)
{
    WdbConfiguration config("dbname=foo;user=bar;referencetime=sometime");
    WciReadQuerySpecification spec;
    spec.setReferenceTime("sometime");
    BOOST_CHECK_EQUAL(spec, config.query());
}

BOOST_FIXTURE_TEST_CASE(validtimeInExtraSpec, ConfigurationTest)
{
    WdbConfiguration config("dbname=foo;user=bar;validtime=sometime");
    WciReadQuerySpecification spec;
    spec.setValidTime("sometime");
    BOOST_CHECK_EQUAL(spec, config.query());
}

BOOST_FIXTURE_TEST_CASE(parameterInExtraSpec, ConfigurationTest)
{
    WdbConfiguration config("dbname=foo;user=bar;;parameter=foo;parameter=-;parameter=bar;parameter=baz");
    WciReadQuerySpecification spec;
    spec.addParameter("bar");
    spec.addParameter("baz");
    BOOST_CHECK_EQUAL(spec, config.query());
}

BOOST_FIXTURE_TEST_CASE(dataVersionAdditionToConfigFile, ConfigurationTest)
{
    WdbConfiguration config("file="TEST_DIR"/local.wdb.xml;dataversion=34");

    WciReadQuerySpecification spec = expected();
    spec.addDataVersion(34);
    BOOST_CHECK_EQUAL(spec, config.query());
}

BOOST_FIXTURE_TEST_CASE(removeDataVersionFromConfigFile, ConfigurationTest)
{
    WdbConfiguration config("file="TEST_DIR"/local.wdb.xml;dataversion=-;dataversion=4");

    WciReadQuerySpecification s = expected();
    WciReadQuerySpecification spec(s.dataProvider(), s.location(), s.referenceTime(), s.validTime(), s.parameter(), 0);
    spec.addDataVersion(4);
    BOOST_CHECK_EQUAL(spec, config.query());
}

BOOST_FIXTURE_TEST_CASE(notIntegerInDataVersion, ConfigurationTest)
{
    BOOST_CHECK_THROW(
            WdbConfiguration("file="TEST_DIR"/local.wdb.xml;dataversion=q"),
            std::runtime_error
    );
}

BOOST_FIXTURE_TEST_CASE(databaseNameInExtraSpec, ConfigurationTest)
{
    WdbConfiguration config("dbname=wdb;user=someone");

    BOOST_CHECK_EQUAL("dbname=wdb port=5432 user=someone", config.pqDatabaseConnectString());
}

BOOST_FIXTURE_TEST_CASE(portInExtraSpec, ConfigurationTest)
{
    WdbConfiguration config("dbname=wdb1;port=1234;user=someoneelse");

    BOOST_CHECK_EQUAL("dbname=wdb1 port=1234 user=someoneelse", config.pqDatabaseConnectString());
}

BOOST_FIXTURE_TEST_CASE(rubbishValueForPortInExtraSpec, ConfigurationTest)
{
    BOOST_CHECK_THROW(
            WdbConfiguration("dbname=wdb1;port=foo;user=someoneelse"),
            std::runtime_error
    );
}

BOOST_FIXTURE_TEST_CASE(hostInExtraSpec, ConfigurationTest)
{
    WdbConfiguration config("dbname=wdb1;host=some.where.com;port=1234;user=someoneelse");

    BOOST_CHECK_EQUAL("dbname=wdb1 host=some.where.com port=1234 user=someoneelse", config.pqDatabaseConnectString());
}

BOOST_FIXTURE_TEST_CASE(wciUserInExtraSpec, ConfigurationTest)
{
    WdbConfiguration config("dbname=wdb;user=someone;wciUser=someoneelse");

    BOOST_CHECK_EQUAL("dbname=wdb port=5432 user=someone", config.pqDatabaseConnectString());
    BOOST_CHECK_EQUAL("someoneelse", config.wciUser());
}

BOOST_FIXTURE_TEST_CASE(missingValueInExtraSpec, ConfigurationTest)
{
    BOOST_CHECK_THROW(
            WdbConfiguration("file="TEST_DIR"/local.wdb.xml;dataversion"),
            std::runtime_error
    );
}

BOOST_FIXTURE_TEST_CASE(rubbishInExtraSpec, ConfigurationTest)
{
    BOOST_CHECK_THROW(
            WdbConfiguration("file="TEST_DIR"/local.wdb.xml;qwerty#!$"),
            std::runtime_error
    );
}

BOOST_FIXTURE_TEST_CASE(emptyContentBetweenSeparators, ConfigurationTest)
{
    BOOST_CHECK_NO_THROW(
            WdbConfiguration("file="TEST_DIR"/local.wdb.xml;;;;dataprovider=af")
    );
}

BOOST_FIXTURE_TEST_CASE(specEndingWithSeparator, ConfigurationTest)
{
    BOOST_CHECK_NO_THROW(
            WdbConfiguration("file="TEST_DIR"/local.wdb.xml;dataprovider=af:")
    );
}



#endif
