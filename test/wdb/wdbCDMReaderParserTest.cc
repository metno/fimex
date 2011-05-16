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

#include <fimex/CDM.h>
#include <fimex/CDMException.h>
#include <wdb/WdbCDMReaderParser.h>

#include <boost/date_time/posix_time/posix_time.hpp>

// std
//
#include <iostream>

#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#include <boost/test/unit_test.hpp>

#define BOOST_TEST_MODULE WDB Interface Test


using namespace MetNoFimex::wdb;


BOOST_AUTO_TEST_SUITE(WdbCDMReaderParserTest)

class Fixture
{
public:
    WdbCDMReaderParserInfo wdbInfo;

    void parseSource(const std::string & source)
    {
		WdbCDMReaderParser parser;
        wdbInfo = parser.parse(source, std::string());
    }

    void parseConfig(const std::string & relativeConfigFileName)
    {
    	WdbCDMReaderParser parser;
        std::string cfgFileName = TEST_DIR "/" + relativeConfigFileName;
        wdbInfo = parser.parse(std::string(), cfgFileName);
    }

    void parse(const std::string & source, const std::string & relativeConfigFileName)
    {
    	WdbCDMReaderParser parser;
        std::string cfgFileName = TEST_DIR "/" + relativeConfigFileName;
        wdbInfo = parser.parse(source, cfgFileName);

    }
};


BOOST_FIXTURE_TEST_CASE(parseOnlySource, Fixture)
{
	parseSource("dbHost=localhost;dbPort=5432;dbName=wdb;dbUser=wdb");

    BOOST_CHECK_EQUAL(wdbInfo.wdbHost(), "localhost");
    BOOST_CHECK_EQUAL(wdbInfo.wdbName(), "wdb");
    BOOST_CHECK_EQUAL(wdbInfo.wdbUser(), "wdb");
    BOOST_CHECK_EQUAL(wdbInfo.wdbPort(), 5432);

    const WciReadQuerySpecification & qSpec = wdbInfo.getWciReadQuerySpecification();
    BOOST_CHECK(! qSpec.dataProvider());
    BOOST_CHECK(! qSpec.location());
    BOOST_CHECK(! qSpec.referenceTime());
    BOOST_CHECK(! qSpec.parameter());
    BOOST_CHECK(! qSpec.dataVersion());
}

BOOST_FIXTURE_TEST_CASE(testParseQuerySpec, Fixture)
{
    // add provider and place

	std::string source =
			"refTime=20110209T120000;"
			"provider=snoeskred;"
			"place=snoeskred grid";

	parseSource(source);

    const WciReadQuerySpecification & qSpec = wdbInfo.getWciReadQuerySpecification();
    const std::set<std::string> * dataProvider = qSpec.dataProvider();
    BOOST_CHECK(dataProvider);
    if ( dataProvider )
    {
		BOOST_CHECK_EQUAL(1, dataProvider->size());
		BOOST_CHECK(dataProvider->find("snoeskred") != dataProvider->end());
    }

    const std::string * location = qSpec.location();
    BOOST_CHECK(location);
    if ( location )
    	BOOST_CHECK_EQUAL("snoeskred grid", * location);

    const std::string * referenceTime = qSpec.referenceTime();
    BOOST_CHECK(referenceTime);
    if ( referenceTime )
    	BOOST_CHECK_EQUAL("20110209T120000", * referenceTime);
}

BOOST_FIXTURE_TEST_CASE(testParseOnlySourceDataShuffled, Fixture)
{
    // shuffle data
	parseSource("dbHost=proffdb-devel.met.no;"
			"Name=wdb;"
			"dbName=wdb;"
			"dbUser=wdb;"
			"place=snoeskred grid;"
			"wciUser=proffread;"
			"dbPort=5432;"
			"refTime=20110209T120000;"
			"provider=snoeskred;");

	BOOST_CHECK_EQUAL(wdbInfo.wdbHost(), "proffdb-devel.met.no");
    BOOST_CHECK_EQUAL(wdbInfo.wdbName(), "wdb");
    BOOST_CHECK_EQUAL(wdbInfo.wdbUser(), "wdb");
    BOOST_CHECK_EQUAL(wdbInfo.wciUser(), "proffread");
    BOOST_CHECK_EQUAL(wdbInfo.wdbPort(), 5432);

    const WciReadQuerySpecification & qSpec = wdbInfo.getWciReadQuerySpecification();
    const std::set<std::string> * dataProvider = qSpec.dataProvider();
    BOOST_CHECK(dataProvider);
    if ( dataProvider )
    {
		BOOST_CHECK_EQUAL(1, dataProvider->size());
		BOOST_CHECK(dataProvider->find("snoeskred") != dataProvider->end());
    }

    const std::string * location = qSpec.location();
    BOOST_CHECK(location);
    if ( location )
    	BOOST_CHECK_EQUAL("snoeskred grid", * location);

    const std::string * referenceTime = qSpec.referenceTime();
    BOOST_CHECK(referenceTime);
	if ( referenceTime )
    	BOOST_CHECK_EQUAL("20110209T120000", * referenceTime);
}


BOOST_FIXTURE_TEST_CASE(emptySource, Fixture)
{
    // empty source
	parseSource("");

    BOOST_CHECK_EQUAL(wdbInfo.wdbHost(), "");
    BOOST_CHECK_EQUAL(wdbInfo.wdbName(), "");
    BOOST_CHECK_EQUAL(wdbInfo.wdbUser(), "");
    BOOST_CHECK_EQUAL(wdbInfo.wciUser(), "");

    // by default
    BOOST_CHECK_EQUAL(wdbInfo.wdbPort(), 5432);

    const WciReadQuerySpecification & qSpec = wdbInfo.getWciReadQuerySpecification();
    BOOST_CHECK(! qSpec.dataProvider());
    BOOST_CHECK(! qSpec.location());
    BOOST_CHECK(! qSpec.referenceTime());
    BOOST_CHECK(! qSpec.parameter());
    BOOST_CHECK(! qSpec.dataVersion());
}



BOOST_FIXTURE_TEST_CASE(testConfigFileOnly1, Fixture)
{
    parseConfig("wdbreadercfg.xml");

    BOOST_CHECK_EQUAL(wdbInfo.wdbHost(), "proffdb-devel.met.no");
    BOOST_CHECK_EQUAL(wdbInfo.wdbName(), "wdb");
    BOOST_CHECK_EQUAL(wdbInfo.wdbUser(), "wdb");
    BOOST_CHECK_EQUAL(wdbInfo.wciUser(), "proffwrite");
    BOOST_CHECK_EQUAL(wdbInfo.wdbPort(), 5432);


    const WciReadQuerySpecification & qSpec = wdbInfo.getWciReadQuerySpecification();
    const std::set<std::string> * dataProvider = qSpec.dataProvider();
    BOOST_CHECK(dataProvider);
    if ( dataProvider )
    {
		BOOST_CHECK_EQUAL(1, dataProvider->size());
		BOOST_CHECK(dataProvider->find("met.no") != dataProvider->end());
    }

    const std::string * location = qSpec.location();
    BOOST_CHECK(location);
    if ( location )
    	BOOST_CHECK_EQUAL("norge grid", * location);

    const std::string * referenceTime = qSpec.referenceTime();
    BOOST_CHECK(referenceTime);
    if ( referenceTime )
    	BOOST_CHECK_EQUAL("20110210T000000", * referenceTime);
}


BOOST_FIXTURE_TEST_CASE(testConfigFileOnly2, Fixture)
{
	parseConfig("local_wdb_config.xml");

    BOOST_CHECK_EQUAL(wdbInfo.wdbHost(), "localhost");
    BOOST_CHECK_EQUAL(wdbInfo.wdbName(), "wdb");
    BOOST_CHECK_EQUAL(wdbInfo.wdbUser(), "vegardb");
    BOOST_CHECK_EQUAL(wdbInfo.wciUser(), "vegardb");
    BOOST_CHECK_EQUAL(wdbInfo.wdbPort(), 5432);

    const WciReadQuerySpecification & qSpec = wdbInfo.getWciReadQuerySpecification();
    const std::set<std::string> * dataProvider = qSpec.dataProvider();
    BOOST_CHECK(dataProvider);
	if ( dataProvider )
    {
		BOOST_CHECK_EQUAL(1, dataProvider->size());
		BOOST_CHECK(dataProvider->find("met.no eceps modification") != dataProvider->end());
    }

    const std::string * location = qSpec.location();
    BOOST_CHECK(location);
    if ( location )
    	BOOST_CHECK_EQUAL("norway 025", * location);

    BOOST_CHECK(! qSpec.referenceTime());
}

BOOST_FIXTURE_TEST_CASE(manyDataProviders, Fixture)
{
	parseConfig("wdbreadercfg2.xml");

	const std::set<std::string> * dataProviders = wdbInfo.getWciReadQuerySpecification().dataProvider();

	BOOST_REQUIRE(dataProviders);

	BOOST_CHECK_EQUAL(2, dataProviders->size());
	BOOST_CHECK(dataProviders->find("provider 1") != dataProviders->end());
	BOOST_CHECK(dataProviders->find("provider 2") != dataProviders->end());
}

BOOST_FIXTURE_TEST_CASE(configFileWithoutContents, Fixture)
{
	// An exception here may mean that many other tests are invalid. If an
	// exception is thrown bacause a specific key is not found, many
	// specialized tests will pass - maybe falsely, because they expect their
	// parsing to fail.
	parseConfig("wdb_no_content.xml");
}

BOOST_FIXTURE_TEST_CASE(manyLocationsThrows, Fixture)
{
	BOOST_CHECK_THROW(parseConfig("wdb_many_locations.xml"), MetNoFimex::CDMException);
}

BOOST_FIXTURE_TEST_CASE(manyReferenceTimesThrows, Fixture)
{
	BOOST_CHECK_THROW(parseConfig("wdb_many_reference_times.xml"), MetNoFimex::CDMException);
}


BOOST_FIXTURE_TEST_CASE(manyParameters, Fixture)
{
	parseConfig("wdbreadercfg2.xml");

	const std::set<std::string> * parameters = wdbInfo.getWciReadQuerySpecification().parameter();

	BOOST_REQUIRE(parameters);

	BOOST_CHECK_EQUAL(3, parameters->size());
	BOOST_CHECK(parameters->find("air temperature") != parameters->end());
	BOOST_CHECK(parameters->find("air pressure") != parameters->end());
	BOOST_CHECK(parameters->find("cloud cover") != parameters->end());
}

BOOST_FIXTURE_TEST_CASE(manyDataVersions, Fixture)
{
	parseConfig("wdbreadercfg2.xml");

	const std::set<int> * versions = wdbInfo.getWciReadQuerySpecification().dataVersion();

	BOOST_REQUIRE(versions);

	BOOST_CHECK_EQUAL(4, versions->size());
	BOOST_CHECK(versions->find(0) != versions->end());
	BOOST_CHECK(versions->find(-1) != versions->end());
	BOOST_CHECK(versions->find(1) != versions->end());
	BOOST_CHECK(versions->find(2) != versions->end());
}




BOOST_FIXTURE_TEST_CASE(testSourceAndConfigFile, Fixture)
{
	// source should override config file
	parse("dbHost=proffdb-devel.met.no;wciUser=proffread;refTime=20110209T120000;provider=snoeskred", "wdbreadercfg.xml");

    BOOST_CHECK_EQUAL(wdbInfo.wdbHost(), "proffdb-devel.met.no");
    BOOST_CHECK_EQUAL(wdbInfo.wdbName(), "wdb");
    BOOST_CHECK_EQUAL(wdbInfo.wdbUser(), "wdb");
    BOOST_CHECK_EQUAL(wdbInfo.wciUser(), "proffread");
    BOOST_CHECK_EQUAL(wdbInfo.wdbPort(), 5432);

    const WciReadQuerySpecification & qSpec = wdbInfo.getWciReadQuerySpecification();
    const std::set<std::string> * dataProvider = qSpec.dataProvider();
    BOOST_CHECK(dataProvider);
    if ( dataProvider )
    {
		BOOST_CHECK_EQUAL(1, dataProvider->size());
		BOOST_CHECK(dataProvider->find("snoeskred") != dataProvider->end());
    }

    const std::string * location = qSpec.location();
    BOOST_CHECK(location);
    if ( location )
    	BOOST_CHECK_EQUAL("norge grid", * location);

    const std::string * referenceTime = qSpec.referenceTime();
    BOOST_CHECK(referenceTime);
    if ( referenceTime )
    	BOOST_CHECK_EQUAL("20110209T120000", * referenceTime);
}


BOOST_AUTO_TEST_SUITE_END()

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
