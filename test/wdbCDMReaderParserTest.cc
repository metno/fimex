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
#include <fimex/WdbCDMReaderParser.h>

// std
//
#include <iostream>

#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#include <boost/test/unit_test.hpp>

#define BOOST_TEST_MODULE WDB Interface Test


using namespace MetNoFimex;


BOOST_AUTO_TEST_SUITE(WdbCDMReaderParserTest)

BOOST_AUTO_TEST_CASE(testParseOnlySource)
{
    std::string cfgFileName; // empty
    std::string source("dbHost=proffdb-devel.met.no;dbName=wdb;dbUser=wdb;wciUser=proffread;dbPort=5432;refTime=20110209T120000");

    WdbCDMReaderParser parser;
    WdbCDMReaderParserInfo wdbInfo;

    wdbInfo =  parser.parse(source, cfgFileName, false);

    // add provider and place

    std::string newSource("dbHost=proffdb-devel.met.no;dbName=wdb;dbUser=wdb;wciUser=proffread;dbPort=5432;refTime=20110209T120000;provider=snoeskred;place=snoeskred grid");

    wdbInfo =  parser.parse(newSource, cfgFileName, false);
    BOOST_REQUIRE_EQUAL(wdbInfo.provider(), "snoeskred");
    BOOST_REQUIRE_EQUAL(wdbInfo.place(), "snoeskred grid");

    wdbInfo =  parser.parse(newSource, cfgFileName);
    BOOST_REQUIRE_EQUAL(wdbInfo.provider(), "snoeskred");
    BOOST_REQUIRE_EQUAL(wdbInfo.place(), "snoeskred grid");

    // shuffle data
    std::string newerSource("dbHost=proffdb-devel.met.no;Name=wdb;dbName=wdb;dbUser=wdb;place=snoeskred grid;wciUser=proffread;dbPort=5432;refTime=20110209T120000;provider=snoeskred;");

    wdbInfo =  parser.parse(newerSource, cfgFileName);
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbHost(), "proffdb-devel.met.no");
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbName(), "wdb");
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbUser(), "wdb");
    BOOST_REQUIRE_EQUAL(wdbInfo.wciUser(), "proffread");
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbPort(), 5432);
    BOOST_REQUIRE_EQUAL(wdbInfo.provider(), "snoeskred");
    BOOST_REQUIRE_EQUAL(wdbInfo.place(), "snoeskred grid");
    BOOST_REQUIRE_EQUAL(wdbInfo.referenceTime(), "20110209T120000");

    // empty source
    std::string emptySource("");

    wdbInfo =  parser.parse(emptySource, cfgFileName);
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbHost(), "");
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbName(), "");
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbUser(), "");
    BOOST_REQUIRE_EQUAL(wdbInfo.wciUser(), "");

    // by default
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbPort(), 5432);

    BOOST_REQUIRE_EQUAL(wdbInfo.provider(), "");
    BOOST_REQUIRE_EQUAL(wdbInfo.place(), "");
}

BOOST_AUTO_TEST_CASE(testConfigFileOnly)
{
    std::string cfgFileName(TEST_DIR"/wdbreadercfg.xml");
    std::string source; // empty

    WdbCDMReaderParser parser;
    WdbCDMReaderParserInfo wdbInfo;

    wdbInfo =  parser.parse(source, cfgFileName);

    BOOST_REQUIRE_EQUAL(wdbInfo.wdbHost(), "proffdb-devel.met.no");
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbName(), "wdb");
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbUser(), "wdb");
    BOOST_REQUIRE_EQUAL(wdbInfo.wciUser(), "proffwrite");
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbPort(), 5432);
    BOOST_REQUIRE_EQUAL(wdbInfo.provider(), "met.no");
    BOOST_REQUIRE_EQUAL(wdbInfo.place(), "norge grid");
    BOOST_REQUIRE_EQUAL(wdbInfo.referenceTime(), "20110210T000000");
}


BOOST_AUTO_TEST_CASE(testSourceAndConfigFile)
{
    std::string cfgFileName(TEST_DIR"/wdbreadercfg.xml");
    std::string source("dbHost=proffdb-devel.met.no;wciUser=proffread;refTime=20110209T120000;provider=snoeskred");

    /**
      * source will override config file
      */
    WdbCDMReaderParser parser;
    WdbCDMReaderParserInfo wdbInfo;

    wdbInfo =  parser.parse(source, cfgFileName);

    BOOST_REQUIRE_EQUAL(wdbInfo.wdbHost(), "proffdb-devel.met.no");
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbName(), "wdb");
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbUser(), "wdb");
    BOOST_REQUIRE_EQUAL(wdbInfo.wciUser(), "proffread");
    BOOST_REQUIRE_EQUAL(wdbInfo.wdbPort(), 5432);
    BOOST_REQUIRE_EQUAL(wdbInfo.provider(), "snoeskred");
    BOOST_REQUIRE_EQUAL(wdbInfo.place(), "norge grid");
    BOOST_REQUIRE_EQUAL(wdbInfo.referenceTime(), "20110209T120000");
}


BOOST_AUTO_TEST_SUITE_END()

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
