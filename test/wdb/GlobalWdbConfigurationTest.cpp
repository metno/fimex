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

#include <wdb/config/GlobalWdbConfiguration.h>
#include <fimex/CDMException.h>
#include <fimex/CDMAttribute.h>
#include <fimex/Data.h>
#include <boost/foreach.hpp>

using namespace MetNoFimex;
using namespace MetNoFimex::wdb;

BOOST_AUTO_TEST_SUITE(GlobalWdbConfigurationTest)

namespace
{
const CDMAttribute * getAttribute(const std::string & name, const std::vector<CDMAttribute> & list)
{
	const CDMAttribute * ret = 0;

	BOOST_FOREACH(const CDMAttribute & attr, list)
		if ( attr.getName() == name )
		{
			if ( ret )
				throw std::runtime_error("Error i check: multiple occurences of same attribute name");
			ret = & attr;
		}

	return ret;
}

#define CHECK_HAS_ATTRIBUTE(list, name, value) { \
	const std::string attributeName = name; \
	const CDMAttribute * attr = getAttribute(attributeName, globals); \
	BOOST_CHECK_MESSAGE(attr != 0, "No attribute " + attributeName); \
	if ( attr )	BOOST_CHECK_EQUAL(value, attr->getStringValue()); \
}

}



BOOST_AUTO_TEST_CASE(globalVariables)
{
        GlobalWdbConfiguration globalConfig(XMLInputFile(WDB_TEST_DIR "/wdb_config.xml"));

	const GlobalWdbConfiguration::AttributeList & globals = globalConfig.getGlobalAttributes();

	CHECK_HAS_ATTRIBUTE(globals, "Conventions", "CF-1.0");
    CHECK_HAS_ATTRIBUTE(globals, "institution", "Norwegian Meteorological Institute");
    CHECK_HAS_ATTRIBUTE(globals, "product_name", "WdbCDMReader");
    CHECK_HAS_ATTRIBUTE(globals, "project_name", "WdbCDMReader");
    CHECK_HAS_ATTRIBUTE(globals, "title", "Wdb data extraction via Fimex");
    CHECK_HAS_ATTRIBUTE(globals, "topiccategory", "ClimatologyMeteorologyAtmosphere");
    CHECK_HAS_ATTRIBUTE(globals, "contact", "fimex@lists.met.no");
    CHECK_HAS_ATTRIBUTE(globals, "distribution_statement", "Free");

}

BOOST_AUTO_TEST_CASE(implicitCfName)
{
        GlobalWdbConfiguration globalConfig(XMLInputFile(WDB_TEST_DIR "/wdb_config.xml"));

	BOOST_CHECK_EQUAL("air_temperature", globalConfig.cfName("air temperature"));
	BOOST_CHECK_EQUAL("x_wind", globalConfig.cfName("x wind"));
}

BOOST_AUTO_TEST_CASE(explicitCfName)
{
        GlobalWdbConfiguration globalConfig(XMLInputFile(WDB_TEST_DIR "/wdb_config.xml"));
	BOOST_CHECK_EQUAL("mslp", globalConfig.cfName("mean sea level pressure"));
}

BOOST_AUTO_TEST_CASE(getsAttributes)
{
        GlobalWdbConfiguration globalConfig(XMLInputFile(WDB_TEST_DIR "/wdb_config.xml"));
	std::vector<CDMAttribute> attributes = globalConfig.getAttributes("x wind");

	const CDMAttribute * units = getAttribute("units", attributes);
	BOOST_CHECK(units != 0);
	if ( units )
		BOOST_CHECK_EQUAL("m s-1", units->getStringValue());

	const CDMAttribute * standardName = getAttribute("standard_name", attributes);
	BOOST_CHECK(standardName != 0);
	if ( standardName )
		BOOST_CHECK_EQUAL("eastward_wind", standardName->getStringValue());
}

BOOST_AUTO_TEST_CASE(getsAttributes2)
{
        GlobalWdbConfiguration globalConfig(XMLInputFile(WDB_TEST_DIR "/wdb_config.xml"));
	std::vector<CDMAttribute> attributes = globalConfig.getAttributes("y wind");

	const CDMAttribute * units = getAttribute("units", attributes);
	BOOST_CHECK(units != 0);
	if ( units )
		BOOST_CHECK_EQUAL("m s-1", units->getStringValue());

	const CDMAttribute * standardName = getAttribute("standard_name", attributes);
	BOOST_CHECK(standardName != 0);
	if ( standardName )
		BOOST_CHECK_EQUAL("northward_wind", standardName->getStringValue());
}

BOOST_AUTO_TEST_CASE(implicitLongName)
{
        GlobalWdbConfiguration globalConfig(XMLInputFile(WDB_TEST_DIR "/wdb_config.xml"));
	std::vector<CDMAttribute> attributes = globalConfig.getAttributes("y wind");

	const CDMAttribute * longName = getAttribute("long_name", attributes);
	BOOST_CHECK(longName != 0);
	if ( longName )
		BOOST_CHECK_EQUAL("y wind", longName->getStringValue());
}

BOOST_AUTO_TEST_CASE(overrideDefaultValues)
{
        GlobalWdbConfiguration globalConfig(XMLInputFile(WDB_TEST_DIR "/wdb_config.xml"));
	std::vector<CDMAttribute> attributes = globalConfig.getAttributes("high cloud cover");

	const CDMAttribute * longName = getAttribute("long_name", attributes);
	BOOST_CHECK(longName != 0);
	if ( longName )
		BOOST_CHECK_EQUAL("stuff", longName->getStringValue());
}


BOOST_AUTO_TEST_SUITE_END()

#endif
