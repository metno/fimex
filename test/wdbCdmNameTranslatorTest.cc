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
#include <fimex/CDM.h>
#include <iostream>
#include <algorithm>
#include <boost/assign.hpp>


#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#define BOOST_TEST_MODULE WDB Interface Test
#include <boost/test/unit_test.hpp>

using namespace MetNoFimex;


BOOST_AUTO_TEST_SUITE(CdmNameTranslatorTest)

BOOST_AUTO_TEST_CASE(testSize)
{
    wdb::CdmNameTranslator translator;
    BOOST_CHECK_EQUAL(true, translator.isEmpty());
    BOOST_CHECK_EQUAL(0, translator.size());

    translator.addNamePair("air temperature", "temperature");
    translator.addNamePair("air pressure", "pressure");
    BOOST_CHECK_EQUAL(2, translator.size());

    translator.addNamePair("geopotential height", "height");
    BOOST_CHECK_EQUAL(3, translator.size());

    translator.addNamePair("longitude", "longitutde");
    translator.addNamePair("latitude", "latitude");
    BOOST_CHECK_EQUAL(5, translator.size());
    BOOST_CHECK_EQUAL(false, translator.isEmpty());

    translator.removeWdbName("geopotential height");
    translator.removeCdmName("temperature");
    BOOST_CHECK_EQUAL(3, translator.size());

    translator.addNamePair("x", "projection_x_coordinate");
    translator.addNamePair("y", "projection_y_coordinate");
    BOOST_CHECK_EQUAL(5, translator.size());

    translator.clear();
    BOOST_CHECK_EQUAL(true, translator.isEmpty());
    BOOST_CHECK_EQUAL(0, translator.size());
}

BOOST_AUTO_TEST_CASE(testHasNames)
{
    wdb::CdmNameTranslator translator;
    BOOST_CHECK_EQUAL(true, translator.isEmpty());

    translator.addNamePair("air temperature", "temperature");
    translator.addNamePair("air pressure", "pressure");
    translator.addNamePair("geopotential height", "height");
    translator.addNamePair("longitude", "longitude");
    translator.addNamePair("latitude", "latitude");
    translator.addNamePair("x", "projection_x_coordinate");
    translator.addNamePair("y", "projection_y_coordinate");

    BOOST_CHECK_EQUAL(7, translator.size());

    BOOST_CHECK_EQUAL(true, translator.hasCdmName("temperature"));
    BOOST_CHECK_EQUAL(true, translator.hasWdbName("air temperature"));

    BOOST_CHECK_EQUAL(true, translator.hasCdmName("projection_x_coordinate"));
    BOOST_CHECK_EQUAL(true, translator.hasWdbName("x"));

    translator.removeCdmName("temperature");
    BOOST_CHECK_EQUAL(false, translator.hasCdmName("temperature"));
    BOOST_CHECK_EQUAL(false, translator.hasWdbName("air temperature"));

    translator.removeWdbName("x");
    BOOST_CHECK_EQUAL(false, translator.hasCdmName("projection_x_coordinate"));
    BOOST_CHECK_EQUAL(false, translator.hasWdbName("x"));

    BOOST_CHECK_EQUAL(false, translator.isEmpty());
}

BOOST_AUTO_TEST_CASE(testTranslation)
{
    wdb::CdmNameTranslator translator;
    BOOST_CHECK_EQUAL(true, translator.isEmpty());

    translator.addNamePair("air temperature", "temperature");
    translator.addNamePair("air pressure", "pressure");
    translator.addNamePair("geopotential height", "height");
    translator.addNamePair("longitude", "longitude");
    translator.addNamePair("latitude", "latitude");
    translator.addNamePair("x", "projection_x_coordinate");
    translator.addNamePair("y", "projection_y_coordinate");

    BOOST_CHECK_EQUAL(7, translator.size());

    BOOST_CHECK_EQUAL("longitutde", translator.toCdmName("longitutde"));
    BOOST_CHECK_EQUAL("longitutde", translator.toWdbName("longitutde"));

    BOOST_CHECK_EQUAL("temperature", translator.toCdmName("air temperature"));
    BOOST_CHECK_EQUAL("geopotential height", translator.toWdbName("height"));

    BOOST_CHECK_EQUAL("not_in_translator", translator.toCdmName("not in translator"));
    BOOST_CHECK_EQUAL("not in translator", translator.toWdbName("not_in_translator"));

    BOOST_CHECK_EQUAL("notintranslator", translator.toCdmName("notintranslator"));
    BOOST_CHECK_EQUAL("notintranslator", translator.toWdbName("notintranslator"));

    BOOST_CHECK_EQUAL(false, translator.isEmpty());
}

BOOST_AUTO_TEST_CASE(testExceptions)
{
    wdb::CdmNameTranslator translator;
    BOOST_CHECK_EQUAL(true, translator.isEmpty());

    translator.addNamePair("air temperature", "temperature");
    translator.addNamePair("air pressure", "pressure");
    translator.addNamePair("geopotential height", "height");
    translator.addNamePair("longitude", "longitude");
    translator.addNamePair("latitude", "latitude");
    translator.addNamePair("x", "projection_x_coordinate");
    translator.addNamePair("y", "projection_y_coordinate");

    BOOST_REQUIRE_EQUAL(7, translator.size());

    BOOST_REQUIRE_THROW(translator.addNamePair("air pressure", "pressure"), wdb::CdmNameTranslatorException);
    BOOST_REQUIRE_THROW(translator.addNamePair("x", "projection_x_coordinate"), wdb::CdmNameTranslatorException);

    translator.removeWdbName("air pressure");
    BOOST_REQUIRE_EQUAL(6, translator.size());

    translator.removeCdmName("projection_x_coordinate");
    BOOST_REQUIRE_EQUAL(5, translator.size());

    BOOST_REQUIRE_NO_THROW(translator.addNamePair("air pressure", "pressure"));
    BOOST_REQUIRE_NO_THROW(translator.addNamePair("x", "projection_x_coordinate"));

    BOOST_REQUIRE_EQUAL(7, translator.size());
}

BOOST_AUTO_TEST_CASE(testXmlRead)
{
    wdb::CdmNameTranslator translator;
    BOOST_CHECK_EQUAL(true, translator.isEmpty());

    translator.readXML(TEST_DIR"/wdbreadercfg.xml");
    BOOST_CHECK_EQUAL(false, translator.isEmpty());

    BOOST_REQUIRE_EQUAL(6, translator.size());

    BOOST_CHECK_EQUAL(true, translator.hasWdbName("height above ground distance"));
    BOOST_CHECK_EQUAL(true, translator.hasWdbName("height above mean sea level distance"));

    BOOST_CHECK_EQUAL(true, translator.hasCdmName("height"));
    BOOST_CHECK_EQUAL(true, translator.hasCdmName("temperature"));

    BOOST_CHECK_EQUAL(false, translator.hasCdmName("precipitation"));

    BOOST_CHECK_EQUAL("temperature", translator.toCdmName("air_temperature"));
    BOOST_CHECK_EQUAL("relative_humidity", translator.toCdmName("relative_humidity"));
    BOOST_CHECK_EQUAL("height above mean sea level distance", translator.toWdbName("GeoZ"));
}

BOOST_AUTO_TEST_SUITE_END()

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif

