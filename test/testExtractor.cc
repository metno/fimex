/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>
#ifdef HAVE_LIBMIC
#include "fimex/FeltCDMReader.h"
#else
#include "fimex/FeltCDMReader2.h"
#endif
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/CDMExtractor.h"
#include "fimex/Logger.h"
#include "fimex/Data.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_extract )
{
    //defaultLogLevel(Logger::DEBUG);
	string topSrcDir(TOP_SRCDIR);
	string fileName(topSrcDir+"/test/flth00.dat");
	if (!ifstream(fileName.c_str())) {
		// no testfile, skip test
		return;
	}
#ifdef HAVE_LIBMIC
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader(fileName, topSrcDir + "/share/etc/felt2nc_variables.xml"));
#else
    boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader2(fileName, topSrcDir + "/share/etc/felt2nc_variables.xml"));
#endif
	boost::shared_ptr<CDMExtractor> extract(new CDMExtractor(feltReader));
	extract->removeVariable("relative_humidity");
	try {
		extract->getCDM().getVariable("relative_humidity");
		BOOST_CHECK(false);
	} catch (...) {
		BOOST_CHECK(true);
	}

	extract->reduceDimension("y", 10, 50);
	extract->reduceDimension("x", 80, 50); // spain
	FimexTime startTime(2007,5,16, 9);
    FimexTime endTime(2007,5,16, 20);
	extract->reduceTime(startTime, endTime); // 12 hours 9..20
	BOOST_CHECK(extract->getData("y")->size() == 50);
    BOOST_CHECK(extract->getData("x")->size() == 50);
    BOOST_CHECK(extract->getData("time")->size() == 12);
    BOOST_CHECK(extract->getData("altitude")->size() == 50*50);
    BOOST_CHECK(extract->getData("precipitation_amount")->size() == 50*50*12);
	NetCDF_CDMWriter(extract, "test3.nc");
	BOOST_CHECK(true);

	extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
	extract->reduceTime(FimexTime(FimexTime::min_date_time), FimexTime(FimexTime::max_date_time));
    BOOST_CHECK(extract->getData("time")->size() == 61);

    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    // reduce sigma from 4 to 2
    extract->reduceVerticalAxis("", .5, .85);
    BOOST_CHECK(extract->getData("sigma")->size() == 2);

    // reduce time to 0 and sigma to 0
    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    extract->reduceVerticalAxis("", -0.1, -0.05);
    extract->reduceTime(FimexTime(2006,1,1), FimexTime(2006,1,2)); // time out of range
    BOOST_CHECK(extract->getData("time")->size() == 0);
    NetCDF_CDMWriter(extract, "test_0time.nc");
    BOOST_CHECK(true);

    // test selectVariable
    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    std::set<std::string> variables;
    variables.insert("time");
    variables.insert("altitude");
    variables.insert("relative_humidity");
    extract->selectVariables(variables);
    BOOST_CHECK(extract->getCDM().hasVariable("time"));
    BOOST_CHECK(extract->getCDM().hasVariable("altitude"));
    BOOST_CHECK(extract->getCDM().hasVariable("relative_humidity"));
    BOOST_CHECK(false == extract->getCDM().hasVariable("precipitation_amount"));

    // test selection of non existing selectVariable
    extract = boost::shared_ptr<CDMExtractor>(new CDMExtractor(feltReader));
    variables.insert("not_there");
    extract->selectVariables(variables);
    BOOST_CHECK(extract->getCDM().hasVariable("relative_humidity"));
    BOOST_CHECK(false == extract->getCDM().hasVariable("precipitation_amount"));


}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
