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

#include "fimex_config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#include <iostream>
#include <fstream>
#include <boost/shared_ptr.hpp>
#include "FeltCDMReader2.h"

#include "fimex/NetCDF_CDMWriter.h"

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

using namespace std;
using namespace MetNoFelt;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_feltNetcdfWrite )
{
	string topSrcDir(TOP_SRCDIR);
	string fileName(topSrcDir+"/test/flth00.dat");
	if (!ifstream(fileName.c_str())) {
		// no testfile, skip test
		return;
	}
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader2(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
	NetCDF_CDMWriter(feltReader, "test_feltNetcdfWrite.nc");
}

BOOST_AUTO_TEST_CASE( test_feltNetcdfWriteConfig )
{
	string topSrcDir(TOP_SRCDIR);
	string fileName(topSrcDir+"/test/flth00.dat");
	if (!ifstream(fileName.c_str())) {
		// no testfile, skip test
		return;
	}
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader2(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
	NetCDF_CDMWriter writer(feltReader, "test_feltNetcdfWriteConfig.nc", topSrcDir+"/share/etc/cdmWriterConfigDeprecated.xml");
	BOOST_CHECK(writer.getVariableName("sea_level_pressure") == "sea_pressure");
	BOOST_CHECK(writer.getDimensionName("x") == "x_c");
	BOOST_CHECK(writer.getVariableName("x") == "x_c");
	BOOST_CHECK(writer.getAttribute("air_temperature", "standard_name").getStringValue() == "temperature");
	bool exceptionThrown = false;
	try {
	    writer.getAttribute(CDM::globalAttributeNS(), "comment");
	} catch (exception& ex) {
		exceptionThrown = true;
	}
	BOOST_CHECK(exceptionThrown == true);

	exceptionThrown = false;
	try {
		writer.getAttribute("surface_snow_thickness", "long_name");
	} catch (CDMException& ex) {
		exceptionThrown = true;
	}
	BOOST_CHECK(exceptionThrown);

}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
