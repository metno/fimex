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

#include "fimex/config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#include <iostream>
#include <fstream>
#include <boost/shared_ptr.hpp>
#include "fimex/FeltCDMReader.h"
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
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
	NetCDF_CDMWriter(feltReader, "test.nc");
	string testFile2("/disk1/opdata/hirlam20/grdn06.dat");
	ifstream grdFile(testFile2.c_str());
	if (grdFile.is_open()) {
		grdFile.close();
		feltReader = boost::shared_ptr<CDMReader>(new FeltCDMReader(testFile2, topSrcDir+"/share/etc/felt2nc_variables_hirlam20.xml"));
		NetCDF_CDMWriter(feltReader, "test2.nc");
	}
}

BOOST_AUTO_TEST_CASE( test_feltNetcdfWriteConfig )
{
	string topSrcDir(TOP_SRCDIR);
	string fileName(topSrcDir+"/test/flth00.dat");
	if (!ifstream(fileName.c_str())) {
		// no testfile, skip test
		return;
	}
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
	NetCDF_CDMWriter writer(feltReader, "test.nc", topSrcDir+"/share/etc/cdmWriterConfig.xml");
	BOOST_CHECK(writer.getVariableName("sea_level_pressure") == "sea_pressure");
	BOOST_CHECK(writer.getDimensionName("x") == "x_c");
	BOOST_CHECK(writer.getVariableName("x") == "x_c");
	BOOST_CHECK(writer.getAttribute(CDM::globalAttributeNS(), "minimum_time").getName() == "minimum_time");
	BOOST_CHECK(writer.getAttribute("air_temperature", "standard_name").getStringValue() == "temperature");
	bool exceptionThrown = false;
	try {
		writer.getAttribute(CDM::globalAttributeNS(), "comment");
	} catch (CDMException& ex) {
		exceptionThrown = true;
	}
	BOOST_CHECK(exceptionThrown);

	exceptionThrown = false;
	try {
		writer.getAttribute("surface_snow_sickness", "long_name");
	} catch (CDMException& ex) {
		exceptionThrown = true;
	}
	BOOST_CHECK(exceptionThrown);

	exceptionThrown = false;
	try {
		writer.getVariableName("land_ice_area_fraction");
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
