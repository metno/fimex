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

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>

#include "fimex/FeltCDMReader.h"
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/interpolation.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE(test_interpolator)
{
	string topSrcDir(TOP_SRCDIR);
	string fileName(topSrcDir+"/test/flth00.dat");
	if (!ifstream(fileName.c_str())) {
		// no testfile, skip test
		return;
	}
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
	boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(feltReader));
	vector<double> xAxis, yAxis;
	for (int i = -100; i < 10; i++) {
		xAxis.push_back(i * 50000);
		yAxis.push_back(i * 50000);
	}
	interpolator->changeProjection(MIFI_BILINEAR, "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +elips=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m");
	//interpolator->getCDM().toXMLStream(cerr);
	BOOST_CHECK(true);

	NetCDF_CDMWriter(interpolator, "testInterpolator.nc");
	BOOST_CHECK(true);
}
BOOST_AUTO_TEST_CASE(test_interpolator2)
{
	string topSrcDir(TOP_SRCDIR);
	string grdFile("/disk1/opdata/hirlam20/grdn06.dat");
	ifstream inputFile(grdFile.c_str());
	if (inputFile.is_open()) {
		inputFile.close();
		boost::shared_ptr<CDMReader> feltReader = boost::shared_ptr<CDMReader>(new FeltCDMReader(grdFile, topSrcDir+"/share/etc/felt2nc_variables_hirlam20.xml"));
		boost::shared_ptr<CDMInterpolator> interpolator = boost::shared_ptr<CDMInterpolator>(new CDMInterpolator(feltReader));
		vector<double> xAxis, yAxis;
		xAxis = vector<double>();
		yAxis = vector<double>();
		for (int i = -114; i < 114; i++) {
			xAxis.push_back(i * 50000);
		}
		for (int i = -147; i < 48; i++) {
			yAxis.push_back(i * 50000);
		}
		interpolator->changeProjection(MIFI_BILINEAR, "+proj=stere +lat_0=90 +lon_0=0 +lat_ts=60 +elips=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m");
		BOOST_CHECK(true);
		NetCDF_CDMWriter(interpolator, "testInterpolator2.nc");
		BOOST_CHECK(true);
	}
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
