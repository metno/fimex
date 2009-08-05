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
#ifdef HAVE_LIBMIC
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
#else
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader2(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
#endif
	boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(feltReader));
	vector<double> xAxis, yAxis;
	for (int i = -100; i < 10; i++) {
		xAxis.push_back(i * 50000);
		yAxis.push_back(i * 50000);
	}
	interpolator->changeProjection(MIFI_INTERPOL_NEAREST_NEIGHBOR, "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m");
	//interpolator->getCDM().toXMLStream(cerr);
	BOOST_CHECK(true);
    boost::shared_ptr<Data> altitudeData = interpolator->getDataSlice("altitude");
    boost::shared_array<double> altArray = altitudeData->asDouble();
    int found = 0;
    for (size_t i = 0; i < altitudeData->size(); i++) {
        if (altArray[i] > 2000) {
            found++;
        }
    }
    BOOST_CHECK(found > 100); // at least 100 cells above 2000m

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
#ifdef HAVE_LIBMIC
		boost::shared_ptr<CDMReader> feltReader = boost::shared_ptr<CDMReader>(new FeltCDMReader(grdFile, topSrcDir+"/share/etc/felt2nc_variables_hirlam20.xml"));
#else
		boost::shared_ptr<CDMReader> feltReader = boost::shared_ptr<CDMReader>(new FeltCDMReader2(grdFile, topSrcDir+"/share/etc/felt2nc_variables_hirlam20.xml"));
#endif
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
		interpolator->changeProjection(MIFI_INTERPOL_BILINEAR, "+proj=stere +lat_0=90 +lon_0=0 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m");
		BOOST_CHECK(true);
		NetCDF_CDMWriter(interpolator, "testInterpolator2.nc");
		BOOST_CHECK(true);
	}
}

BOOST_AUTO_TEST_CASE(test_interpolatorRelative)
{
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/flth00.dat");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
#ifdef HAVE_LIBMIC
    boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
#else
    boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader2(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
#endif
    boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(feltReader));
    interpolator->changeProjection(MIFI_INTERPOL_BILINEAR, "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", "0,50000,...,x;relativeStart=0", "0,50000,...,x;relativeStart=0", "m", "m");
    //interpolator->getCDM().toXMLStream(cerr);
    BOOST_CHECK(true);
    BOOST_CHECK(interpolator->getDataSlice("x")->size() == 297);
    BOOST_CHECK(interpolator->getDataSlice("y")->size() == 286);
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
