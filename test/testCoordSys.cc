/*
 * Fimex, testCoordSys.cc
 *
 * (C) Copyright 2009, met.no
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
 *
 *  Created on: Mar 11, 2010
 *      Author: Heiko Klein
 */

#include "../config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>
#include <numeric>

#include "fimex/NetCDF_CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/Data.h"
#include "fimex/CDM.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_coordSys )
{
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/coordTest.nc");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> reader(new NetCDF_CDMReader(fileName));
    const CDM& cdm = reader->getCDM();

    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(cdm);
    BOOST_CHECK(coordSys.size() == 3);

    // find an appropriate coordinate system for a variable
    string altitude = "altitude";
    vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(altitude));
    if (varSysIt == coordSys.end()) {
        cerr << "no coordinate system found for " << altitude << endl;
        BOOST_CHECK(false);
        return;
    }
    BOOST_CHECK((*varSysIt)->getConventionName() == "CF-1.X");

    if ((*varSysIt)->isSimpleSpatialGridded()) {
        // find the geographical axes, returns 0 axes if not found
        CoordinateSystem::ConstAxisPtr xAxis = (*varSysIt)->getGeoXAxis(); // X or Lon
        BOOST_CHECK(xAxis->getName() == "x");
        CoordinateSystem::ConstAxisPtr yAxis = (*varSysIt)->getGeoYAxis(); // Y or Lat
        BOOST_CHECK(yAxis->getName() == "y");
        // find vertical axis
        CoordinateSystem::ConstAxisPtr vAxis = (*varSysIt)->getGeoZAxis();
        BOOST_CHECK(vAxis.get() == 0);
        // find time axis
        CoordinateSystem::ConstAxisPtr tAxis = (*varSysIt)->getTimeAxis();
        BOOST_CHECK(tAxis->getName() == "time");

        //std::cerr << *((*varSysIt)->getProjection()) << endl;
        BOOST_CHECK((*varSysIt)->hasProjection());
        BOOST_CHECK((*varSysIt)->getProjection()->getName() == "stereographic");

        // create a slice-builder for the variable
        // the slicebuilder starts with the maximum variable size
        SliceBuilder sb(cdm, altitude);
        sb.setStartAndSize(yAxis, 3, 5);
        // select the 3rd vertical layer
        sb.setStartAndSize(vAxis, 3, 2); // should not fail, though vAxis is undefined
        // select the 2n time slice
        sb.setStartAndSize(tAxis, 2, 2);

        // do something with the other dimensions (i.e. eps)
        // here: set all other dimensions to 0
        const vector<string>& shape = cdm.getVariable(altitude).getShape();
        for (vector<string>::const_iterator dimIt = shape.begin(); dimIt != shape.end(); ++dimIt) {
            if (xAxis.get() && xAxis->getName() == *dimIt) {
                // ignore
            } else if (yAxis.get() && yAxis->getName() == *dimIt) {
                // ignore
            } else if (vAxis.get() && vAxis->getName() == *dimIt) {
                // ignore
            } else if (tAxis.get() && tAxis->getName() == *dimIt) {
                // ignore
            } else {
                sb.setStartAndSize(*dimIt, 0, 1);
            }
        }

        // fetch the data
        boost::shared_ptr<Data> data = reader->getDataSlice(altitude, sb);
        BOOST_CHECK(data->size() == 11*5*2); // x=11,y=5, v=none, t = 2


        // get the proj-string for which-ever reason:
        // may return "" if no projection found
        // TODO: impl
        //string projStr = (*varSysIt)->getProjString();

    } else {
        BOOST_CHECK(false); // not simpleSpatialGrid
    }

    // slice check, varName is 4d sequence, starting at 0
    string varName("cloud_area_fraction_in_atmosphere_layer");
    varSysIt = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt == coordSys.end()) {
        cerr << "no coordinate system found for " << varName << endl;
        BOOST_CHECK(false);
        return;
    }
    if (!(*varSysIt)->isSimpleSpatialGridded()) {
        BOOST_CHECK(false);
    }
    const CoordinateSystem& cs = **varSysIt;
    boost::shared_ptr<Data> allData = reader->getData("cloud_area_fraction_in_atmosphere_layer");
    size_t n = 11 * 11 * 4 * 4;
    BOOST_CHECK(allData->size() == n);
    boost::shared_array<short> all = allData->asShort();
    BOOST_CHECK(accumulate(all.get(), all.get()+n, 0) == (n*(n-1)/2)); // gauss computation of sum of sequence

    // check accessor function
    string timeAxis = reader->getCDM().getTimeAxis("cloud_area_fraction_in_atmosphere_layer");
    BOOST_CHECK(timeAxis == "time");
    timeAxis = reader->getCDM().getTimeAxis("time");
    BOOST_CHECK(timeAxis == "time");

    SliceBuilder sb(cdm, varName);
    // last slice
    sb.setStartAndSize(cs.getGeoZAxis(), 3, 1);
    sb.setStartAndSize(cs.getTimeAxis(), 3, 1);
    // generic slice reader
    boost::shared_ptr<Data> sData = reader->CDMReader::getDataSlice("cloud_area_fraction_in_atmosphere_layer", sb);
    size_t s = 11*11;
    BOOST_CHECK(sData->size() == s);
    boost::shared_array<short> slice = sData->asShort();
    BOOST_CHECK(accumulate(slice.get(), slice.get()+s, 0) == (n*(n-1)/2 - (n-s)*(n-s-1)/2));

    // native slice reader
    sData = reader->getDataSlice("cloud_area_fraction_in_atmosphere_layer", sb);
    s = 11*11;
    BOOST_CHECK(sData->size() == s);
    slice = sData->asShort();
    BOOST_CHECK(accumulate(slice.get(), slice.get()+s, 0) == (n*(n-1)/2 - (n-s)*(n-s-1)/2));

    // vertical slice, general reader
    sb.setStartAndSize(cs.getGeoXAxis(), 1, 1);
    sb.setStartAndSize(cs.getGeoYAxis(), 2, 1);
    sb.setStartAndSize(cs.getGeoZAxis(), 0, 4);
    sb.setStartAndSize(cs.getTimeAxis(), 2, 1);
    sData = reader->CDMReader::getDataSlice("cloud_area_fraction_in_atmosphere_layer", sb);
    slice = sData->asShort();
    BOOST_CHECK(sData->size() == 4);
    short firstVal = slice[0];
    for (size_t i = 1; i < 4; i++) {
        BOOST_CHECK(slice[i] == firstVal+(i*s));
    }

    // general time slice
    sb.setStartAndSize(cs.getGeoXAxis(), 2, 1);
    sb.setStartAndSize(cs.getGeoYAxis(), 3, 1);
    sb.setStartAndSize(cs.getGeoZAxis(), 2, 1);
    sb.setStartAndSize(cs.getTimeAxis(), 0, 4);
    sData = reader->CDMReader::getDataSlice("cloud_area_fraction_in_atmosphere_layer", sb);
    slice = sData->asShort();
    BOOST_CHECK(sData->size() == 4);
    firstVal = slice[0];
    for (size_t i = 1; i < 4; i++) {
        BOOST_CHECK(slice[i] == firstVal+(i*s*4));
    }

    // vertical slice, native reader
    sb.setStartAndSize(cs.getGeoXAxis(), 1, 1);
    sb.setStartAndSize(cs.getGeoYAxis(), 2, 1);
    sb.setStartAndSize(cs.getGeoZAxis(), 0, 4);
    sb.setStartAndSize(cs.getTimeAxis(), 2, 1);
    sData = reader->getDataSlice("cloud_area_fraction_in_atmosphere_layer", sb);
    slice = sData->asShort();
    BOOST_CHECK(sData->size() == 4);
    firstVal = slice[0];
    for (size_t i = 1; i < 4; i++) {
        BOOST_CHECK(slice[i] == firstVal+(i*s));
    }

    // native time slice
    sb.setStartAndSize(cs.getGeoXAxis(), 2, 1);
    sb.setStartAndSize(cs.getGeoYAxis(), 3, 1);
    sb.setStartAndSize(cs.getGeoZAxis(), 2, 1);
    sb.setStartAndSize(cs.getTimeAxis(), 0, 4);
    sData = reader->getDataSlice("cloud_area_fraction_in_atmosphere_layer", sb);
    slice = sData->asShort();
    BOOST_CHECK(sData->size() == 4);
    firstVal = slice[0];
    for (size_t i = 1; i < 4; i++) {
        BOOST_CHECK(slice[i] == firstVal+(i*s*4));
    }

    // check reference time
    boost::posix_time::ptime refTime = getUniqueForecastReferenceTime(reader);
    BOOST_CHECK(refTime == boost::posix_time::ptime(boost::gregorian::date(2000,1,1), boost::posix_time::time_duration(10,0,0)));

}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif

