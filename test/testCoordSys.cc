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

#include "testinghelpers.h"

#include "fimex/CDMFileReaderFactory.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/OceanSG2.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/Data.h"
#include "fimex/CDM.h"

#if HAVE_BOOST_UNIT_TEST_FRAMEWORK
// for message formatting
#include "boost/date_time/posix_time/posix_time_io.hpp"
#endif

#include <numeric>

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_coordSys)
{
    const string fileName = pathTest("coordTest.nc");
    CDMReader_p reader = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileName);

    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    CoordinateSystem_cp_v coordSys = listCoordinateSystems(reader);
    const CDM& cdm = reader->getCDM();
    TEST4FIMEX_CHECK_EQ(coordSys.size(), 3);

    // find an appropriate coordinate system for a variable
    string altitude = "altitude";
    CoordinateSystem_cp_v::iterator varSysIt = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(altitude));
    if (varSysIt == coordSys.end()) {
        TEST4FIMEX_FAIL("no coordinate system found for '" << altitude << "'");
        return;
    }
    TEST4FIMEX_CHECK_EQ((*varSysIt)->getConventionName(), "CF-1.X");

    if ((*varSysIt)->isSimpleSpatialGridded()) {
        // find the geographical axes, returns 0 axes if not found
        CoordinateAxis_cp xAxis = (*varSysIt)->getGeoXAxis(); // X or Lon
        TEST4FIMEX_CHECK_EQ(xAxis->getName(), "x");
        CoordinateAxis_cp yAxis = (*varSysIt)->getGeoYAxis(); // Y or Lat
        TEST4FIMEX_CHECK_EQ(yAxis->getName(), "y");
        // find vertical axis
        CoordinateAxis_cp vAxis = (*varSysIt)->getGeoZAxis();
        TEST4FIMEX_CHECK(!vAxis);
        // find time axis
        CoordinateAxis_cp tAxis = (*varSysIt)->getTimeAxis();
        TEST4FIMEX_CHECK_EQ(tAxis->getName(), "time");

        //std::cerr << *((*varSysIt)->getProjection()) << endl;
        TEST4FIMEX_CHECK((*varSysIt)->hasProjection());
        TEST4FIMEX_CHECK_EQ((*varSysIt)->getProjection()->getName(), "stereographic");

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
        DataPtr data = reader->getDataSlice(altitude, sb);
        TEST4FIMEX_CHECK_EQ(data->size(), 11 * 5 * 2); // x=11,y=5, v=none, t = 2

        // get the proj-string for which-ever reason:
        // may return "" if no projection found
        // TODO: impl
        //string projStr = (*varSysIt)->getProjString();

    } else {
        TEST4FIMEX_FAIL("not a simple spatial grid");
    }

    // slice check, varName is 4d sequence, starting at 0
    string varName("cloud_area_fraction_in_atmosphere_layer");
    varSysIt = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt == coordSys.end()) {
        TEST4FIMEX_FAIL("no coordinate system found for '" << varName << "'");
    }
    if (!(*varSysIt)->isSimpleSpatialGridded()) {
        TEST4FIMEX_FAIL("cs not a simple spatial grid");
    }
    const CoordinateSystem& cs = **varSysIt;
    DataPtr allData = reader->getData("cloud_area_fraction_in_atmosphere_layer");
    size_t n = 11 * 11 * 4 * 4;
    TEST4FIMEX_REQUIRE_EQ(allData->size(), n);
    boost::shared_array<short> all = allData->asShort();
    TEST4FIMEX_CHECK_EQ(accumulate(all.get(), all.get() + n, 0UL), static_cast<size_t>(n * (n - 1) / 2)); // gauss computation of sum of sequence

    // check accessor function
    string timeAxis = reader->getCDM().getTimeAxis("cloud_area_fraction_in_atmosphere_layer");
    TEST4FIMEX_CHECK_EQ(timeAxis, "time");
    timeAxis = reader->getCDM().getTimeAxis("time");
    TEST4FIMEX_CHECK_EQ(timeAxis, "time");

    SliceBuilder sb(cdm, varName);
    // last slice
    sb.setStartAndSize(cs.getGeoZAxis(), 3, 1);
    sb.setStartAndSize(cs.getTimeAxis(), 3, 1);
    // generic slice reader
    DataPtr sData = reader->CDMReader::getDataSlice("cloud_area_fraction_in_atmosphere_layer", sb);
    size_t s = 11*11;
    TEST4FIMEX_CHECK_EQ(sData->size(), s);
    boost::shared_array<short> slice = sData->asShort();
    TEST4FIMEX_CHECK_EQ(accumulate(slice.get(), slice.get() + s, 0UL), static_cast<size_t>(n * (n - 1) / 2 - (n - s) * (n - s - 1) / 2));

    // native slice reader
    sData = reader->getDataSlice("cloud_area_fraction_in_atmosphere_layer", sb);
    s = 11*11;
    TEST4FIMEX_CHECK_EQ(sData->size(), s);
    slice = sData->asShort();
    TEST4FIMEX_CHECK_EQ(accumulate(slice.get(), slice.get() + s, 0UL), static_cast<size_t>(n * (n - 1) / 2 - (n - s) * (n - s - 1) / 2));

    // vertical slice, general reader
    sb.setStartAndSize(cs.getGeoXAxis(), 1, 1);
    sb.setStartAndSize(cs.getGeoYAxis(), 2, 1);
    sb.setStartAndSize(cs.getGeoZAxis(), 0, 4);
    sb.setStartAndSize(cs.getTimeAxis(), 2, 1);
    sData = reader->CDMReader::getDataSlice("cloud_area_fraction_in_atmosphere_layer", sb);
    slice = sData->asShort();
    TEST4FIMEX_REQUIRE_EQ(sData->size(), 4);
    short firstVal = slice[0];
    for (size_t i = 1; i < 4; i++) {
        TEST4FIMEX_CHECK_EQ(slice[i], static_cast<short>(firstVal + (i * s)));
    }

    // general time slice
    sb.setStartAndSize(cs.getGeoXAxis(), 2, 1);
    sb.setStartAndSize(cs.getGeoYAxis(), 3, 1);
    sb.setStartAndSize(cs.getGeoZAxis(), 2, 1);
    sb.setStartAndSize(cs.getTimeAxis(), 0, 4);
    sData = reader->CDMReader::getDataSlice("cloud_area_fraction_in_atmosphere_layer", sb);
    slice = sData->asShort();
    TEST4FIMEX_REQUIRE_EQ(sData->size(), 4);
    firstVal = slice[0];
    for (size_t i = 1; i < 4; i++) {
        TEST4FIMEX_CHECK_EQ(slice[i], static_cast<short>(firstVal + (i * s * 4)));
    }

    // vertical slice, native reader
    sb.setStartAndSize(cs.getGeoXAxis(), 1, 1);
    sb.setStartAndSize(cs.getGeoYAxis(), 2, 1);
    sb.setStartAndSize(cs.getGeoZAxis(), 0, 4);
    sb.setStartAndSize(cs.getTimeAxis(), 2, 1);
    sData = reader->getDataSlice("cloud_area_fraction_in_atmosphere_layer", sb);
    slice = sData->asShort();
    TEST4FIMEX_REQUIRE_EQ(sData->size(), 4);
    firstVal = slice[0];
    for (size_t i = 1; i < 4; i++) {
        TEST4FIMEX_CHECK_EQ(slice[i], static_cast<short>(firstVal + (i * s)));
    }

    // native time slice
    sb.setStartAndSize(cs.getGeoXAxis(), 2, 1);
    sb.setStartAndSize(cs.getGeoYAxis(), 3, 1);
    sb.setStartAndSize(cs.getGeoZAxis(), 2, 1);
    sb.setStartAndSize(cs.getTimeAxis(), 0, 4);
    sData = reader->getDataSlice("cloud_area_fraction_in_atmosphere_layer", sb);
    slice = sData->asShort();
    TEST4FIMEX_REQUIRE_EQ(sData->size(), 4);
    firstVal = slice[0];
    for (size_t i = 1; i < 4; i++) {
        TEST4FIMEX_CHECK_EQ(slice[i], static_cast<short>(firstVal + (i * s * 4)));
    }

    // check reference time
    boost::posix_time::ptime refTime = getUniqueForecastReferenceTime(reader);
    TEST4FIMEX_CHECK_EQ(refTime, boost::posix_time::ptime(boost::gregorian::date(2000, 1, 1), boost::posix_time::time_duration(10, 0, 0)));
}

TEST4FIMEX_TEST_CASE(test_vTrans)
{
    const string fileName = pathTest("verticalOceanSG2.nc");
    CDMReader_p reader = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileName);

    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    CoordinateSystem_cp_v coordSys = listCoordinateSystems(reader);
    string varName = "temp";
    CoordinateSystem_cp_v::iterator varSysIt = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(varName));
    TEST4FIMEX_CHECK(varSysIt != coordSys.end());
    TEST4FIMEX_CHECK((*varSysIt)->hasVerticalTransformation());
    std::shared_ptr<const VerticalTransformation> vtran = (*varSysIt)->getVerticalTransformation();
    TEST4FIMEX_CHECK(vtran);
    TEST4FIMEX_CHECK_EQ(vtran->getName(), OceanSG2::NAME());
    TEST4FIMEX_CHECK(dynamic_cast<const OceanSG2*>(vtran.get()));
}
