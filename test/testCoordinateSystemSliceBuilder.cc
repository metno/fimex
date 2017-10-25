/*
 * Fimex, testCoordinateSystemSliceBuilder.cc
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Jun 7, 2011
 *      Author: Heiko Klein
 */

#include "fimex_config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/CoordinateSystemSliceBuilder.h"
#include "fimex/Data.h"
#include "fimex/CDMReader.h"

using namespace std;
using namespace MetNoFimex;

#include "testinghelpers.h"

BOOST_AUTO_TEST_CASE( test_cs_slicebuilder_simple )
{
    CDMReader_p reader = CDMFileReaderFactory::create("netcdf", pathTest("coordTest.nc"));
    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(reader);
    const CDM& cdm = reader->getCDM();
    // find an appropriate coordinate system for a variable
    string varName = "air_temperature";
    vector<boost::shared_ptr<const CoordinateSystem> >::iterator csIt =
            find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(varName));
    if (csIt == coordSys.end()) BOOST_CHECK(false);
    CoordinateSystemSliceBuilder sb(cdm, *csIt);
    sb.setReferenceTimePos(1);
    sb.setTimeStartAndSize(0, 4);

    BOOST_CHECK(sb.getUnsetDimensionNames()[0] == "x");
    BOOST_CHECK(sb.getUnsetDimensionNames()[1] == "y");

    DataPtr data = reader->getDataSlice((*csIt)->getTimeAxis()->getName(),
                                                        sb.getTimeVariableSliceBuilder());
    BOOST_CHECK(data->size() == 4);
    BOOST_CHECK(data->asInt()[1] == 1179313200);
}

BOOST_AUTO_TEST_CASE( test_cs_slicebuilder_reftime )
{
    CDMReader_p reader = CDMFileReaderFactory::create("netcdf", pathTest("coordRefTimeTest.nc"));
    BOOST_CHECK(reader.get() != 0);
    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(reader);
    BOOST_CHECK(coordSys.size() > 0);
    const CDM& cdm = reader->getCDM();
    // find an appropriate coordinate system for a variable
    string varName = "air_temperature";
    vector<boost::shared_ptr<const CoordinateSystem> >::iterator csIt =
            find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(varName));
    if (csIt == coordSys.end()) BOOST_CHECK(false);
    CoordinateSystemSliceBuilder sb(cdm, *csIt);
    sb.setReferenceTimePos(1);
    sb.setTimeStartAndSize(0, 2);

    BOOST_CHECK(sb.getUnsetDimensionNames()[0] == "x");
    BOOST_CHECK(sb.getUnsetDimensionNames()[1] == "y");

    DataPtr data = reader->getDataSlice((*csIt)->getTimeAxis()->getName(),
                                                        sb.getTimeVariableSliceBuilder());
    BOOST_CHECK(data->size() == 2);
    BOOST_CHECK(data->asShort()[1] == 27);
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
