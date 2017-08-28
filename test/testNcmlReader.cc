/*
 * Fimex, testNcmlReader.cc
 *
 * (C) Copyright 2012, met.no
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
 *  Created on: Mar 13, 2012
 *      Author: Heiko Klein
 */

#include "fimex_config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/SliceBuilder.h"
#include "fimex/Data.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMconstants.h"
#include "fimex/CDM.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/Logger.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_ncmlRead )
{
    //defaultLogLevel(Logger::DEBUG);
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/coordTest.nc");
    string ncmlName(topSrcDir+"/test/test.ncml");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileName, XMLInputFile(ncmlName)));
    BOOST_CHECK(true);
    DataPtr data = reader->getDataSlice("sea_surface_temperature", 1);
    SliceBuilder sb(reader->getCDM(), "sea_surface_temperature");
    sb.setStartAndSize("time", 1, 1);
    DataPtr dataSlice = reader->getDataSlice("sea_surface_temperature", sb);
    BOOST_CHECK(true);
    BOOST_CHECK(data->size() == dataSlice->size());
    boost::shared_array<short> d = data->asShort();
    boost::shared_array<short> ds = dataSlice->asShort();
    for (size_t i = 0; i < data->size(); i++) {
        BOOST_CHECK(d[i] == ds[i]);
    }

    // proj4-string removed
    CDMAttribute proj4;
    BOOST_CHECK(!reader->getCDM().getAttribute("projection_1", "proj4",proj4));

    // find towgs84
    vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(reader);
    vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt = find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator("sea_surface_temperature"));
    string s = (*varSysIt)->getProjection()->getProj4String();
    BOOST_CHECK(s.find("+towgs84=") != string::npos);


    BOOST_CHECK(reader->getCDM().hasDimension("Sigma"));

    // check attributes of renamed variable
    CDMAttribute attr;
    BOOST_CHECK(reader->getCDM().getAttribute("snow_thickness", "metno_name", attr));
    BOOST_CHECK(!reader->getCDM().getAttribute("snow_thickness", "long_name", attr));

    // check of new variable
    BOOST_CHECK(reader->getCDM().hasVariable("new_var"));
    data = reader->getDataSlice("new_var", 1);
    BOOST_CHECK(data->size() == 0);
    sb = SliceBuilder(reader->getCDM(), "new_var");
    sb.setStartAndSize("time", 1, 1);
    dataSlice = reader->getDataSlice("new_var", sb);
    BOOST_CHECK(dataSlice->size() == 0);

}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif




