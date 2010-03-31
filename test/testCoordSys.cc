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

#include "fimex/config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>

#include "fimex/FeltCDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_coordSys )
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
    const CDM& cdm = feltReader->getCDM();

    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(cdm);

    // find an appropriate coordinate system for a variable
    string varName = "tksoil";
    vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt = find(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator("tk"));
    if (varSysIt == coordSys.end()) {
        cerr << "no coordinate system found for " << varName << endl;
        BOOST_CHECK(false);
        return;
    }

    if (*varSysIt->isSimpleSpatialGridded()) {
        // find the geographical axes, returns 0 axes if not found
        boost::shared_ptr<CoordinateAxis> xAxis = *varSysIt->getGeoXAxis(); // X or Lon
        boost::shared_ptr<CoordinateAxis> yAxis = *varSysIt->getGeoYAxis(); // Y or Lat
        // find vertical axis
        boost::shared_ptr<CoordinateAxis> vAxis = *varSysIt->getGeoZAxis();
        // find time axis
        boost::shared_ptr<CoordinateAxis> tAxis = *varSysIt->getTimeAxis();

        // create a slice-builder for the variable
        // the slicebuilder starts with the maximum variable size
        SliceBuilder sb = new SliceBuilder(cdm, varName);
        // select the 3rd vertical layer
        sb.setStartAndSize(vAxis, 3, 1);
        // select the 5th time slice
        sb.setStartAndSize(tAxis, 5, 1);

        // do something with the other dimensions (i.e. eps)
        // here: set all other dimensions to 0
        const DimVec& dims = cdm.getDimensions();
        for (DimVec::const_iterator dimIt = dims.begin(); dimIt != dims.end(); ++dimIt) {
            if (xAxis.get() != 0 && dimIt->getName() == xAxis->getName()) {
                // ignore
            } else if (yAxis.get() != 0 && dimIt->getName() == yAxis->getName()) {
                // ignore
            } else if (zAxis.get() != 0 && dimIt->getName() == zAxis->getName()) {
                // ignore
            } else if (tAxis.get() != 0 && dimIt->getName() == tAxis->getName()) {
                // ignore
            } else {
                sb.setStartAndSize(dimIt->getName(), 0, 1);
            }
        }

        // fetch the data
        boost::shared_ptr<Data> data = feltReader->getDataSlice(varName, sb);

        // get the proj-string for which-ever reason:
        // may return "" if no projection found
        String projStr = *varSysIt->getProjString();

    } else {
        // TODO: implement other data-types, i.e.
        // var(r)
        // lon(r)
        // lat(r)
        // time(r)
    }
        BOOST_CHECK(true);
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif

