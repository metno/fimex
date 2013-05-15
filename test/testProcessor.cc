/*
 * Fimex, testProcessor.cc
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
 *  Created on: Mar 19, 2012
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
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMProcessor.h"
#include "fimex/CDMconstants.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_function )
{
//    defaultLogLevel(Logger::DEBUG);
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/coordTest.nc");

    boost::shared_ptr<CDMReader> nc = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileName);
    double t0 = 1179309600.;
    {
        boost::shared_ptr<CDMProcessor> proc(new CDMProcessor(nc));
        proc->deAccumulate("time");

        DataPtr data = proc->getData("time");
        boost::shared_array<double> time = data->asDouble();
        BOOST_CHECK_CLOSE(time[0], t0, 1e-5); // unchanged
        BOOST_CHECK_CLOSE(time[1], 3600., 1e-5);
        BOOST_CHECK_CLOSE(time[2], 3600., 1e-5);
        BOOST_CHECK_CLOSE(time[3], 3600., 1e-5);
    }

    {
        boost::shared_ptr<CDMProcessor> proc(new CDMProcessor(nc));
        proc->accumulate("time");
        DataPtr data = proc->getData("time");
        boost::shared_array<double> time = data->asDouble();
        BOOST_CHECK_CLOSE(time[0], t0, 1e-5); // unchanged
        BOOST_CHECK_CLOSE(time[1], time[0] + t0+3600., 1e-5);
        BOOST_CHECK_CLOSE(time[2], time[1] + t0+2*3600., 1e-5);
        BOOST_CHECK_CLOSE(time[3], time[2] + t0+3*3600., 1e-5);
    }

}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif




