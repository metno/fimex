/*
 * Fimex, testNcmlAggregationReader.cc
 *
 * (C) Copyright 2013, met.no
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
 *  Created on: Apr 17, 2013
 *      Author: heikok
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
#include "fimex/CDMconstants.h"
#include "fimex/CDMReader.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"

using namespace std;
using namespace MetNoFimex;


BOOST_AUTO_TEST_CASE( test_joinExisting )
{
    defaultLogLevel(Logger::DEBUG);
    string topSrcDir(TOP_SRCDIR);
    string ncmlName(topSrcDir+"/test/data/joinExistingAgg.ncml");
    if (!ifstream(ncmlName.c_str())) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NCML, ncmlName));
    BOOST_CHECK(true);
    BOOST_CHECK(reader->getCDM().getUnlimitedDim()->getLength() == 5);
    BOOST_CHECK(reader->getDataSlice("unlim", 3)->asShort()[0] == 4);
    BOOST_CHECK(reader->getDataSlice("multi", 3)->asShort()[1] == -4);

}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
