/*
 * Fimex, testProjections.cc
 *
 * (C) Copyright 2010, met.no
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
 *  Created on: Apr 29, 2010
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
#include "fimex/coordSys/Projection.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_projection )
{
    string proj4stere = "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a=6371000 +e=0";
    boost::shared_ptr<Projection> projs = Projection::createByProj4(proj4stere);
    BOOST_CHECK(projs->getName() == "stereographic");
    BOOST_CHECK(projs->isDegree() == false);

    std::vector<CDMAttribute> attrs = projs->getParameters();
    // generate another projection
    boost::shared_ptr<Projection> projs2 = Projection::create(attrs);
    BOOST_CHECK(projs2->getName() == "stereographic");


    // TODO: test other projections
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif

