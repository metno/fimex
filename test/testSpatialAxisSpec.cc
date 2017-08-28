/*
 * Fimex, testSpatialAxisSpec.cc
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
 *  Created on: Mar 18, 2009
 *      Author: Heiko Klein
 */

#include "fimex_config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>
#include "fimex/Utils.h"
#include "fimex/SpatialAxisSpec.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_SpatialAxisSpec )
{
	string axisSpec("-450000,-400000,...,50000");
	double start = 30000;
	double end = 230000;

	SpatialAxisSpec sas(axisSpec, start, end);
	const vector<double>& vals = sas.getAxisSteps();
//	cerr << vals.size() << endl;
	BOOST_CHECK(vals.size() == 11);
//	for (size_t i = 0; i < vals.size(); ++i)
//		cerr << "i: " << vals[i] << endl;
	BOOST_CHECK(vals[0] == -450000);
	BOOST_CHECK(vals[10] == 50000);

	string relAxisSpec("0,50000,...,x,x+50000;relativeStart=0");
	SpatialAxisSpec relSas(relAxisSpec, start, end);
	const vector<double>& relVals = relSas.getAxisSteps();
	//cerr << relVals.size() << endl;
	BOOST_CHECK(relVals.size() == 6);
	//for (size_t i = 0; i < relVals.size(); ++i)
	//	cerr << "i: " << relVals[i] << endl;
	BOOST_CHECK(relVals[0] == 0);
	BOOST_CHECK(relVals[5] == 250000);

}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif

