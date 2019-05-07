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

#include "testinghelpers.h"
#include "fimex/Utils.h"
#include "fimex/SpatialAxisSpec.h"

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_SpatialAxisSpec)
{
	string axisSpec("-450000,-400000,...,50000");
	double start = 30000;
	double end = 230000;

	SpatialAxisSpec sas(axisSpec, start, end);
	const vector<double>& vals = sas.getAxisSteps();
//	cerr << vals.size() << endl;
        TEST4FIMEX_CHECK_EQ(vals.size(), 11);
        //	for (size_t i = 0; i < vals.size(); ++i)
        //		cerr << "i: " << vals[i] << endl;
        TEST4FIMEX_CHECK_EQ(vals[0], -450000);
        TEST4FIMEX_CHECK_EQ(vals[10], 50000);

        string relAxisSpec("0,50000,...,x,x+50000;relativeStart=0");
	SpatialAxisSpec relSas(relAxisSpec, start, end);
	const vector<double>& relVals = relSas.getAxisSteps();
	//cerr << relVals.size() << endl;
        TEST4FIMEX_CHECK_EQ(relVals.size(), 6);
        //for (size_t i = 0; i < relVals.size(); ++i)
	//	cerr << "i: " << relVals[i] << endl;
        TEST4FIMEX_CHECK_EQ(relVals[0], 0);
        TEST4FIMEX_CHECK_EQ(relVals[5], 250000);
}
