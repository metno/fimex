/*
 * Fimex, testSliceBuilder.cc
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
 *  Created on: Jun 1, 2011
 *      Author: Heiko Klein
 */

#include "testinghelpers.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#include "fimex/CDM.h"
#include <fimex/SliceBuilder.h>

using namespace std;
using namespace MetNoFimex;

void setupCDM(CDM& cdm)
{
    string dim1Str("dim1");
    string dim2Str("dim2");
    string dim3Str("dim3");
    CDMDimension dim1(dim1Str, 5);
    CDMDimension dim2(dim2Str, 6);

    cdm.addDimension(dim1);
    cdm.addDimension(dim2);

    vector<std::string> varDims;
    varDims.push_back(dim1Str);
    varDims.push_back(dim2Str);
    CDMVariable var("var", CDM_NAT, varDims);
    cdm.addVariable(var);
}

BOOST_AUTO_TEST_CASE( test_slicebuilder )
{
    CDM cdm;
    setupCDM(cdm);
    BOOST_CHECK(cdm.hasVariable("var"));
    SliceBuilder sb(cdm, "var");
    BOOST_CHECK(true);
    sb.setStartAndSize("dim1", 1, 3);

    try {
        sb.setStartAndSize("doesNotExit", 5, 1);
        BOOST_CHECK(false);
    } catch (CDMException& ce) {
        BOOST_CHECK(true);
    }

    try {
        sb.setStartAndSize("dim2", 17, 1);
        BOOST_CHECK(false);
    } catch (out_of_range& ce) {
        BOOST_CHECK(true);
    }

    BOOST_CHECK(sb.getDimensionNames()[0] == "dim1");
    BOOST_CHECK(sb.getDimensionNames()[1] == "dim2");
    BOOST_CHECK(sb.getDimensionSizes()[0] == 3);
    BOOST_CHECK(sb.getDimensionSizes()[1] == 6);
    BOOST_CHECK(sb.getDimensionStartPositions()[0] == 1);
    BOOST_CHECK(sb.getDimensionStartPositions()[1] == 0);

    // cerr << "unset dimensionNames: " << sb.getUnsetDimensionNames().size() << endl;
    BOOST_CHECK(sb.getUnsetDimensionNames().size() == 1);
    BOOST_CHECK(sb.getUnsetDimensionNames()[0] == "dim2");
}

#endif // HAVE_BOOST_UNIT_TEST_FRAMEWORK
