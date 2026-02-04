/*
 * Fimex, testSliceBuilder.cc
 *
 * (C) Copyright 2011-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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
#include "fimex/CDM.h"
#include "fimex/CDMException.h"
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

TEST4FIMEX_TEST_CASE(test_slicebuilder)
{
    CDM cdm;
    setupCDM(cdm);
    TEST4FIMEX_CHECK(cdm.hasVariable("var"));
    SliceBuilder sb(cdm, "var");
    sb.setStartAndSize("dim1", 1, 3);

    TEST4FIMEX_CHECK_THROW(sb.setStartAndSize("doesNotExit", 5, 1), CDMException);
    TEST4FIMEX_CHECK_THROW(sb.setStartAndSize("dim2", 17, 1), out_of_range);

    TEST4FIMEX_CHECK_EQ(sb.getDimensionNames()[0], "dim1");
    TEST4FIMEX_CHECK_EQ(sb.getDimensionNames()[1], "dim2");
    TEST4FIMEX_CHECK_EQ(sb.getDimensionSizes()[0], 3);
    TEST4FIMEX_CHECK_EQ(sb.getDimensionSizes()[1], 6);
    TEST4FIMEX_CHECK_EQ(sb.getDimensionStartPositions()[0], 1);
    TEST4FIMEX_CHECK_EQ(sb.getDimensionStartPositions()[1], 0);

    // cerr << "unset dimensionNames: " << sb.getUnsetDimensionNames().size() << endl;
    TEST4FIMEX_CHECK_EQ(sb.getUnsetDimensionNames().size(), 1);
    TEST4FIMEX_CHECK_EQ(sb.getUnsetDimensionNames()[0], "dim2");
}
