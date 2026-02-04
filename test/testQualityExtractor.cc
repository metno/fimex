/*
 * Fimex
 *
 * (C) Copyright 2009-2026, met.no
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
 */

#include "testinghelpers.h"

#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMQualityExtractor.h"
#include "fimex/Data.h"
#include "fimex/SharedArray.h"
#include "fimex/mifi_constants.h"

#include <memory>

using namespace std;
using namespace MetNoFimex;

//#define TEST_DEBUG

#ifdef HAVE_FELT
TEST4FIMEX_TEST_CASE(test_qualityExtract)
{
    CDMReader_p feltReader = getFLTH00Reader();
    if (!feltReader)
        return;

    std::shared_ptr<CDMQualityExtractor> extract = std::make_shared<CDMQualityExtractor>(feltReader, "", pathTest("testQualityConfig.xml"));

    map<string, string> statusVariables = extract->getStatusVariable();
    map<string, string> variableFlags = extract->getVariableFlags();
    map<string, vector<double>> variableValues = extract->getVariableValues();
#ifdef TEST_DEBUG
    for (map<string, string>::iterator svIt = statusVariables.begin(); svIt != statusVariables.end(); ++svIt) {
        string varName = svIt->first;
        string statusVarName = svIt->second;
        cerr << varName << ": " << statusVarName;
        if (variableFlags.find(varName) != variableFlags.end()) {
            cerr << ": flag: " << variableFlags[varName];
        }
        if (variableValues.find(varName) != variableValues.end()) {
            cerr << ": values: ";
            vector<double> vals = variableValues[varName];
            for (size_t i = 0; i < vals.size(); i++) {
                cerr << i << ", ";
            }
        }
        cerr << endl;
    }
#endif /* TEST_DEBUG */
    TEST4FIMEX_CHECK(statusVariables.find("air_temperature") != statusVariables.end());
    TEST4FIMEX_CHECK_EQ(statusVariables["bla"], "blub");
    TEST4FIMEX_CHECK(variableFlags.find("bla") == variableFlags.end());
    TEST4FIMEX_CHECK_EQ(variableValues["bla"][2], 3);
}

TEST4FIMEX_TEST_CASE(test_qualityExtract_convert)
{
    CDMReader_p feltReader = getFLTH00Reader();
    if (!feltReader)
        return;
    std::shared_ptr<CDMQualityExtractor> qe = std::make_shared<CDMQualityExtractor>(feltReader, "", pathTest("testQualityConfig.xml"));

    TEST4FIMEX_CHECK(writeToFile(qe, "test_qualityExtract_convert.nc"));
}
#endif // HAVE_FELT

#ifdef HAVE_NETCDF_H
TEST4FIMEX_TEST_CASE(test_qualityExtract_mask)
{
    const string fileNameD = pathTest("testQEmask_data.nc");
    const string fileNameX = require("testQEmask.xml");
    const string fileNameM = pathTest("testQEmask_mask.nc");

    CDMReader_p readerD = CDMFileReaderFactory::create("netcdf", fileNameD);
    std::shared_ptr<CDMQualityExtractor> mask = std::make_shared<CDMQualityExtractor>(readerD, "", fileNameX);

    DataPtr sliceM = mask->getDataSlice("salt", 0);
    TEST4FIMEX_CHECK(sliceM);

    const int NXSI = 21, NETA = 16, N_SRHO=35;
    TEST4FIMEX_CHECK_EQ(sliceM->size(), N_SRHO * NXSI * NETA);
    auto valuesM = sliceM->asDouble();
    const int offset0 = 0, offset1 = 12+NXSI*12;
    TEST4FIMEX_CHECK(valuesM[offset0] > 1e36);
    TEST4FIMEX_CHECK(fabs(valuesM[offset1] - 35.114) < 0.001);
}
#endif /* HAVE_NETCDF_H */
