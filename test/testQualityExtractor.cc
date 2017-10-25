/*
 * Fimex
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
 */

#include "fimex_config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <boost/make_shared.hpp>
#include <boost/shared_array.hpp>

#ifdef HAVE_FELT
#include "FeltCDMReader2.h"
#endif

#include "fimex/CDMconstants.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMQualityExtractor.h"
#ifdef HAVE_NETCDF_H
#include "fimex/NetCDF_CDMWriter.h"
#else
#include "fimex/Null_CDMWriter.h"
#endif
#include "fimex/Logger.h"
#include "fimex/Data.h"
#include "fimex/mifi_constants.h"

#include "testinghelpers.h"

using namespace std;
using namespace MetNoFimex;

//#define TEST_DEBUG

BOOST_AUTO_TEST_CASE( test_qualityExtract )
{
        if (!hasTestExtra())
            return;
        const string fileName = pathTestExtra("flth00.dat");
	CDMReader_p feltReader(new FeltCDMReader2(fileName, pathShareEtc("felt2nc_variables.xml")));
#ifdef TEST_DEBUG
	defaultLogLevel(Logger::DEBUG);
#endif
	boost::shared_ptr<CDMQualityExtractor> extract(new CDMQualityExtractor(feltReader, "", pathTest("testQualityConfig.xml")));

	map<string, string> statusVariables = extract->getStatusVariable();
	map<string, string> variableFlags = extract->getVariableFlags();
	map<string, vector<double> > variableValues = extract->getVariableValues();
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
	BOOST_CHECK(statusVariables.find("air_temperature") != statusVariables.end());
    BOOST_CHECK(statusVariables["bla"] == "blub");
    BOOST_CHECK(variableFlags.find("bla") == variableFlags.end());
    BOOST_CHECK(variableValues["bla"][2] == 3);
}



BOOST_AUTO_TEST_CASE( test_qualityExtract_convert )
{
    if (!hasTestExtra())
        return;
    const string fileName = pathTestExtra("flth00.dat");
    CDMReader_p feltReader(new FeltCDMReader2(fileName, pathShareEtc("felt2nc_variables.xml")));
    boost::shared_ptr<CDMQualityExtractor> qe(new CDMQualityExtractor(feltReader, "", pathTest("testQualityConfig.xml")));

#ifdef HAVE_NETCDF_H
    string outputFile("test_qualityExtract_convert.nc");
    NetCDF_CDMWriter(qe, outputFile);
    BOOST_CHECK(exists(outputFile));
#else
    Null_CDMWriter(qe, "");
    BOOST_CHECK(true);
#endif /* NETCDF */
}



BOOST_AUTO_TEST_CASE( test_qualityExtract_mask )
{
#ifdef HAVE_NETCDF_H
    const string fileNameD = pathTest("testQEmask_data.nc");
    const string fileNameX = require("testQEmask.xml");
    const string fileNameM = pathTest("testQEmask_mask.nc");

    CDMReader_p readerD = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileNameD);
    boost::shared_ptr<CDMQualityExtractor> mask = boost::make_shared<CDMQualityExtractor>(readerD, "", fileNameX);

    DataPtr sliceM = mask->getDataSlice("salt", 0);
    BOOST_CHECK( sliceM.get() != 0 );
    
    const int NXSI = 21, NETA = 16, N_SRHO=35;
    BOOST_CHECK( sliceM->size() == N_SRHO*NXSI*NETA );
    boost::shared_array<double> valuesM = sliceM->asDouble();
    const int offset0 = 0, offset1 = 12+NXSI*12;
    BOOST_CHECK( valuesM[offset0] > 1e36 );
    BOOST_CHECK( fabs(valuesM[offset1] - 35.114) < 0.001 );
#else
    BOOST_CHECK(true);
#endif /* HAVE_NETCDF_H */
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
