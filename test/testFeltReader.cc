/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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

#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#include <iostream>
#include <fstream>
#include <boost/array.hpp>
#include <cassert>
#include <ctime>
#include <cmath>
#include "fimex/FeltParameters.h"
#include "fimex/Felt_Array2.h"
#include "fimex/Felt_File2.h"
#include "fimex/FeltCDMReader2.h"
#include "felt/FeltGridDefinition.h"
#include <boost/date_time/posix_time/posix_time.hpp>

#include "fimex/Data.h"
#include "fimex/CDM.h"

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

using namespace std;
using namespace MetNoFelt;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_feltparameter )
{
	string topSrcDir(TOP_SRCDIR);
	FeltParameters fp = FeltParameters(topSrcDir + "/test/diana.setup");
	const boost::array<short, 16>  tksoil = fp.getParameters(string("tksoil"));
	//cout << "tksoil:" << tksoil[10] << ":" << tksoil[11] << ":" << tksoil[12] << endl;
	BOOST_CHECK(tksoil[10] == 2);
	BOOST_CHECK(tksoil[11] == 29);
	BOOST_CHECK(tksoil[12] == 1000);
	//cout << "reverse lookup: " << fp.getParameterName(tksoil) << endl;
	BOOST_CHECK(fp.getParameterName(tksoil) == "tksoil");
	const boost::array<short, 16>  lameps_prob_t2m = fp.getParameters(string("lameps_prob_t2m>+30"));
	//cout << "lameps_prob_t2m>+30:" << lameps_prob_t2m[10] << ":" << lameps_prob_t2m[11] << ":" << lameps_prob_t2m[12] << endl;
	BOOST_CHECK(lameps_prob_t2m[10] == 2);
	BOOST_CHECK(lameps_prob_t2m[11] == 115);
	BOOST_CHECK(lameps_prob_t2m[12] == 1000);
	//cout << "reverse lookup: " << fp.getParameterName(lameps_prob_t2m) << endl;
	BOOST_CHECK(fp.getParameterName(lameps_prob_t2m) == "lameps_prob_t2m>+30");
	//boost::array<short, 16> special = { {88, 1905, 2007, 516, 0, 20684, 1, 12137, 3, 0, 2, 58, 1000, 0, 1, 101} };
	//cout << "special array is: " << fp.getParameterName(special) << endl;
}

BOOST_AUTO_TEST_CASE( test_feltfile )
{
	string topSrcDir(TOP_SRCDIR);
	string fileName(topSrcDir+"/test/flth00.dat");
	if (!ifstream(fileName.c_str())) {
		// no testfile, skip test
		return;
	}
    typedef vector<boost::posix_time::ptime> timeVec;
    //defaultLogLevel(Logger::DEBUG);

	Felt_File2 ff(fileName);
	vector<boost::shared_ptr<Felt_Array2> > vec = ff.listFeltArrays();
    Felt_Array2& fa = *(ff.getFeltArray("u10m").get());
    vector<LevelPair> levels = fa.getLevelPairs();
    BOOST_CHECK((fa.getX() * fa.getY()) == 44884);
    vector<short> data(fa.getX()*fa.getY());
    int scaleFacter = fa.getGrid(fa.getTimes().at(50), levels.at(0), data);
    BOOST_CHECK( levels.size() == 1 );
	BOOST_CHECK( fa.getName() == "u10m" );
	BOOST_CHECK( fa.getTimes().size() == 61);
	//cout << fa.getX() << "x" << fa.getY() << ": " << fa.getScalingFactor() << endl;
	BOOST_CHECK( fa.getX() == 229 );
	BOOST_CHECK( fa.getY() == 196 );
	BOOST_CHECK( std::abs(fa.getScalingFactor() - 0.001) < 1e-6 );

	try {
		ff.getFeltArray("this parameter is intentionally unknown");
		BOOST_CHECK(false); // should never reach this line
	} catch (Felt_File_Error& ffe) {
		//cout << ffe.toString() << endl;
		BOOST_CHECK(true);
	}
	//FeltParameters xx("/home/heikok/bla/test");

	BOOST_CHECK(static_cast<int>(data.size()) == (fa.getX()*fa.getY()));
	// u10m not defined on border
	//cerr << "data: " << data[1000] << " " << data[10000] << " " << data[20000] << endl;
	BOOST_CHECK(data[0] == ANY_VALUE());
	BOOST_CHECK(data[10000] == 820);
	BOOST_CHECK(data[20000] == 8964);


	// check consistency of Felt_File and Felt_Array for time
	timeVec ff_times = ff.getFeltTimes();
	timeVec fa_times = fa.getTimes();
	for (timeVec::iterator it = fa_times.begin(); it < fa_times.end(); ++it) {
		if (find(ff_times.begin(), ff_times.end(), *it) == ff_times.end()) {
			BOOST_CHECK(false); // not found
		}
	}

    // check consistency of Felt_File and Felt_Array for levels
	vector<LevelPair> ff_levels = ff.getFeltLevelPairs()[fa.getLevelType()];
    vector<LevelPair> fa_levels = fa.getLevelPairs();
    for (vector<LevelPair>::iterator it = fa_levels.begin(); it < fa_levels.end(); ++it) {
        if (find(ff_levels.begin(), ff_levels.end(), *it) == ff_levels.end()) {
            BOOST_CHECK(false); // not found
        }
    }

}

BOOST_AUTO_TEST_CASE( test_felt_axis )
{
	string topSrcDir(TOP_SRCDIR);
	string fileName(topSrcDir+"/test/flth00.dat");
	if (!ifstream(fileName.c_str())) {
		// no testfile, skip test
		return;
	}
	Felt_File2 ff(fileName);
	// gridPar needs to be a copy here, since the reference of gridPar will disappear with the gridDefinition
    boost::array<float, 6> gridPar = ff.getGridDefinition()->getGridParameters();
	BOOST_CHECK(ff.getGridType() == 1);

    boost::shared_ptr<Data> xdata = ff.getXData();
    //cerr << ff.getGridDefinition()->startX() - 5.71849e+06  << " x " << ff.getGridDefinition()->getXIncrement() - 50162.2<< endl;
	//cerr << gridPar[0] << "'" << ((int)gridPar[0]-1) << ": "<< (xdata->asFloat())[(int)gridPar[0]-1] << " x " << (xdata->asFloat())[(int)gridPar[0]-1] << endl;
	BOOST_CHECK(fabs((xdata->asFloat())[(int)gridPar[0]-1]) < .5);
	BOOST_CHECK((xdata->asInt())[(int)gridPar[0]] == 50162); // tis is 50162.2...
	BOOST_CHECK(fabs((ff.getYData()->asFloat())[(int)gridPar[1]-1]) < .5);
	BOOST_CHECK((ff.getYData()->asInt())[(int)gridPar[1]] == 50162); // tis is 50162.2...
}

BOOST_AUTO_TEST_CASE( test_felt_cdm_reader )
{
	string topSrcDir(TOP_SRCDIR);
	string fileName(topSrcDir+"/test/flth00.dat");
	if (!ifstream(fileName.c_str())) {
		// no testfile, skip test
		return;
	}
    FeltCDMReader2 feltCDM(fileName, topSrcDir + "/share/etc/felt2nc_variables.xml");
    //feltCDM.getCDM().toXMLStream(std::cerr);
	string projName, projXAxis, projYAxis, projXUnit, projYUnit;
	feltCDM.getCDM().getProjectionAndAxesUnits(projName, projXAxis, projYAxis, projXUnit, projYUnit);
	BOOST_CHECK(projName == "projection_1");
	BOOST_CHECK(projXAxis == "x");
	BOOST_CHECK(projYAxis == "y");
	BOOST_CHECK(projXUnit == "m");
	BOOST_CHECK(projYUnit == "m");
	boost::shared_ptr<Data> xVals = feltCDM.getData(projXAxis);
	boost::shared_ptr<Data> yVals = feltCDM.getData(projYAxis);
	BOOST_CHECK(xVals->size() == 229);
	BOOST_CHECK(yVals->size() == 196);

	const CDMAttribute& attr = feltCDM.getCDM().getAttribute(feltCDM.getCDM().globalAttributeNS(), "min_time");
	BOOST_CHECK(attr.getStringValue().substr(0,4) == "2007");


    FeltCDMReader2 feltCDM2(fileName, topSrcDir + "/test/felt2nc_variables_level1000.xml");
    // without level restrictions:
    BOOST_CHECK(feltCDM.getData("sigma")->size() == 4);
    // with level restrictions
    BOOST_CHECK(feltCDM2.getData("sigma")->size() == 1);
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
