/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
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
#include "FeltCDMReader2.h"
#include "FeltParameters.h"
#include "Felt_Array2.h"
#include "Felt_File2.h"

#include "FeltGridDefinition.h"

#include <array>
#include <cassert>
#include <cmath>
#include <ctime>

#include "fimex/Data.h"
#include "fimex/CDM.h"

using namespace std;
using namespace MetNoFelt;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_feltparameter)
{
    FeltParameters fp = FeltParameters(pathTest("diana.setup"));
    const std::array<short, 16> tksoil = fp.getParameters(string("tksoil"));
    TEST4FIMEX_CHECK_EQ(tksoil[10], 2);
    TEST4FIMEX_CHECK_EQ(tksoil[11], 29);
    TEST4FIMEX_CHECK_EQ(tksoil[12], 1000);
    TEST4FIMEX_CHECK_EQ(fp.getParameterName(tksoil), "tksoil");
    const std::array<short, 16> lameps_prob_t2m = fp.getParameters(string("lameps_prob_t2m>+30"));
    TEST4FIMEX_CHECK_EQ(lameps_prob_t2m[10], 2);
    TEST4FIMEX_CHECK_EQ(lameps_prob_t2m[11], 115);
    TEST4FIMEX_CHECK_EQ(lameps_prob_t2m[12], 1000);
    TEST4FIMEX_CHECK_EQ(fp.getParameterName(lameps_prob_t2m), "lameps_prob_t2m>+30");
}

TEST4FIMEX_TEST_CASE(test_feltfile)
{
    if (!hasTestExtra())
        return;
    typedef vector<FimexTime> timeVec;
    //defaultLogLevel(Logger::DEBUG);

    Felt_File2 ff(pathTestExtra("flth00.dat"), pathTest("diana.setup"));
    vector<std::shared_ptr<Felt_Array2>> vec = ff.listFeltArrays();
    Felt_Array2& fa = *(ff.getFeltArray("u10m").get());
    vector<LevelPair> levels = fa.getLevelPairs();
    TEST4FIMEX_CHECK_EQ((fa.getX() * fa.getY()), 44884);
    vector<short> data(fa.getX()*fa.getY());
    fa.getGrid(fa.getTimes().at(50), levels.at(0), data);
    TEST4FIMEX_CHECK_EQ(levels.size(), 1);
    TEST4FIMEX_CHECK_EQ(fa.getName(), "u10m");
    TEST4FIMEX_CHECK_EQ(fa.getTimes().size(), 61);
    TEST4FIMEX_CHECK_EQ(fa.getX(), 229);
    TEST4FIMEX_CHECK_EQ(fa.getY(), 196);
    TEST4FIMEX_CHECK(std::abs(fa.getScalingFactor() - 0.001) < 1e-6);

    TEST4FIMEX_CHECK_THROW(ff.getFeltArray("this parameter is intentionally unknown"), Felt_File_Error);

    TEST4FIMEX_CHECK_EQ(static_cast<int>(data.size()), (fa.getX() * fa.getY()));
    // u10m not defined on border
    TEST4FIMEX_CHECK_EQ(data[0], ANY_VALUE());
    TEST4FIMEX_CHECK_EQ(data[10000], 820);
    TEST4FIMEX_CHECK_EQ(data[20000], 8964);

    // check consistency of Felt_File and Felt_Array for time
    timeVec ff_times = ff.getFeltTimes();
    timeVec fa_times = fa.getTimes();
    for (timeVec::iterator it = fa_times.begin(); it < fa_times.end(); ++it) {
        if (find(ff_times.begin(), ff_times.end(), *it) == ff_times.end()) {
            TEST4FIMEX_FAIL("time '" << make_time_string_extended(*it) << "' not found");
        }
    }

    // check consistency of Felt_File and Felt_Array for levels
    vector<LevelPair> ff_levels = ff.getFeltLevelPairs()[fa.getLevelType()];
    vector<LevelPair> fa_levels = fa.getLevelPairs();
    for (vector<LevelPair>::iterator it = fa_levels.begin(); it < fa_levels.end(); ++it) {
        if (find(ff_levels.begin(), ff_levels.end(), *it) == ff_levels.end()) {
            TEST4FIMEX_FAIL("level " << it->first << "/" << it->second << " not found");
        }
    }
}

TEST4FIMEX_TEST_CASE(test_felt_axis)
{
    if (!hasTestExtra())
        return;
    Felt_File2 ff(pathTestExtra("flth00.dat"), pathTest("diana.setup"));
    // gridPar needs to be a copy here, since the reference of gridPar will disappear with the gridDefinition
    std::array<float, 6> gridPar = ff.getGridDefinition()->getGridParameters();
    TEST4FIMEX_CHECK_EQ(ff.getGridType(), 1);

    DataPtr xdata = ff.getXData();
    TEST4FIMEX_CHECK(fabs((xdata->asFloat())[(int)gridPar[0] - 1]) < .5);
    TEST4FIMEX_CHECK_EQ((xdata->asInt())[(int)gridPar[0]], 50162); // tis is 50162.2...
    TEST4FIMEX_CHECK(fabs((ff.getYData()->asFloat())[(int)gridPar[1] - 1]) < .5);
    TEST4FIMEX_CHECK_EQ((ff.getYData()->asInt())[(int)gridPar[1]], 50162); // tis is 50162.2...
}

TEST4FIMEX_TEST_CASE(test_felt_cdm_reader)
{
    if (!hasTestExtra())
        return;
    const string fileName = pathTestExtra("flth00.dat");
    FeltCDMReader2 feltCDM(fileName, pathShareEtc("felt2nc_variables.xml"));
    string projName, projXAxis, projYAxis, projXUnit, projYUnit;
    feltCDM.getCDM().getProjectionAndAxesUnits(projName, projXAxis, projYAxis, projXUnit, projYUnit);
    TEST4FIMEX_CHECK_EQ(projName, "projection_1");
    TEST4FIMEX_CHECK_EQ(projXAxis, "x");
    TEST4FIMEX_CHECK_EQ(projYAxis, "y");
    TEST4FIMEX_CHECK_EQ(projXUnit, "m");
    TEST4FIMEX_CHECK_EQ(projYUnit, "m");
    DataPtr xVals = feltCDM.getData(projXAxis);
    DataPtr yVals = feltCDM.getData(projYAxis);
    TEST4FIMEX_CHECK_EQ(xVals->size(), 229);
    TEST4FIMEX_CHECK_EQ(yVals->size(), 196);

    const CDMAttribute& attr = feltCDM.getCDM().getAttribute(feltCDM.getCDM().globalAttributeNS(), "min_time");
    TEST4FIMEX_CHECK_EQ(attr.getStringValue().substr(0, 4), "2007");

    FeltCDMReader2 feltCDM2(fileName, pathTest("felt2nc_variables_level1000.xml"));
    // without level restrictions:
    TEST4FIMEX_CHECK_EQ(feltCDM.getData("sigma")->size(), 4);
    // with level restrictions
    TEST4FIMEX_CHECK_EQ(feltCDM2.getData("sigma")->size(), 1);
}
