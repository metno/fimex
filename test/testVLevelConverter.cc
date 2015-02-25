/*
 * Fimex, testVLevelConverter.cc
 *
 * (C) Copyright 2015, met.no
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
 *  Created on: Feb 25, 2015
 */

#include "../config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>
using boost::unit_test_framework::test_suite;

#include <fstream>

#include "fimex/CDMFileReaderFactory.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"
#include "fimex/Logger.h"

using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE(test_pressure_integrator)
{
    std::string topSrcDir(TOP_SRCDIR);
    std::string fileName(topSrcDir+"/test/testdata_arome_vc.nc");
    if (!std::ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }

    typedef boost::shared_ptr<CDMReader> CDMReader_p;
    typedef boost::shared_ptr<const CoordinateSystem> CoordinateSystem_p;
    typedef std::vector<CoordinateSystem_p> CoordinateSystem_pv;
    typedef boost::shared_ptr<const VerticalTransformation> VerticalTransformation_cp;
    typedef boost::shared_ptr<ToVLevelConverter> ToVLevelConverter_p;

    CDMReader_p reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileName));
    CoordinateSystem_pv coordinateSystems = MetNoFimex::listCoordinateSystems(reader);
    CoordinateSystem_pv::const_iterator it =
            std::find_if(coordinateSystems.begin(), coordinateSystems.end(),
                    MetNoFimex::CompleteCoordinateSystemForComparator("x_wind_ml"));

    BOOST_CHECK(it != coordinateSystems.end());
    CoordinateSystem_p cs = *it;

    VerticalTransformation_cp vt = cs->getVerticalTransformation();
    BOOST_CHECK(vt);

    ToVLevelConverter_p pressc = vt->getConverter(reader, MIFI_VINT_PRESSURE, 0, cs);
    BOOST_CHECK(pressc);

    const std::vector<double> pressures = (*pressc)(1, 0, 0);
    BOOST_CHECK(pressures.size() == 65);
    BOOST_CHECK_CLOSE(10, pressures[0], 1);
    BOOST_CHECK_CLOSE(980, pressures[64], 1);


    ToVLevelConverter_p altic = vt->getConverter(reader, MIFI_VINT_ALTITUDE, 0, cs);
    BOOST_CHECK(altic);

    const std::vector<double> altitudes = (*altic)(1, 0, 0);
    BOOST_CHECK(altitudes.size() == 65);
    BOOST_CHECK_CLOSE(29910, altitudes[0], 1);
    BOOST_CHECK_CLOSE(23210, altitudes[1], 1);
    BOOST_CHECK_CLOSE(198, altitudes[63], 1);
    BOOST_CHECK_CLOSE(173, altitudes[64], 1);
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif

