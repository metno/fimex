/*
  Fimex, test/testTimeAxisRead.cc

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/

#include <iostream>
#include <numeric>
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/SliceBuilder.h"
#include "fimex/Data.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/CDM.h"
#include "fimex/CDMReaderUtils.h"
#include <boost/date_time/posix_time/posix_time.hpp>

int main(int argc, char* argv[])
{
    using namespace std;
    using namespace MetNoFimex;
	using namespace boost::posix_time;

    //string fileName("/opdata/arome_norway25/AROME_Norway25_00.nc");
    string fileName("/disk1/Fimex/wrfout_d01_2012-12-04_00:00:00.nc");
    CDMReader_p reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileName));

	vector<double> lonVals(1, 90.);
	vector<double> latVals(1, 23.);

        std::shared_ptr<CDMInterpolator> interpol(new CDMInterpolator(reader));
        interpol->changeProjection(MIFI_INTERPOL_BILINEAR, lonVals, latVals);
        cerr << interpol->getCDM().getTimeAxis("U") << endl;
        cerr << to_iso_string(MetNoFimex::getUniqueForecastReferenceTime(reader)) << endl;
        return 0;
}
