/*
  Fimex, test/testPerformanceRead.cc

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

int main(int argc, char* argv[])
{
    using namespace std;
    using namespace MetNoFimex;
    string fileName("/opdata/arome_norway25/AROME_Norway25_00.nc");
    //string fileName("/disk1/Fimex/AROME_Norway25_00_d1.nc");
    CDMReader_p reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileName));

    std::shared_ptr<Data> data;
    for (int i = 0; i < 67; ++i) {
        data = reader->getScaledDataSliceInUnit("air_temperature_pl", "mK", i);
    }
    boost::shared_array<double> da = data->asDouble();
    double sum = accumulate(&da[0], &da[0]+data->size(), 0.);
    cerr << "sum: " << sum << endl;
    return 0;
}
