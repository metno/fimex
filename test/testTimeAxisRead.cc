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
    boost::shared_ptr<CDMReader> reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileName));

	vector<double> lonVals(1, 90.);
	vector<double> latVals(1, 23.);

	boost::shared_ptr<CDMInterpolator> interpol(new CDMInterpolator(reader));
    interpol->changeProjection(MIFI_INTERPOL_BILINEAR, lonVals, latVals);
	cerr << interpol->getCDM().getTimeAxis("U") << endl;
	cerr << to_iso_string(MetNoFimex::getUniqueForecastReferenceTime(reader)) << endl;
    return 0;
}
