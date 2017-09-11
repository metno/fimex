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

    boost::shared_ptr<Data> data;
    for (int i = 0; i < 67; ++i) {
        data = reader->getScaledDataSliceInUnit("air_temperature_pl", "mK", i);
    }
    boost::shared_array<double> da = data->asDouble();
    double sum = accumulate(&da[0], &da[0]+data->size(), 0.);
    cerr << "sum: " << sum << endl;
    return 0;
}
