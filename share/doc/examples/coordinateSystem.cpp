#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/CoordinateSystemSliceBuilder.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMReader.h"

using namespace MetNoFimex;
using namespace std;

int main(int argc, char* args[]) {
    boost::shared_ptr<CDMReader> reader = CDMFileReaderFactory::create("coordTest.nc", "netcdf");
    //boost::shared_ptr<CDMReader> reader(new NetCDF_CDMReader("coordRefTimeTest.nc"));
    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(reader);
    const CDM& cdm = reader->getCDM();

    // find an appropriate coordinate system for a variable
    string varName = "air_temperature";
    vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
            find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt != coordSys.end()) {
        if ((*varSysIt)->isSimpleSpatialGridded()) {
            CoordinateSystem::ConstAxisPtr xAxis = (*varSysIt)->getGeoXAxis(); // X or Lon
            CoordinateSystem::ConstAxisPtr yAxis = (*varSysIt)->getGeoYAxis(); // Y or Lat
            CoordinateSystem::ConstAxisPtr tAxis = (*varSysIt)->getTimeAxis(); // time

            CoordinateSystemSliceBuilder sb(cdm, *varSysIt);
            // handling of time
            if (tAxis.get() != 0) {
                // time-Axis, eventually multi-dimensional, i.e. forecast_reference_time
                if ((*varSysIt)->hasAxisType(CoordinateAxis::ReferenceTime)) {
                    CoordinateSystem::ConstAxisPtr rtAxis = (*varSysIt)->findAxisOfType(CoordinateAxis::ReferenceTime);
                    DataPtr refTimes = reader->getScaledDataInUnit(rtAxis->getName(),"seconds since 1970-01-01 00:00:00");
                    /* do something with the refTimes and select the wanted Position */
                    size_t refTimePos = 3; /* or whatever you select between 0 (default) and refTimes->size()-1 */
                    sb.setReferenceTimePos(refTimePos);
                }
                DataPtr times = reader->getDataSlice(tAxis->getName(), sb.getTimeVariableSliceBuilder());
                /* select the desired startTime and the sice for the time-slices */
                // fetch the 2nd and 3rd time-step of the 4th run
                sb.setTimeStartAndSize(1, 2); // default is all of ReferenceTimePos
            }


            // further selection of data
            // select 3-7 y-points
            sb.setStartAndSize(yAxis, 3, 5);
            sb.setAll(xAxis);

            // by default, all other dimensions are fetched at maximum size
            // here, I reduce them to the first slice
            vector<string> dims = sb.getUnsetDimensionNames();
            for (vector<string>::iterator dim = dims.begin(); dim != dims.end(); ++dim) {
                sb.setStartAndSize(*dim, 0, 1);
            }

            // fetch the data
            DataPtr data = reader->getDataSlice(varName, sb);
            /* do something with the data */
        }
    }
    return 0;
}

