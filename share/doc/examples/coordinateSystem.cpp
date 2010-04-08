#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/NetCDF_CDMReader.h"

int main(int argc, char* args[]) {
    boost::shared_ptr<CDMReader> reader(new NetCDF_CDMReader("coordTest.nc"));
    const CDM& cdm = reader->getCDM();

    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(cdm);
    // find an appropriate coordinate system for a variable
    string varName = "altitude";
    vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
            find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt != coordSys.end()) {
        if ((*varSysIt)->isSimpleSpatialGridded()) {
            CoordinateSystem::ConstAxisPtr xAxis = (*varSysIt)->getGeoXAxis(); // X or Lon
            CoordinateSystem::ConstAxisPtr yAxis = (*varSysIt)->getGeoYAxis(); // Y or Lat
            CoordinateSystem::ConstAxisPtr tAxis = (*varSysIt)->getTimeAxis(); // time
            // create a slice-builder for the variable
            // the slicebuilder starts with the maximum variable size
            SliceBuilder sb(cdm, altitude);
            // select 3-7 y-points
            sb.setStartAndSize(yAxis, 3, 5);
            // select the 2nd time slice
            sb.setStartAndSize(tAxis, 2, 2);

            // fetch the data
            boost::shared_ptr<Data> data = reader->getDataSlice(altitude, sb);
            /* do something with the data */
        }
    }
    return 0;
}

