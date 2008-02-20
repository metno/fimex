#include "CDMInterpolator.h"
#include "interpolation.h"

namespace MetNoUtplukk
{

CDMInterpolator::CDMInterpolator(boost::shared_ptr<CDMReader> dataReader)
: dataReader(dataReader)
{
	cdm = dataReader->getCDM();
}

CDMInterpolator::~CDMInterpolator()
{
}

const boost::shared_ptr<Data> CDMInterpolator::getDataSlice(const CDMVariable& variable, size_t unLimDimPos) throw(CDMException)
{
	
}

void changeProjection(int method, const string& proj_input, const vector<double> out_x_axis, const vector<double> out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit) throw(CDMException)
{
	// TODO detect original projection (via grid_mapping/grid_mapping_name)
	//      change projection to new projection parameters
	//      remove/add long/lat if required (new/old latlong) 
	//      detect original projection axes (x,y,lon,lat,rlat,rlon) (via projection_x/y_coordinate, degrees_east/north, grid_longitude/latitutde)
	//      change original projection axes from x,y <-> lon, lat if changing from proj <-> latlon
	//      change all variables with projection (grid_mapping) to match new axes
	//      store projection changes to be used in data-section
}


}
