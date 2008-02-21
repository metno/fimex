#include "CDMInterpolator.h"

#include <boost/regex.hpp>
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

void CDMInterpolator::changeProjection(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit) throw(CDMException)
{
	cdm = dataReader->getCDM(); // reset previous changes
	
	// detect original projection and axes
	std::string orgProjection;
	std::string orgXAxis;
	std::string orgYAxis;
	boost::shared_ptr<Data> orgXAxisVals, orgYAxisVals;
	std::string orgXAxisUnits, orgYAxisUnits;
	getProjectionAndAxesFromCDM(cdm, orgProjection, orgXAxis, orgYAxis, orgXAxisVals, orgYAxisVals, orgXAxisUnits, orgYAxisUnits);
	
	// get the new projection
	std::string newProj;
	boost::smatch what;
	if (boost::regex_search(proj_input, what, boost::regex("\\+proj=(\\S+)"))) {
		newProj = what[1].str();
	} else {
		throw CDMException("cannot find +proj=... in proj-string: " + proj_input);
	}
	
	// remove projection and coordinates (lon lat) 
	if (orgProjection != "latlong") {
		cdm.removeVariable(orgProjection);
		std::string var = (cdm.findVariables("grid_mapping", orgProjection))[0];
		if (var != "") {
			std::string coordinates = cdm.getAttribute(var, "coordinates").getStringValue();
			std::string coord1, coord2;
			size_t sepPos = coordinates.find(" ");
			if (sepPos != std::string::npos) {
				coord1 = coordinates.substr(0, sepPos);
				coord2 = coordinates.substr(sepPos+1);
				cdm.removeVariable(coord1);
				cdm.removeVariable(coord2);
			} else {
				throw CDMException("could not find two coordinates in " + coordinates);
			}
		}
	}
	
	
	
	// add new projection and parameters
	std::string newProjection = "latlong";
	if (newProj != "latlong") {
		newProjection = "projection_"+newProjection;
		cdm.addVariable(CDMVariable(newProjection, CDM_NAT, std::vector<std::string>()));
		std::vector<CDMAttribute> projAttrs = projStringToAttributes(proj_input);
		for (std::vector<CDMAttribute>::iterator it = projAttrs.begin(); it != projAttrs.end(); ++it) {
			cdm.addAttribute(newProjection, *it);
		}
	}
	
	// change/add new axes
	// don't change the name of the dimension, even if this might look strange if e.g. lon is a projection_x_coordinate
	std::string xStandardName;
	std::string yStandardName;
	if (newProj == "latlong") {
		xStandardName = "longitude";
		yStandardName = "latitude";
	} else if (newProjection == "projection_rotated_latitude_longitude") {
		xStandardName = "grid_longitude";
		yStandardName = "grid_latitude";
	} else {
		xStandardName = "projection_x_coordinate";
		yStandardName = "projection_y_coordinate";
	}
	cdm.removeAttribute(orgXAxis, "long_name");
	cdm.removeAttribute(orgYAxis, "long_name");
	cdm.addOrReplaceAttribute(orgXAxis, CDMAttribute("standard_name", xStandardName);
	cdm.addOrReplaceAttribute(orgYAxis, CDMAttribute("standard_name", yStandardName);
	
	
	
	// TODO change original projection axes from x,y <-> lon, lat if changing from proj <-> latlon
	// TODO change all variables with projection (grid_mapping) to match new axes
	// TODO store projection changes to be used in data-section
}


}
