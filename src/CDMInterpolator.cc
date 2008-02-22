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
		newProjection = "projection_"+newProj;
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
	cdm.addOrReplaceAttribute(orgXAxis, CDMAttribute("standard_name", xStandardName));
	cdm.addOrReplaceAttribute(orgYAxis, CDMAttribute("standard_name", yStandardName));
	cdm.getVariable(orgXAxis).setData(createData(CDM_DOUBLE, out_x_axis.size(), out_x_axis.begin(), out_x_axis.end()));
	cdm.getVariable(orgYAxis).setData(createData(CDM_DOUBLE, out_y_axis.size(), out_y_axis.begin(), out_y_axis.end()));
	
	cdm.getDimension(orgXAxis).setLength(out_x_axis.size());
	cdm.getDimension(orgYAxis).setLength(out_y_axis.size());
	
	std::string lat("lat");
	std::string lon("lon");
	if (newProj != "latlong") {
		generateProjectionCoordinates(cdm, newProjection, orgXAxis, orgYAxis, lon, lat);
	}
	
	// change all variable attributes grid_mapping and coordinates
	{
		// mapping all variables with matching orgX/orgY dimensions
		std::vector<std::string> dims;
		std::map<std::string, std::string> attrs;
		dims.push_back(orgXAxis);
		dims.push_back(orgYAxis);
		std::vector<std::string> projVars = cdm.findVariables(attrs, dims);
		for (std::vector<std::string>::iterator varIt = projVars.begin(); varIt != projVars.end(); ++varIt) {
			if (newProj != "latlong") {
				cdm.addOrReplaceAttribute(*varIt, CDMAttribute("coordinates", lon + " " + lat));
				cdm.addOrReplaceAttribute(*varIt, CDMAttribute("grid_mapping", newProjection));
			} else {
				cdm.removeAttribute(*varIt, "coordinates");
				cdm.removeAttribute(*varIt, "grid_mapping");
			}
		}
	}
	// TODO store projection changes to be used in data-section
}


}
