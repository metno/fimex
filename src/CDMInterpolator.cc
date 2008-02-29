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
	if (std::find(projectionVariables.begin(), projectionVariables.end(), variable.getName()) == projectionVariables.end()) {
		// no projection, just forward
		return dataReader->getDataSlice(variable, unLimDimPos);
	} else {
		// TODO: handle fillValues, scaling?, datatypes?
		return cachedInterpolation.interpolateValues(dataReader->getDataSlice(variable, unLimDimPos));
	}
}

void CDMInterpolator::changeProjection(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit) throw(CDMException)
{
	cdm = dataReader->getCDM(); // reset previous changes
	projectionVariables.assign(0, ""); // reset variables
	
	// detect original projection and axes
	std::string orgProjection;
	std::string orgXAxis;
	std::string orgYAxis;
	boost::shared_ptr<Data> orgXAxisVals, orgYAxisVals;
	std::string orgXAxisUnits, orgYAxisUnits;
	cdm.getProjectionAndAxes(orgProjection, orgXAxis, orgYAxis, orgXAxisVals, orgYAxisVals, orgXAxisUnits, orgYAxisUnits);
	
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
		cdm.generateProjectionCoordinates(newProjection, orgXAxis, orgYAxis, lon, lat);
	}
	
	// find all reprojectible variables and change variable attributes grid_mapping and coordinates
	{
		// mapping all variables with matching orgX/orgY dimensions
		std::vector<std::string> dims;
		std::map<std::string, std::string> attrs;
		dims.push_back(orgXAxis);
		dims.push_back(orgYAxis);
		projectionVariables = cdm.findVariables(attrs, dims);
		for (std::vector<std::string>::iterator varIt = projectionVariables.begin(); varIt != projectionVariables.end(); ++varIt) {
			if (newProj != "latlong") {
				cdm.addOrReplaceAttribute(*varIt, CDMAttribute("coordinates", lon + " " + lat));
				cdm.addOrReplaceAttribute(*varIt, CDMAttribute("grid_mapping", newProjection));
			} else {
				cdm.removeAttribute(*varIt, "coordinates");
				cdm.removeAttribute(*varIt, "grid_mapping");
			}
		}
	}
	// store projection changes to be used in data-section
	// TODO: handle axes types (lon lat (deg -> rad))
	size_t fieldSize = out_x_axis.size() * out_x_axis.size();
	vector<double> pointsOnXAxis(fieldSize);
	vector<double> pointsOnYAxis(fieldSize);
	std::string orgProjStr = attributesToProjString(dataReader->getCDM().getAttributes(orgProjection));
	if (MIUP_OK != miup_project_axes(proj_input.c_str(), orgProjStr.c_str(), &out_x_axis[0], &out_y_axis[0], out_x_axis.size(), out_y_axis.size(), &pointsOnXAxis[0], &pointsOnYAxis[0])) {
		throw CDMException("unable to project axes from "+orgProjStr+ " to " +proj_input.c_str());
	}
	// translate coordinates in m to indices
	miup_points2position(&pointsOnXAxis[0], fieldSize, orgXAxisVals->asDouble().get(), orgXAxisVals->size(), MIUP_PROJ_AXIS);
	miup_points2position(&pointsOnYAxis[0], fieldSize, orgYAxisVals->asDouble().get(), orgYAxisVals->size(), MIUP_PROJ_AXIS);

	
	cachedInterpolation = CachedInterpolation(method, pointsOnXAxis, pointsOnYAxis, orgXAxisVals->size(), orgYAxisVals->size(), out_x_axis.size(), out_y_axis.size());
}

}
