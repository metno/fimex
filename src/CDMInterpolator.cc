/*
 * Fimex
 * 
 * (C) Copyright 2008, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

#include "CDMInterpolator.h"

#include <boost/regex.hpp>
#include "interpolation.h"
#include "DataImpl.h"

namespace MetNoFimex
{

using namespace std;

const int DEBUG = 0;

CDMInterpolator::CDMInterpolator(boost::shared_ptr<CDMReader> dataReader)
: dataReader(dataReader), latitudeName("lat"), longitudeName("lon")
{
	cdm = dataReader->getCDM();
}

CDMInterpolator::~CDMInterpolator()
{
}

const boost::shared_ptr<Data> CDMInterpolator::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
	const CDMVariable& variable = cdm.getVariable(varName);
	if (variable.hasData()) {
		return getDataFromMemory(variable, unLimDimPos);
	}
	if (std::find(projectionVariables.begin(), projectionVariables.end(), variable.getName()) == projectionVariables.end()) {
		// no projection, just forward
		return dataReader->getDataSlice(varName, unLimDimPos);
	} else {
		// TODO: handle fillValues, scaling?, datatypes?
		boost::shared_ptr<Data> data = cachedInterpolation.interpolateValues(dataReader->getDataSlice(varName, unLimDimPos), cdm.getFillValue(varName)); 
		if (variable.isSpatialVector()) {
			// TODO: the current implementation is sub-optimal since it will fetch and transpose all vector-data twice (once for each direction)
			const std::string& counterpart = variable.getSpatialVectorCounterpart();
			boost::shared_ptr<Data> counterpartData = cachedInterpolation.interpolateValues(dataReader->getDataSlice(counterpart, unLimDimPos));
			const std::string& direction = variable.getSpatialVectorDirection();
			if (direction.find("x") != string::npos || direction.find("longitude") != string::npos) {
				cachedVectorReprojection.reprojectValues(data, counterpartData, cdm.getFillValue(varName), cdm.getFillValue(counterpart));
			} else if (direction.find("y") != string::npos || direction.find("latitude") != string::npos) {
				cachedVectorReprojection.reprojectValues(counterpartData, data, cdm.getFillValue(counterpart), cdm.getFillValue(varName));
			} else {
				throw CDMException("could not find x,longitude,y,latitude direction for vector: " + varName + ", direction: " + direction);
			}
		}
		return data;
	}
}

static void degreeToRad(double& val) {
	val *= DEG_TO_RAD;
}

void CDMInterpolator::changeProjection(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit) throw(CDMException)
{
	cdm = dataReader->getCDM(); // reset previous changes
	projectionVariables.assign(0, ""); // reset variables
	
	// detect original projection and axes
	std::string orgProjection;
	std::string orgXAxis;
	std::string orgYAxis;
	std::string orgXAxisUnits, orgYAxisUnits;
	cdm.getProjectionAndAxesUnits(orgProjection, orgXAxis, orgYAxis, orgXAxisUnits, orgYAxisUnits);
	boost::shared_ptr<Data> orgXAxisVals = dataReader->getDataSlice(orgXAxis);
	boost::shared_ptr<Data >orgYAxisVals = dataReader->getDataSlice(orgYAxis);
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
		CDMVariable projVar(newProjection, CDM_NAT, std::vector<std::string>());
		projVar.setData(createData(CDM_NAT, 0)); // define empty data
		cdm.addVariable(projVar);
		std::vector<CDMAttribute> projAttrs = projStringToAttributes(proj_input);
		for (std::vector<CDMAttribute>::iterator it = projAttrs.begin(); it != projAttrs.end(); ++it) {
			cdm.addAttribute(newProjection, *it);
		}
	}

	if (DEBUG) {
		std::cerr << "original projection: " << orgProjection << std::endl;
		std::cerr << "new projection: " << newProjection << std::endl;
		std::cerr << "new proj: " << newProj << std::endl;
	}

	
	// change/add new axes
	// don't change the name of the dimension, even if this might look strange if e.g. lon is a projection_x_coordinate
	cdm.removeAttribute(orgXAxis, "long_name");
	cdm.removeAttribute(orgYAxis, "long_name");
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
		//TODO: the following lines are required by damocles and should be moved to an output
		// changer for netcdf. remove here when output changer is in place
		cdm.addOrReplaceAttribute(orgXAxis, CDMAttribute("long_name", "x-coordinate in Cartesian system"));
		cdm.addOrReplaceAttribute(orgYAxis, CDMAttribute("long_name", "y-coordinate in Cartesian system"));
	}
	cdm.addOrReplaceAttribute(orgXAxis, CDMAttribute("standard_name", xStandardName));
	cdm.addOrReplaceAttribute(orgYAxis, CDMAttribute("standard_name", yStandardName));
	cdm.addOrReplaceAttribute(orgXAxis, CDMAttribute("units", out_x_axis_unit));
	cdm.addOrReplaceAttribute(orgYAxis, CDMAttribute("units", out_y_axis_unit));
	cdm.getVariable(orgXAxis).setData(createData(CDM_DOUBLE, out_x_axis.size(), out_x_axis.begin(), out_x_axis.end()));
	cdm.getVariable(orgYAxis).setData(createData(CDM_DOUBLE, out_y_axis.size(), out_y_axis.begin(), out_y_axis.end()));
	
	cdm.getDimension(orgXAxis).setLength(out_x_axis.size());
	cdm.getDimension(orgYAxis).setLength(out_y_axis.size());
	
	std::string lat(getLatitudeName());
	std::string lon(getLongitudeName());
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
			if (newProj != "latlong" && (*varIt != lat) && (*varIt != lon)) {
				cdm.addOrReplaceAttribute(*varIt, CDMAttribute("coordinates", lon + " " + lat));
				cdm.addOrReplaceAttribute(*varIt, CDMAttribute("grid_mapping", newProjection));
			} else {
				cdm.removeAttribute(*varIt, "coordinates");
				cdm.removeAttribute(*varIt, "grid_mapping");
			}
		}
	}
	// store projection changes to be used in data-section
	// translate temporary new axes from deg2rad if required
	vector<double> outXAxis = out_x_axis;
	vector<double> outYAxis = out_y_axis;
	boost::regex degree(".*degree.*");
	if (boost::regex_match(out_x_axis_unit, degree)) {
		for_each(outXAxis.begin(), outXAxis.end(), degreeToRad);
	}
	if (boost::regex_match(out_y_axis_unit, degree)) {
		for_each(outYAxis.begin(), outYAxis.end(), degreeToRad);
	}
	
	size_t fieldSize = outXAxis.size() * outYAxis.size();
	vector<double> pointsOnXAxis(fieldSize);
	vector<double> pointsOnYAxis(fieldSize);
	std::string orgProjStr = attributesToProjString(dataReader->getCDM().getAttributes(orgProjection));
	if (MIFI_OK != mifi_project_axes(proj_input.c_str(), orgProjStr.c_str(), &outXAxis[0], &outYAxis[0], outXAxis.size(), outYAxis.size(), &pointsOnXAxis[0], &pointsOnYAxis[0])) {
		throw CDMException("unable to project axes from "+orgProjStr+ " to " +proj_input.c_str());
	}

	// translate original axes from deg2rad if required
	int miupXAxis = MIFI_PROJ_AXIS;
	int miupYAxis = MIFI_PROJ_AXIS;
	boost::shared_array<double> orgXAxisValsArray = orgXAxisVals->asDouble();
	boost::shared_array<double> orgYAxisValsArray = orgYAxisVals->asDouble();
	if (boost::regex_match(orgXAxisUnits, degree)) {
		miupXAxis = MIFI_LONGITUDE;
		for_each(&(orgXAxisValsArray.get())[0], &(orgXAxisValsArray.get())[orgXAxisVals->size()], degreeToRad);
	}
	if (boost::regex_match(orgYAxisUnits, degree)) {
		miupYAxis = MIFI_LATITUDE;
		for_each(&(orgYAxisValsArray.get())[0], &(orgYAxisValsArray.get())[orgYAxisVals->size()], degreeToRad);
	}	
	// translate coordinates (in rad or m) to indices
	mifi_points2position(&pointsOnXAxis[0], fieldSize, orgXAxisValsArray.get(), orgXAxisVals->size(), miupXAxis);
	mifi_points2position(&pointsOnYAxis[0], fieldSize, orgYAxisValsArray.get(), orgYAxisVals->size(), miupYAxis);
	
	cachedInterpolation = CachedInterpolation(method, pointsOnXAxis, pointsOnYAxis, orgXAxisVals->size(), orgYAxisVals->size(), out_x_axis.size(), out_y_axis.size());

	// prepare interpolation of vectors
	// TODO: only prepare if at least one vector exists
	boost::shared_array<double> matrix(new double[out_x_axis.size() * out_y_axis.size() * 4]);
	mifi_get_vector_reproject_matrix(orgProjStr.c_str(), proj_input.c_str(), &out_x_axis[0], &out_y_axis[0], miupXAxis, miupYAxis, out_x_axis.size(), out_y_axis.size(), matrix.get());
	cachedVectorReprojection = CachedVectorReprojection(MIFI_VECTOR_KEEP_SIZE, matrix, out_x_axis.size(), out_y_axis.size());
}

}
