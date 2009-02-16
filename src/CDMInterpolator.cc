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

#include "fimex/CDMInterpolator.h"

#include <boost/regex.hpp>
#include "fimex/interpolation.h"
#include "fimex/DataImpl.h"
#include "fimex/Logger.h"

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

static boost::shared_array<float> data2InterpolationArray(const boost::shared_ptr<Data>& inData, double badValue) {
	boost::shared_array<float> array = inData->asFloat();
	mifi_bad2nanf(&array[0], &array[inData->size()], badValue);
	return array;
}

// for performance reasons, the iData-reference will be modified and used within the return data
static boost::shared_ptr<Data> interpolationArray2Data(boost::shared_array<float> iData, size_t size, double badValue) {
	mifi_nanf2bad(&iData[0], &iData[size], badValue);
	return boost::shared_ptr<Data>(new DataImpl<float>(iData, size));
}

const boost::shared_ptr<Data> CDMInterpolator::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
	const CDMVariable& variable = cdm.getVariable(varName);
	if (variable.hasData()) {
		return getDataSliceFromMemory(variable, unLimDimPos);
	}
	if (std::find(projectionVariables.begin(), projectionVariables.end(), variable.getName()) == projectionVariables.end()) {
		// no projection, just forward
		return dataReader->getDataSlice(varName, unLimDimPos);
	} else {
		boost::shared_ptr<Data> data = dataReader->getDataSlice(varName, unLimDimPos);
		double badValue = cdm.getFillValue(varName);
		boost::shared_array<float> array = data2InterpolationArray(data, badValue);
		size_t newSize = 0;
		boost::shared_array<float> iArray = cachedInterpolation.interpolateValues(array, data->size(), newSize);
		if (variable.isSpatialVector()) {
			// fetch and transpose vector-data
			// transposing needed once for each direction (or caching, but that needs to much memory)
			const std::string& counterpart = variable.getSpatialVectorCounterpart();
			boost::shared_array<float> counterPartArray = data2InterpolationArray(dataReader->getDataSlice(counterpart, unLimDimPos), cdm.getFillValue(counterpart));
			boost::shared_array<float> counterpartiArray = cachedInterpolation.interpolateValues(counterPartArray, data->size(), newSize);
			const std::string& direction = variable.getSpatialVectorDirection();
			if (direction.find("x") != string::npos || direction.find("longitude") != string::npos) {
				cachedVectorReprojection.reprojectValues(iArray, counterpartiArray, newSize);
			} else if (direction.find("y") != string::npos || direction.find("latitude") != string::npos) {
				cachedVectorReprojection.reprojectValues(counterpartiArray, iArray, newSize);
			} else {
				throw CDMException("could not find x,longitude,y,latitude direction for vector: " + varName + ", direction: " + direction);
			}
		}
		return interpolationArray2Data(iArray, newSize, badValue);
	}
}

static void degreeToRad(double& val) {
	val *= DEG_TO_RAD;
}

void CDMInterpolator::changeProjection(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit) throw(CDMException)
{
	cdm = dataReader->getCDM(); // reset previous changes
	projectionVariables.assign(0, ""); // reset variables
	try {
		changeProjectionByProjectionParameters(method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit);
	} catch (CDMException& ex) {
		LoggerPtr logger = getLogger("fimex.CDMInterpolator.changeProjection");
		LOG4FIMEX(logger, Logger::INFO, "no original projection found, trying coordinate-projection: "<< ex.what());
		changeProjectionByCoordinates(method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit);
	}
}

string getProjectionName(const string& proj_input) {
	// get the new projection
	std::string newProj;
	boost::smatch what;
	if (boost::regex_search(proj_input, what, boost::regex("\\+proj=(\\S+)"))) {
		newProj = what[1].str();
	} else {
		throw CDMException("cannot find +proj=... in proj-string: " + proj_input);
	}
	return newProj;
}

/**
 * make changes in the CDM structure to reflect the new projection (attributes, coordinates, projection-variable, dimensions)
 *
 * @param cmd
 * @param proj_input
 * @param orgProjection
 * @param orgXAxis
 * @param orgYAxis
 */
void changeCDM(CDM& cdm, const string& proj_input, const string& orgProjection, const vector<string>& projectionVariables, const string& orgXAxis, const string& orgYAxis, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit, const string& longitudeName, const string& latitudeName)
{
	string newProj = getProjectionName(proj_input);

	// remove projection and coordinates (lon lat)
	if (orgProjection != "" && orgProjection != "latitude_longitude") {
		cdm.removeVariable(orgProjection);
		std::string var = projectionVariables[0];
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
		int i = 0;
		while (cdm.hasVariable(newProjection)) newProjection = "projection_"+newProj+type2string(++i);
		CDMVariable projVar(newProjection, CDM_NAT, std::vector<std::string>());
		projVar.setData(createData(CDM_NAT, 0)); // define empty data
		cdm.addVariable(projVar);
		std::vector<CDMAttribute> projAttrs = projStringToAttributes(proj_input);
		for (std::vector<CDMAttribute>::iterator it = projAttrs.begin(); it != projAttrs.end(); ++it) {
			cdm.addAttribute(newProjection, *it);
		}
	}

	if (DEBUG) {
		std::cerr << "orgX, orgY: " << orgXAxis << ", "<< orgYAxis << std::endl;
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
	}
	cdm.addOrReplaceAttribute(orgXAxis, CDMAttribute("standard_name", xStandardName));
	cdm.addOrReplaceAttribute(orgYAxis, CDMAttribute("standard_name", yStandardName));
	cdm.addOrReplaceAttribute(orgXAxis, CDMAttribute("units", out_x_axis_unit));
	cdm.addOrReplaceAttribute(orgYAxis, CDMAttribute("units", out_y_axis_unit));
	cdm.getVariable(orgXAxis).setData(createData(CDM_DOUBLE, out_x_axis.size(), out_x_axis.begin(), out_x_axis.end()));
	cdm.getVariable(orgYAxis).setData(createData(CDM_DOUBLE, out_y_axis.size(), out_y_axis.begin(), out_y_axis.end()));

	cdm.getDimension(orgXAxis).setLength(out_x_axis.size());
	cdm.getDimension(orgYAxis).setLength(out_y_axis.size());

	std::string lat(latitudeName);
	std::string lon(longitudeName);
	if (newProj != "latlong") {
		int i = 0;
		while (cdm.hasVariable(lon)) {
			lon = longitudeName + type2string(++i);
		}
		i = 0;
		while (cdm.hasVariable(lat)) {
			lat = latitudeName + type2string(++i);
		}
		cdm.generateProjectionCoordinates(newProjection, orgXAxis, orgYAxis, lon, lat);
	}

	// find all reprojectible variables and change variable attributes grid_mapping and coordinates
	{
		for (std::vector<std::string>::const_iterator varIt = projectionVariables.begin(); varIt != projectionVariables.end(); ++varIt) {
			if (newProj != "latlong") {
				cdm.addOrReplaceAttribute(*varIt, CDMAttribute("coordinates", lon + " " + lat));
				cdm.addOrReplaceAttribute(*varIt, CDMAttribute("grid_mapping", newProjection));
			} else {
				cdm.removeAttribute(*varIt, "coordinates");
				cdm.removeAttribute(*varIt, "grid_mapping");
			}
		}
	}

}

void CDMInterpolator::changeProjectionByCoordinates(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit) throw(CDMException)
{
	if (method != MIFI_NEAREST_NEIGHBOR) {
		throw CDMException("changeProjectionByCoordinates works only with nearest neighbor interpolation method");
	}
	// detect a variable with coordinates axes, the interpolator does not allow for
	// conversion of variable with different dimensions, converting only all variables
	// with the same dimensions/coordinates as the first variable with coordinates
	std::vector<std::string> variables = cdm.findVariables("coordinates", ".*");
	if (variables.size() < 1) throw CDMException("could not find coordinates needed for projection");
	std::string var = variables[0];
	std::string coordinates = cdm.getAttribute(var, "coordinates").getStringValue();
	string longitude, latitude;
	if (!cdm.getLatitudeLongitude(var, latitude, longitude)) throw CDMException("could not find lat/long coordinates in " + coordinates + " of " + var);
	const vector<string> dims = cdm.getVariable(latitude).getShape();
	// remove the old coordinates
	cdm.removeVariable(longitude);
	cdm.removeVariable(latitude);

	// mapping all variables with matching orgX/orgY dimensions
	std::map<std::string, std::string> attrs;
	attrs["coordinates"] = coordinates;
	projectionVariables = cdm.findVariables(attrs, dims);

	size_t orgXDimSize = cdm.getDimension(dims[0]).getLength();
	size_t orgYDimSize = cdm.getDimension(dims[1]).getLength();

	changeCDM(cdm, proj_input, "", projectionVariables, dims[0], dims[1], out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit, getLongitudeName(), getLatitudeName());

	boost::shared_array<double> latVals = dataReader->getData(latitude)->asDouble();
	size_t latSize = dataReader->getData(latitude)->size();
	boost::shared_array<double> lonVals = dataReader->getData(longitude)->asDouble();
	for_each(&latVals[0], &latVals[latSize], degreeToRad);
	for_each(&lonVals[0], &lonVals[latSize], degreeToRad);

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
	// get output axes expressed in latitude, longitude
	size_t fieldSize = outXAxis.size() * outYAxis.size();
	vector<double> pointsOnXAxis(fieldSize);
	vector<double> pointsOnYAxis(fieldSize);
	if (getProjectionName(proj_input) != "latlong") {
		std::string orgProjStr = "+elips=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0 +proj=latlong";
		if (MIFI_OK != mifi_project_axes(proj_input.c_str(), orgProjStr.c_str(), &outXAxis[0], &outYAxis[0], outXAxis.size(), outYAxis.size(), &pointsOnXAxis[0], &pointsOnYAxis[0])) {
			throw CDMException("unable to project axes from "+orgProjStr+ " to " +proj_input.c_str());
		}
	}

	//translatePointsToClosestInputCell(pointsOnXAxis, pointsOnYAxis, &lonVals[0], &latVals[0], xDimSize, yDimSize);
	{
		// try to determine a average grid-distance, take some example points, evaluate the max,
		// multiply that with a number slightly bigger than 1 (i use 1.414
		// and define that as grid distance
		std::vector<double> samples;
		for (size_t ik = 0; ik < 10; ik++) {
			size_t samplePos = static_cast<int>(fieldSize/10) * ik;
			double lon0 = lonVals[samplePos];
			double lat0 = latVals[samplePos];
			double min_cos_d = -2; // max possible distance on unit-sphere has cos_d -1 -> d= pi * r
			for (size_t ix = 0; ix < orgXDimSize; ix++) {
				for (size_t iy = 0; iy < orgYDimSize; iy++) {
					// find smallest distance (= max cosinus value): http://en.wikipedia.org/wiki/Great-circle_distance
					if (ix+iy*orgYDimSize != samplePos) {
						double lon1 = lonVals[ix+iy*orgYDimSize];
						double lat1 = latVals[ix+iy*orgYDimSize];
						double dlon = lon0 - lon1;

						double cos_d = cos(lat0) * cos(lat1) * cos(dlon) + sin(lat0) * sin(lat1);
						if (cos_d > min_cos_d) {
							min_cos_d = cos_d;
						}
					}
				}
			}
			samples.push_back(min_cos_d);
		}
		double max_grid_d = acos(*(max_element(samples.begin(), samples.end())));
		max_grid_d *= 1.414; // allow a bit larger extrapolation (diagonal = sqrt(2))
		if (max_grid_d > PI) max_grid_d = PI;
		double min_grid_cos_d = cos(max_grid_d);

		for (size_t i = 0; i < fieldSize; i++) {
			double lon0 = pointsOnXAxis[i];
			double lat0 = pointsOnYAxis[i];
			double min_cos_d = min_grid_cos_d; // max allowed distance
			int minI = -1; // default: outside array
			int minY = -1;
			for (size_t ix = 0; ix < orgXDimSize; ix++) {
				for (size_t iy = 0; iy < orgYDimSize; iy++) {
					// find smallest distance (= max cosinus value): http://en.wikipedia.org/wiki/Great-circle_distance
					double lon1 = lonVals[ix+iy*orgYDimSize];
					double lat1 = latVals[ix+iy*orgYDimSize];
					double dlon = lon0 - lon1;

					double cos_d = cos(lat0) * cos(lat1) * cos(dlon) + sin(lat0) * sin(lat1);
					if (cos_d > min_cos_d) {
						// std::cerr << i << ": " << cos_d << "->" << (RAD_TO_DEG * acos(cos_d)) << " : (" << (RAD_TO_DEG * lat0) << "," << (RAD_TO_DEG * lon0) << ") <->" << "(" << (RAD_TO_DEG * lat1) << "," << (RAD_TO_DEG * lon1) << ")" << std::endl;
						// smaller distance
						min_cos_d = cos_d;
						minI = ix;
						minY = iy;
					}
				}
			}
			pointsOnXAxis[i] = minI;
			pointsOnYAxis[i] = minY;
		}
	}
	cachedInterpolation = CachedInterpolation(method, pointsOnXAxis, pointsOnYAxis, orgXDimSize, orgYDimSize, out_x_axis.size(), out_y_axis.size());

	//TODO: prepare interpolation of vectors???
}

void CDMInterpolator::changeProjectionByProjectionParameters(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit) throw(CDMException)
{
	// detect original projection and axes of the first variable with projection, only convert those
	// projection and the according axes. No support for multiple axes yes.
	std::string orgProjection;
	std::string orgXAxis;
	std::string orgYAxis;
	std::string orgXAxisUnits, orgYAxisUnits;
	cdm.getProjectionAndAxesUnits(orgProjection, orgXAxis, orgYAxis, orgXAxisUnits, orgYAxisUnits);

	// mapping all variables with matching orgX/orgY dimensions
	std::vector<std::string> dims;
	std::map<std::string, std::string> attrs;
	if (orgProjection != "latitude_longitude") {
		attrs["grid_mapping"] = orgProjection;
	}
	dims.push_back(orgXAxis);
	dims.push_back(orgYAxis);
	projectionVariables = cdm.findVariables(attrs, dims);


	changeCDM(cdm, proj_input, orgProjection, projectionVariables, orgXAxis, orgYAxis, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit, getLongitudeName(), getLatitudeName());

	boost::shared_ptr<Data> orgXAxisVals = dataReader->getData(orgXAxis);
	boost::shared_ptr<Data >orgYAxisVals = dataReader->getData(orgYAxis);
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

	// calculate the mapping from the new projection points to the original axes pointsOnXAxis(x_new, y_new), pointsOnYAxis(x_new, y_new)
	size_t fieldSize = outXAxis.size() * outYAxis.size();
	vector<double> pointsOnXAxis(fieldSize);
	vector<double> pointsOnYAxis(fieldSize);
	std::string orgProjStr = "+elips=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0 +proj=latlong";
	if (orgProjection != "latitude_longitude") {
		attributesToProjString(dataReader->getCDM().getAttributes(orgProjection));
	}
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
		for_each(&orgXAxisValsArray[0], &orgXAxisValsArray[orgXAxisVals->size()], degreeToRad);
	}
	if (boost::regex_match(orgYAxisUnits, degree)) {
		miupYAxis = MIFI_LATITUDE;
		for_each(&orgYAxisValsArray[0], &orgYAxisValsArray[orgYAxisVals->size()], degreeToRad);
	}
	// translate coordinates (in rad or m) to indices
	mifi_points2position(&pointsOnXAxis[0], fieldSize, orgXAxisValsArray.get(), orgXAxisVals->size(), miupXAxis);
	mifi_points2position(&pointsOnYAxis[0], fieldSize, orgYAxisValsArray.get(), orgYAxisVals->size(), miupYAxis);

	cachedInterpolation = CachedInterpolation(method, pointsOnXAxis, pointsOnYAxis, orgXAxisVals->size(), orgYAxisVals->size(), out_x_axis.size(), out_y_axis.size());

	// prepare interpolation of vectors
	boost::shared_array<double> matrix(new double[out_x_axis.size() * out_y_axis.size() * 4]);
	mifi_get_vector_reproject_matrix(orgProjStr.c_str(), proj_input.c_str(), &out_x_axis[0], &out_y_axis[0], miupXAxis, miupYAxis, out_x_axis.size(), out_y_axis.size(), matrix.get());
	cachedVectorReprojection = CachedVectorReprojection(MIFI_VECTOR_KEEP_SIZE, matrix, out_x_axis.size(), out_y_axis.size());
}

}
