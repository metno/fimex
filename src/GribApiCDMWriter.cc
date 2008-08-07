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

#include "fimex/config.h"
#ifdef HAVE_GRIBAPI_H
#include "fimex/GribApiCDMWriter.h"
#include "fimex/interpolation.h"
#include "fimex/Utils.h"
#include "fimex/XMLDoc.h"
#include "fimex/Data.h"
#include "fimex/TimeUnit.h"
#include <fcntl.h>
#include <grib_api.h>
#include <fstream>
#include <iostream>
#include <cmath>
#include <boost/shared_ptr.hpp>


namespace MetNoFimex
{
static void writeGribData(std::ofstream& gribFile, boost::shared_ptr<grib_handle> gribHandle, const boost::shared_ptr<Data>& data)
{
	GRIB_CHECK(grib_set_double_array(gribHandle.get(), "values", data->asConstDouble().get(), data->size()), "");
	// write data to file
    size_t size;
    const void* buffer;
    /* get the coded message in a buffer */
    GRIB_CHECK(grib_get_message(gribHandle.get(),&buffer,&size),0);
    gribFile.write(reinterpret_cast<const char*>(buffer), size);
}

void gribSetDate(grib_handle* gh, const FimexTime& fiTime) {
	long date = fiTime.year * 10000 + fiTime.month * 100 + fiTime.mday;
	long time = fiTime.hour * 100 + fiTime.minute;
	GRIB_CHECK(grib_set_long(gh, "dataDate", date), "setting dataDate");
	GRIB_CHECK(grib_set_long(gh, "dataTime", time), "setting dataTime");
}

void GribApiCDMWriter::writeData(std::ofstream& gribFile, boost::shared_ptr<grib_handle> gribHandle, boost::shared_ptr<Data> data, std::vector<size_t> orgDims, const std::string& time, const std::string& level, int timePos, int levelPos, size_t currentTime, size_t currentLevel, const boost::shared_array<double>& timeData, const boost::shared_array<double>& levelData, TimeUnit tu)
{
	// read the times and levels of the variable
	std::vector<size_t> finalDimSize = orgDims;
	std::vector<size_t> startDimPos(orgDims.size(), 0);
	if (timePos >= 0) {
		if (levelPos >= 0) {
			// level and time
			for (size_t t = 0; t < orgDims[timePos]; t++) {
				for (size_t l = 0; l < orgDims[levelPos]; l++) {
					finalDimSize[timePos] = 1;
					finalDimSize[levelPos] = 1;
					startDimPos[timePos] = t;
					startDimPos[levelPos] = l;
					boost::shared_ptr<Data> ndata = data->slice(orgDims, startDimPos, finalDimSize);
					// add vertical axis, add time
					gribSetDate(gribHandle.get(), tu.unitTime2fimexTime(timeData[t]));
					GRIB_CHECK(grib_set_long(gribHandle.get(), "level", static_cast<long>(levelData[l])), "setting level");
					writeGribData(gribFile, gribHandle, ndata);
				}
			}
		} else {
			// time
			for (size_t t = 0; t < orgDims[timePos]; t++) {
				finalDimSize[timePos] = 1;
				startDimPos[timePos] = t;
				boost::shared_ptr<Data> ndata = data->slice(orgDims, startDimPos, finalDimSize);
				// add vertical axis, add time
				gribSetDate(gribHandle.get(), tu.unitTime2fimexTime(timeData[t]));
				if (level != "") {
					GRIB_CHECK(grib_set_long(gribHandle.get(), "level", static_cast<long>(levelData[currentLevel])), "setting level");
				}
				writeGribData(gribFile, gribHandle, ndata);
			}
		}
	} else {
		if (levelPos >= 0) {
			// level
			for (size_t l = 0; l < orgDims[levelPos]; l++) {
				finalDimSize[levelPos] = 1;
				startDimPos[levelPos] = l;
				boost::shared_ptr<Data> ndata = data->slice(orgDims, startDimPos, finalDimSize);
				// add vertical axis, add time
				if (time != "") {
					gribSetDate(gribHandle.get(), tu.unitTime2fimexTime(timeData[currentTime]));
				}
				GRIB_CHECK(grib_set_long(gribHandle.get(), "level", static_cast<long>(levelData[l])), "setting level");
				writeGribData(gribFile, gribHandle, ndata);
			}
		} else {
			// only xy, no level, no time
			// add vertical axis, add time
			if (time != "") {
				gribSetDate(gribHandle.get(), tu.unitTime2fimexTime(timeData[currentTime]));
			}
			if (level != "") {
				GRIB_CHECK(grib_set_long(gribHandle.get(), "level", static_cast<long>(levelData[currentLevel])), "setting level");
			}
			writeGribData(gribFile, gribHandle, data);
		}
	}
}

GribApiCDMWriter::GribApiCDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile, const int gribVersion, const std::string& configFile)
: CDMWriter(cdmReader, outputFile), configFile(configFile)
{
	XMLDoc xmlConfig(configFile);

	// open the file
	std::ofstream gribFile(outputFile.c_str(), std::ios::binary|std::ios::out);
	if (!gribFile.is_open()) throw CDMException("Cannot write grib-file: "+outputFile);

	// get the major grib-handle, including projection and x/y axes
	const std::string gribVersionStr = type2string(gribVersion);
	std::string gribTemplate("GRIB" + gribVersionStr);
	boost::shared_ptr<grib_handle> mainGH(grib_handle_new_from_template(0, gribTemplate.c_str()), grib_handle_delete);
	if (mainGH.get() == 0) throw CDMException("unable to open grib_handle_from_template for grib-template: " + gribTemplate);
	// TODO: set global attributes

	const CDM& cdm = cdmReader->getCDM();
	const CDM::VarVec& vars = cdm.getVariables();
	// iterator over all variables
	for (CDM::VarVec::const_iterator vi = vars.begin(); vi != vars.end(); ++vi) {
		// TODO: detect more projections
		const std::string varName(vi->getName());
		CDM::AttrVec projAttrs = cdm.getProjection(varName);
		if (!projAttrs.empty()) {
			boost::shared_ptr<grib_handle> gh = boost::shared_ptr<grib_handle>(grib_handle_clone(mainGH.get()), grib_handle_delete);
			CDM::AttrVec::iterator projIt = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("grid_mapping_name"));
			const std::string x = cdm.getHorizontalXAxis(varName);
			const std::string y = cdm.getHorizontalYAxis(varName);
			const std::string level = cdm.getVerticalAxis(varName);
			const std::string time = cdm.getTimeAxis(varName);
			if (projIt != projAttrs.end()) {
				const std::string projection(projIt->getData()->asString());
				const boost::shared_ptr<Data> xData = cdmReader->getData(x);
				const boost::shared_ptr<Data> yData = cdmReader->getData(y);
				if (xData->size() < 2 || yData->size() < 2) {
					throw CDMException(varName + " variable has to small x-y dimensions, not a grid for GRIB");
				}
				if (projection == "stereographic" || projection == "polar_stereographic") {
					// latitude_of_projection_origin (polar_stereographic, +- 90), via scale_factor_at_projection_origin (stereographic
					// straight_vertical_longitude_from_pole (polar_stereographic), longitude_of_projection_origin (stereographic)
					double latitudeWhereDxAndDyAreSpecifiedInDegrees = 90.;
					double orientationOfTheGridInDegrees = 0.;
					if (projection == "polar_stereograhpic") {
						// get lat_ts fixed
						latitudeWhereDxAndDyAreSpecifiedInDegrees = 90.;
						// get lon0
						CDM::AttrVec::iterator ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("straight_vertical_longitude_from_pole"));
						if (ait != projAttrs.end()) {
							orientationOfTheGridInDegrees = ait->getData()->asDouble()[0];
						}
					} else {
						// test stereographic is +- 90deg latitude (grib knows only polar-stereographic)
						CDM::AttrVec::iterator ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("latitude_of_projection_origin"));
						if (ait != projAttrs.end()) {
							double lat = ait->getData()->asDouble()[0];
							if (std::fabs(lat) < 89.9995) {
								throw CDMException("grib doesn't know general stereographic projection: found origin latitude " + type2string(lat));
							}
						}
						// get lat_ts via scale_factor
						ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("scale_factor_at_projection_origin"));
						if (ait != projAttrs.end()) {
							double scale = ait->getData()->asDouble()[0];
							double x = 2*scale - 1;
							if (x <= 1 || x >= -1) {
								latitudeWhereDxAndDyAreSpecifiedInDegrees = RAD_TO_DEG * asin(2*scale - 1);
							} else {
								throw CDMException("scale_factor_at_projection_origin not defined properly: abs(2*scale-1) >= 1: " + type2string(x));
							}
						}
						// get lon0
						ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("longitude_of_projection_origin"));
						if (ait != projAttrs.end()) {
							orientationOfTheGridInDegrees = ait->getData()->asDouble()[0];
						}
					}
					std::string polar_stereographic("polar_stereographic");
					size_t ps_size = polar_stereographic.size();
					GRIB_CHECK(grib_set_string(gh.get(), "typeOfGrid", polar_stereographic.c_str(), &ps_size), "");
					GRIB_CHECK(grib_set_long(gh.get(), "numberOfPointsAlongXAxis", xData->size()),"");
					GRIB_CHECK(grib_set_long(gh.get(), "numberOfPointsAlongYAxis", yData->size()),"");
					std::string latitude, longitude;
					if (cdm.getLatitudeLongitude(varName, latitude, longitude)) {
						GRIB_CHECK(grib_set_double(gh.get(), "latitudeOfFirstGridPointInDegrees", cdmReader->getData(latitude)->asConstDouble()[0]),"");
						GRIB_CHECK(grib_set_double(gh.get(), "longitudeOfFirstGridPointInDegrees", cdmReader->getData(longitude)->asConstDouble()[0]),"");
					} else {
						throw CDMException("unable to find latitude/longitude for variable " + varName);
					}
					GRIB_CHECK(grib_set_double(gh.get(), "orientationOfTheGridInDegrees", orientationOfTheGridInDegrees),"");
					GRIB_CHECK(grib_set_double(gh.get(), "latitudeWhereDxAndDyAreSpecifiedInDegrees", latitudeWhereDxAndDyAreSpecifiedInDegrees),"");
					const boost::shared_array<double> xArray = xData->asConstDouble();
					// grib1 doesn't allow to set double values for this! // (grib2 not checked)
					GRIB_CHECK(grib_set_long(gh.get(), "xDirectionGridLengthInMetres", static_cast<long>(xArray[1] - xArray[0])),"");
					const boost::shared_array<double> yArray = yData->asConstDouble();
					GRIB_CHECK(grib_set_long(gh.get(), "yDirectionGridLengthInMetres", static_cast<long>(yArray[1] - yArray[0])),"");
				} else if (projection == "latitude_longitude") {
					throw CDMException("grid_mapping_name " + projection + " not supported yet by GribApiCDMWriter" );
				} else if (projection == "rotated_latitude_longitude") {
					throw CDMException("grid_mapping_name " + projection + " not supported yet by GribApiCDMWriter" );
				} else if (projection == "transverse_mercator") {
					throw CDMException("grid_mapping_name " + projection + " not supported yet by GribApiCDMWriter" );
				} else {
					throw CDMException("grid_mapping_name " + projection + " not supported yet by GribApiCDMWriter" );
				}
			} else {
				throw CDMException("Cannot find grid_mapping_name for projection of variable " + vi->getName());
			}

			// TODO: add parameter attribute
			std::string parameterXPath("/cdm_gribwriter_config/variables/parameter");
			try {
				const CDMAttribute& attr = cdm.getAttribute(varName, "standard_name");
				parameterXPath += "[@standard_name=\"" + attr.getData()->asString() + "\"]";
			} catch (CDMException& e) {
				parameterXPath += "[@name=\"" + varName + "\"]";
			}
			std::string parameterUnits;
			parameterXPath += "/grib"+gribVersionStr;
			std::cerr << parameterXPath << std::endl;
			XPathObjPtr xpathObj = xmlConfig.getXPathObject(parameterXPath);
			xmlNodeSetPtr nodes = xpathObj->nodesetval;
			int size = (nodes) ? nodes->nodeNr : 0;
			if (size == 1) {
				xmlNodePtr node = nodes->nodeTab[0];
				parameterUnits = getXmlProp(node, "units");
				std::string parameter = getXmlProp(node, "parameterNumber");
				if (gribVersion == 1) {
					GRIB_CHECK(grib_set_long(gh.get(), "indicatorOfParameter", string2type<long>(parameter)),"");
				} else {
					GRIB_CHECK(grib_set_long(gh.get(), "parameterNumber", string2type<long>(parameter)),"");
					std::string category = getXmlProp(node, "parameterCategory");
					GRIB_CHECK(grib_set_long(gh.get(), "parameterCategory", string2type<long>(category)),"");
					std::string discipline = getXmlProp(node, "discipline");
					GRIB_CHECK(grib_set_long(gh.get(), "discipline", string2type<long>(discipline)),"");
				}
			} else if (size > 1) {
				throw CDMException("several entries in grib-config at " + configFile + ": " + parameterXPath);
			} else {
				std::cerr << "could not find " << parameterXPath << " in " << configFile << ", skipping parameter" << std::endl;
				continue;
			}

			// set missing value
			GRIB_CHECK(grib_set_double(gh.get(), "missingValue", cdm.getFillValue(varName)), "setting missing value");

			// TODO: proper definition of level (code table 3) (indicatorOfLevel)
			// recalculate level values to have units as defined in code table 3
			// recalculate levels to be of type 'long'

			// add data
			std::cerr << "starting setting data of " << varName << ": " << time << " " << level << std::endl;
			GRIB_CHECK(grib_set_double(gh.get(), "missingValue", cdm.getFillValue(varName)),"");
			std::vector<std::string> dimNames = cdm.getVariable(varName).getShape();
			std::vector<CDMDimension> dims;
			for (std::vector<std::string>::iterator it = dimNames.begin(); it != dimNames.end(); ++it) {
				dims.push_back(cdm.getDimension(*it));
			}
			std::vector<size_t> orgDims;
			int xPos = -1;
			int yPos = -1;
			int timePos = -1;
			int levelPos = -1;
			size_t currentTime = 0;
			size_t currentLevel = 0;
			std::string unLimDim;
			for (size_t i = 0; i < dims.size(); i++) {
				if (!dims[i].isUnlimited()) {
					orgDims.push_back(dims[i].getLength());
					std::string currentName = dims[i].getName();
					if (currentName == time) {
						timePos = i;
					} else if (currentName == level) {
						levelPos = i;
					} else if (currentName == x) {
						xPos = i;
					} else if (currentName == y) {
						yPos = i;
					} else {
						if (dims.size() > 1) {
							throw CDMException("unknown dimension: " + currentName);
						}
					}
				} else {
					unLimDim = dims[i].getName();
				}
			}

			TimeUnit tu;
			boost::shared_array<double> timeData;
			if (time != "") {
				timeData = cdmReader->getData(time)->asDouble();
				tu = TimeUnit(cdmReader->getCDM().getAttribute(time, "units").getStringValue());
			}
			boost::shared_array<double> levelData;
			if (level != "") {
				levelData = cdmReader->getData(level)->asDouble();
			}
			if (!cdm.hasUnlimitedDim(cdm.getVariable(varName))) {
				boost::shared_ptr<Data> data = cdmReader->getData(varName);
				writeData(gribFile, gh, data, orgDims, time, level, timePos, levelPos, currentTime, currentLevel, timeData, levelData, tu);
			} else {
				const CDMDimension* unLimDim = cdm.getUnlimitedDim();
				for (size_t i = 0; i < unLimDim->getLength(); ++i) {
					boost::shared_ptr<Data> data = cdmReader->getDataSlice(varName, i);
					if (data->size() > 0) {
						// might be zero if slice not defined
						if (unLimDim->getName() == time) currentTime = i;
						else if (unLimDim->getName() == level) currentLevel = i;
						writeData(gribFile, gh, data, orgDims, time, level, timePos, levelPos, currentTime, currentLevel, timeData, levelData, tu);
					}
				}
			}


		} else {
			// skip var, no grid?
			// TODO: message/log
		}
	}
	gribFile.close();
}

GribApiCDMWriter::~GribApiCDMWriter()
{
}



}

#endif //HAVE_GRIBAPI_H
