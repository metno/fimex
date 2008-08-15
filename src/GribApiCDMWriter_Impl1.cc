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

#include <cmath>
#include "fimex/interpolation.h"
#include "fimex/GribApiCDMWriter_Impl1.h"
#include "fimex/Units.h"

namespace MetNoFimex
{

GribApiCDMWriter_Impl1::GribApiCDMWriter_Impl1(const boost::shared_ptr<CDMReader>& cdmReader, const std::string& outputFile, const std::string& configFile)
: GribApiCDMWriter_ImplAbstract(1, cdmReader, outputFile, configFile)
{
	logger = getLogger("fimex.GribApi_CDMWriter.Impl1");
}

GribApiCDMWriter_Impl1::~GribApiCDMWriter_Impl1()
{
}

void GribApiCDMWriter_Impl1::setParameter(const std::string& varName, const FimexTime& fTime, double levelValue) throw(CDMException)
{
	LOG4FIMEX(logger, Logger::DEBUG, "setParameter(" << varName << ", " << fTime << ", " << levelValue << ")" );
	// TODO: check possible parameter per level
	const CDM& cdm = cdmReader->getCDM();
	std::string parameterXPath("/cdm_gribwriter_config/variables/parameter");
	{
		CDMAttribute attr;
		if (cdm.getAttribute(varName, "standard_name", attr)) {
			parameterXPath += "[@standard_name=\"" + attr.getData()->asString() + "\"]";
		} else {
			parameterXPath += "[@name=\"" + varName + "\"]";
		}
	}
	std::string parameterUnits;
	parameterXPath += "/grib"+type2string(gribVersion);
	XPathObjPtr xpathObj = xmlConfig->getXPathObject(parameterXPath);
	xmlNodeSetPtr nodes = xpathObj->nodesetval;
	int size = (nodes) ? nodes->nodeNr : 0;
	std::vector<std::map<std::string, std::string> > levelParameters;
	if (size == 1) {
		xmlNodePtr node = nodes->nodeTab[0];
		parameterUnits = getXmlProp(node, "units");
		std::string parameter = getXmlProp(node, "parameterNumber");
		if (gribVersion == 1) {
			GRIB_CHECK(grib_set_long(gribHandle.get(), "indicatorOfParameter", string2type<long>(parameter)),"");
			std::string tableNumber = getXmlProp(node, "codeTable");
			GRIB_CHECK(grib_set_long(gribHandle.get(), "gribTablesVersionNo", string2type<long>(tableNumber)),"");
		} else {
			GRIB_CHECK(grib_set_long(gribHandle.get(), "parameterNumber", string2type<long>(parameter)),"");
			std::string category = getXmlProp(node, "parameterCategory");
			GRIB_CHECK(grib_set_long(gribHandle.get(), "parameterCategory", string2type<long>(category)),"");
			std::string discipline = getXmlProp(node, "discipline");
			GRIB_CHECK(grib_set_long(gribHandle.get(), "discipline", string2type<long>(discipline)),"");
		}
	} else if (size > 1) {
		throw CDMException("several entries in grib-config at " + configFile + ": " + parameterXPath);
	} else {
		throw CDMException("could not find " + varName + " in " + configFile + ", skipping parameter");
	}
}

void GribApiCDMWriter_Impl1::setProjection(const std::string& varName) throw(CDMException)
{
	LOG4FIMEX(logger, Logger::DEBUG, "setProjection(" << varName << ")");
	const CDM& cdm = cdmReader->getCDM();
	// TODO: detect more projections
	CDM::AttrVec projAttrs = cdm.getProjection(varName);
	if (!projAttrs.empty()) {
		CDM::AttrVec::iterator projIt = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("grid_mapping_name"));
		const std::string x = cdm.getHorizontalXAxis(varName);
		const std::string y = cdm.getHorizontalYAxis(varName);
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
					LOG4FIMEX(logger, Logger::INFO, "polar_stereographic projection for" << varName);
					// get lat_ts fixed
					latitudeWhereDxAndDyAreSpecifiedInDegrees = 90.;
					// get lon0
					CDM::AttrVec::iterator ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("straight_vertical_longitude_from_pole"));
					if (ait != projAttrs.end()) {
						orientationOfTheGridInDegrees = ait->getData()->asDouble()[0];
					}
				} else {
					LOG4FIMEX(logger, Logger::INFO, "stereographic projection for" << varName);
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
				GRIB_CHECK(grib_set_string(gribHandle.get(), "typeOfGrid", polar_stereographic.c_str(), &ps_size), "");
				GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongXAxis", xData->size()),"");
				GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongYAxis", yData->size()),"");
				std::string latitude, longitude;
				if (cdm.getLatitudeLongitude(varName, latitude, longitude)) {
					GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeOfFirstGridPointInDegrees", cdmReader->getData(latitude)->asConstDouble()[0]),"");
					GRIB_CHECK(grib_set_double(gribHandle.get(), "longitudeOfFirstGridPointInDegrees", cdmReader->getData(longitude)->asConstDouble()[0]),"");
				} else {
					throw CDMException("unable to find latitude/longitude for variable " + varName);
				}
				GRIB_CHECK(grib_set_double(gribHandle.get(), "orientationOfTheGridInDegrees", orientationOfTheGridInDegrees),"");
				GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeWhereDxAndDyAreSpecifiedInDegrees", latitudeWhereDxAndDyAreSpecifiedInDegrees),"");
				const boost::shared_array<double> xArray = xData->asConstDouble();
				// grib1 doesn't allow to set double values for this! // (grib2 not checked)
				GRIB_CHECK(grib_set_long(gribHandle.get(), "xDirectionGridLengthInMetres", static_cast<long>(xArray[1] - xArray[0])),"");
				const boost::shared_array<double> yArray = yData->asConstDouble();
				GRIB_CHECK(grib_set_long(gribHandle.get(), "yDirectionGridLengthInMetres", static_cast<long>(yArray[1] - yArray[0])),"");
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
			throw CDMException("Cannot find grid_mapping_name for projection of variable " + varName);
		}
	} else {
		throw CDMException("No projectionn found");
	}
}

void GribApiCDMWriter_Impl1::setLevel(const std::string& varName, double levelValue)
{
	LOG4FIMEX(logger, Logger::DEBUG, "setLevel(" << varName << ", " << levelValue << ")");
	// TODO check for level/parameter dependencies
	const CDM& cdm = cdmReader->getCDM();
	std::string verticalAxis = cdm.getVerticalAxis(varName);
	std::string verticalAxisXPath("/cdm_gribwriter_config/axes/vertical_axis");
	if (verticalAxis != ""){
		CDMAttribute attr;
		if (cdm.getAttribute(verticalAxis, "standard_name", attr)) {
			verticalAxisXPath += "[@standard_name=\""+ attr.getData()->asString() + "\"]";
		} else if (cdm.getAttribute(verticalAxis, "units", attr)) {
			// units compatible to Pa or m
			std::string unit = attr.getData()->asString();
			Units units;
			if (units.areConvertible(unit, "m")) {
				verticalAxisXPath += "[@unitCompatibleTo=\"m\"]";
			} else if (units.areConvertible(unit, "Pa")) {
				verticalAxisXPath += "[@unitCompatibleTo=\"Pa\"]";
			} else {
				throw CDMException("units of vertical axis " + verticalAxis + " should be compatible with m or Pa but are: " + unit);
			}
		} else {
			throw CDMException("couldn't find standard_name or units for vertical Axis " + verticalAxis + ". Is this CF compatible?");
		}
	} else {
		// cdmGribWriterConfig should contain something like standard_name=""
		verticalAxisXPath += "[@standard_name=\"\"]";
	}
	verticalAxisXPath += "/grib" + type2string(gribVersion);
	XPathObjPtr verticalXPObj = xmlConfig->getXPathObject(verticalAxisXPath);
	xmlNodeSetPtr nodes = verticalXPObj->nodesetval;
	int size = (nodes) ? nodes->nodeNr : 0;
	if (size == 1) {
		xmlNodePtr node = nodes->nodeTab[0];
		std::string levelId = getXmlProp(node, "id");
		GRIB_CHECK(grib_set_long(gribHandle.get(), "indicatorOfTypeOfLevel", string2type<long>(levelId)),"setting levelId");
	} else if (size > 1) {
		throw CDMException("several entries in grib-config at " + configFile + ": " + verticalAxisXPath);
	} else {
		throw CDMException("could not find vertical Axis " + verticalAxisXPath + " in " + configFile + ", skipping parameter " + varName);
	}
	GRIB_CHECK(grib_set_long(gribHandle.get(), "level", static_cast<long>(levelValue)), "setting level");
}

double GribApiCDMWriter_Impl1::setMissingValue(const std::string& varName, const FimexTime& fTime, double levelValue)
{
	LOG4FIMEX(logger, Logger::DEBUG, "setMissingValue(" << varName << ", " << fTime << ", " << levelValue << ")" );
	double fillValue = cdmReader->getCDM().getFillValue(varName);
	GRIB_CHECK(grib_set_double(gribHandle.get(), "missingValue", fillValue), "setting missing value");
	return fillValue;

}


}
