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
#include <cmath>
#include "fimex/interpolation.h"
#include "fimex/GribApiCDMWriter_Impl2.h"
#include "fimex/Units.h"

namespace MetNoFimex
{

GribApiCDMWriter_Impl2::GribApiCDMWriter_Impl2(const boost::shared_ptr<CDMReader>& cdmReader, const std::string& outputFile, const std::string& configFile)
: GribApiCDMWriter_ImplAbstract(2, cdmReader, outputFile, configFile)
{
	logger = getLogger("fimex.GribApi_CDMWriter.Impl2");
}

GribApiCDMWriter_Impl2::~GribApiCDMWriter_Impl2()
{
}


void GribApiCDMWriter_Impl2::setParameter(const std::string& varName, const FimexTime& fTime, double levelValue) throw(CDMException)
{
	LOG4FIMEX(logger, Logger::DEBUG, "setParameter(" << varName << ", " << fTime << ", " << levelValue << ")" );
	xmlNodePtr node = getNodePtr(varName, fTime, levelValue);
	std::string parameter = getXmlProp(node, "parameterNumber");
	std::string category = getXmlProp(node, "parameterCategory");
	std::string discipline = getXmlProp(node, "discipline");
	if (parameter == "" || category == "" || discipline == "") {
		throw CDMException("incomplete defininition of " + varName + ": (param, categ, discipl) = (" + parameter + "," + category + "," + discipline +")");
	}
	GRIB_CHECK(grib_set_long(gribHandle.get(), "parameterNumber", string2type<long> (parameter)), "");
	GRIB_CHECK(grib_set_long(gribHandle.get(), "parameterCategory",	string2type<long> (category)), "");
	GRIB_CHECK(grib_set_long(gribHandle.get(), "discipline", string2type<long> (discipline)), "");
}

void GribApiCDMWriter_Impl2::setProjection(const std::string& varName) throw(CDMException)
{
	LOG4FIMEX(logger, Logger::DEBUG, "setProjection(" << varName << ")");
	const CDM& cdm = cdmReader->getCDM();
	// TODO: detect more projections
	CDM::AttrVec projAttrs = cdm.getProjection(varName);
	if (!projAttrs.empty()) {
		std::string projVar = cdm.getAttribute(varName, "grid_mapping").getData()->asString();
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
					double lon = cdmReader->getData(longitude)->asConstDouble()[0];
					while (lon < 0) {
						lon += 360;
					}
					GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeOfFirstGridPointInDegrees", cdmReader->getData(latitude)->asConstDouble()[0]),"");
					GRIB_CHECK(grib_set_double(gribHandle.get(), "longitudeOfFirstGridPointInDegrees", lon),"");
				} else {
					throw CDMException("unable to find latitude/longitude for variable " + varName);
				}
				GRIB_CHECK(grib_set_double(gribHandle.get(), "orientationOfTheGridInDegrees", orientationOfTheGridInDegrees),"");
				GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeWhereDxAndDyAreSpecifiedInDegrees", latitudeWhereDxAndDyAreSpecifiedInDegrees),"");
				const boost::shared_array<double> xArray = xData->asConstDouble();
				GRIB_CHECK(grib_set_double(gribHandle.get(), "xDirectionGridLengthInMetres", (xArray[1] - xArray[0])),"");
				const boost::shared_array<double> yArray = yData->asConstDouble();
				GRIB_CHECK(grib_set_double(gribHandle.get(), "yDirectionGridLengthInMetres", (yArray[1] - yArray[0])),"");
			} else if (projection == "latitude_longitude") {
				LOG4FIMEX(logger, Logger::INFO, "latlong projection for" << varName);
				size_t ni, nj;
				double di, dj, lon0, lat0;
				std::string latitude, longitude;
				if (cdm.getLatitudeLongitude(varName, latitude, longitude)) {
					boost::shared_ptr<Data> lonData = cdmReader->getData(longitude);
					boost::shared_ptr<Data> latData = cdmReader->getData(latitude);
					ni = lonData->size();
					nj = lonData->size();
					if (ni < 2 || nj < 2) {
						throw CDMException("longitude, latitude for varName " + varName + " has to small dimension for grid: (" + type2string(ni) + "," + type2string(nj) + ")");
					}
					const boost::shared_array<double> longs = lonData->asConstDouble();
					const boost::shared_array<double> lats = latData->asConstDouble();
					di = longs[1] - longs[0];
					dj = lats[1] - lats[0];
					lat0 = lats[0];
					lon0 = longs[0];
					while (lon0 < 0) {
						lon0 += 360;
					}
				} else {
					throw CDMException("could not find latitude/longitude for varName: " + varName);
				}
				std::string typeOfGrid("regular_ll");
				// TODO: untested
				size_t tog_size = typeOfGrid.size();
				GRIB_CHECK(grib_set_string(gribHandle.get(), "typeOfGrid", typeOfGrid.c_str(), &tog_size), "");
				GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongAParallel", ni),"");
				GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongAMeridian", nj),"");
				GRIB_CHECK(grib_set_double(gribHandle.get(), "iDirectionIncrementInDegrees", di),"");
				GRIB_CHECK(grib_set_double(gribHandle.get(), "jDirectionIncrementInDegrees", dj),"");
			} else if (projection == "rotated_latitude_longitude") {
				LOG4FIMEX(logger, Logger::INFO, "rotated latlong projection for " << varName);
				const std::string rotLon = cdm.getHorizontalXAxis(varName);
				const std::string rotLat = cdm.getHorizontalYAxis(varName);
				boost::shared_ptr<Data> rLonData = cdmReader->getData(rotLon);
				boost::shared_ptr<Data> rLatData = cdmReader->getData(rotLat);
				size_t ni = rLonData->size();
				size_t nj = rLatData->size();
				if (ni < 2 || nj < 2) {
					throw CDMException("(ni,nj) for varName " + varName + " has to small dimension for grid: (" + type2string(ni) + "," + type2string(nj) + ")");
				}
				double di, dj, rlon0, rlat0;
				const boost::shared_array<double> rlongs = rLonData->asConstDouble();
				const boost::shared_array<double> rlats = rLatData->asConstDouble();
				di = rlongs[1] - rlongs[0];
				dj = rlats[1] - rlats[0];
				rlat0 = rlats[0];
				rlon0 = rlongs[0];
				while (rlon0 < 0) {
					rlon0 += 360;
				}


				double northPoleLon = cdm.getAttribute(projVar, "grid_north_pole_longitude").getData()->asConstDouble()[0];
				double northPoleLat = cdm.getAttribute(projVar, "grid_north_pole_latitude").getData()->asConstDouble()[0];


				double southPoleLat = -1 * northPoleLat;
				while (southPoleLat < -90) {
					southPoleLat += 180;
				}

				double southPoleLon = northPoleLon - 180;
				while (southPoleLon < 0) {
					southPoleLon += 360;
				}

				std::string typeOfGrid("rotated_ll");
				// TODO: this seems still to be inperfect, more tests required
				size_t tog_size = typeOfGrid.size();
				GRIB_CHECK(grib_set_string(gribHandle.get(), "typeOfGrid", typeOfGrid.c_str(), &tog_size), "");
				GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongAParallel", ni),"");
				GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongAMeridian", nj),"");
				GRIB_CHECK(grib_set_double(gribHandle.get(), "iDirectionIncrementInDegrees", di),"");
				GRIB_CHECK(grib_set_long(gribHandle.get(), "latitudeOfTheSouthernPoleOfProjection", static_cast<long>(southPoleLat * 1000000)), "");
				GRIB_CHECK(grib_set_long(gribHandle.get(), "longitudeOfTheSouthernPoleOfProjection", static_cast<long>(southPoleLon * 1000000)), "");
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

void GribApiCDMWriter_Impl2::setLevel(const std::string& varName, double levelValue)
{
	LOG4FIMEX(logger, Logger::DEBUG, "setLevel(" << varName << ", " << levelValue << ")");
	// check for level/parameter dependencies
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
	verticalAxisXPath += "/grib2";
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

boost::shared_ptr<Data> GribApiCDMWriter_Impl2::handleTypeScaleAndMissingData(const std::string& varName, const FimexTime& fTime, double levelValue, boost::shared_ptr<Data> inData)
{
	LOG4FIMEX(logger, Logger::DEBUG, "handleTypeScaleAndMissingData(" << varName << ", " << fTime << ", " << levelValue << ")" );
	const CDM& cdm = cdmReader->getCDM();
	double inFillValue = cdm.getFillValue(varName);
	double outFillValue = inFillValue;
	GRIB_CHECK(grib_set_double(gribHandle.get(), "missingValue", outFillValue), "setting missing value");
	GRIB_CHECK(grib_set_long(gribHandle.get(), "bitMapIndicator", 0), "setting bitmap");

	CDMAttribute attr;
	double scale = 1.;
	double offset = 0.;
	if (cdm.getAttribute(varName, "scale_factor", attr)) {
		scale = attr.getData()->asConstDouble()[0];
	}
	if (cdm.getAttribute(varName, "add_offset", attr)) {
		offset = attr.getData()->asConstDouble()[0];
	}
	// scale and offset by units
	if (cdm.getAttribute(varName, "units", attr)) {
		std::string unit = attr.getData()->asString();
		xmlNodePtr node = getNodePtr(varName, fTime, levelValue);
		std::string gUnit = getXmlProp(node, "units");
		if (gUnit != "") {
			double slope, uOffset;
			Units u;
			u.convert(unit, gUnit, slope, uOffset);
			// join both scalings: (scale*x + offset)*slope + uOffset
			scale *= slope;
			offset *= slope;
			offset += uOffset;
		}
	}

	LOG4FIMEX(logger, Logger::DEBUG, "change from (" << inFillValue << " " << scale << "," << offset << ") to (" << outFillValue << "," << 1 << "," << 0 << ")" );

	return inData->convertDataType(inFillValue, scale, offset, CDM_DOUBLE, outFillValue,1,0);
}


}
#endif /* HAVE_GRIBAPI_H */
