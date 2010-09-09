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
#include <grib_api.h>
#include "fimex/interpolation.h"
#include "proj_api.h"
#include "fimex/GribApiCDMWriter_Impl1.h"
#include "fimex/CDM.h"
#include "fimex/Units.h"
#include "fimex/Utils.h"
#include "fimex/Data.h"
#include <libxml/tree.h>
#include <libxml/xpath.h>

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
	xmlNodePtr node = getNodePtr(varName, fTime, levelValue);
	std::string parameter = getXmlProp(node, "parameterNumber");
	GRIB_CHECK(grib_set_long(gribHandle.get(), "indicatorOfParameter", string2type<long>(parameter)),"");
	std::string tableNumber = getXmlProp(node, "codeTable");
	GRIB_CHECK(grib_set_long(gribHandle.get(), "gribTablesVersionNo", string2type<long>(tableNumber)),"");
}

void GribApiCDMWriter_Impl1::setProjection(const std::string& varName) throw(CDMException)
{
	LOG4FIMEX(logger, Logger::DEBUG, "setProjection(" << varName << ")");
	const CDM& cdm = cdmReader->getCDM();
	// TODO: detect more projections
	boost::shared_ptr<const Projection> proj = cdm.getProjectionOf(varName);
	if (proj.get() != 0) {
        if (proj->getName() == "stereographic" || proj->getName() == "polar_stereographic") {
            CDM::AttrVec projAttrs = proj->getParameters();
            // latitude_of_projection_origin (polar_stereographic, +- 90), via scale_factor_at_projection_origin (stereographic
            // straight_vertical_longitude_from_pole (polar_stereographic), longitude_of_projection_origin (stereographic)
            double latitudeWhereDxAndDyAreSpecifiedInDegrees = 60.;
            double orientationOfTheGridInDegrees = 0.;
            if (proj->getName() == "polar_stereograhpic") {
                LOG4FIMEX(logger, Logger::INFO, "polar_stereographic projection for" << varName);
                // get lat_ts fixed
                latitudeWhereDxAndDyAreSpecifiedInDegrees = 60.;
                // get lon0
                CDM::AttrVec::iterator ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("straight_vertical_longitude_from_pole"));
                if (ait != projAttrs.end()) {
                    orientationOfTheGridInDegrees
                            = ait->getData()->asDouble()[0];
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
                    double x = 2 * scale - 1;
                    if (x <= 1 || x >= -1) {
                        latitudeWhereDxAndDyAreSpecifiedInDegrees = RAD_TO_DEG * asin(2 * scale - 1);
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
            const boost::shared_ptr<Data> xData = cdmReader->getScaledDataInUnit(cdm.getHorizontalXAxis(varName), "m");
            const boost::shared_ptr<Data> yData = cdmReader->getScaledDataInUnit(cdm.getHorizontalYAxis(varName), "m");
            if (xData->size() < 2 || yData->size() < 2) throw CDMException(varName + " variable has to small x-y dimensions, not a grid for GRIB");
            GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongXAxis", xData->size()),"");
            GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongYAxis", yData->size()),"");
            // grib1 doesn't allow to set double values for this! // (grib2 not checked)
            const boost::shared_array<double> xArray = xData->asConstDouble();
            GRIB_CHECK(grib_set_long(gribHandle.get(), "xDirectionGridLengthInMetres", static_cast<long>(xArray[1] - xArray[0])),"");
            const boost::shared_array<double> yArray = yData->asConstDouble();
            GRIB_CHECK(grib_set_long(gribHandle.get(), "yDirectionGridLengthInMetres", static_cast<long>(yArray[1] - yArray[0])),"");
            std::string latitude, longitude;
            if (cdm.getLatitudeLongitude(varName, latitude, longitude)) {
                GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeOfFirstGridPointInDegrees", cdmReader->getData(latitude)->asConstDouble()[0]),"");
                GRIB_CHECK(grib_set_double(gribHandle.get(), "longitudeOfFirstGridPointInDegrees", cdmReader->getData(longitude)->asConstDouble()[0]),"");
            } else {
                throw CDMException("unable to find latitude/longitude for variable " + varName);
            }
            GRIB_CHECK(grib_set_double(gribHandle.get(), "orientationOfTheGridInDegrees", orientationOfTheGridInDegrees),"");
            if (abs(latitudeWhereDxAndDyAreSpecifiedInDegrees - 60.) > 1.e-5) {
                LOG4FIMEX(logger, Logger::ERROR, "grib1 does not support polar_stereographic with lat_ts != 60degree, got " << latitudeWhereDxAndDyAreSpecifiedInDegrees);
                throw CDMException("grib1 does not support polar_stereographic with lat_ts != 60degree");
                //GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeWhereDxAndDyAreSpecifiedInDegrees", latitudeWhereDxAndDyAreSpecifiedInDegrees),"");
            }
        } else if (proj->getName() == "latitude_longitude") {
            throw CDMException("projection " + proj->getName() + " not supported yet by GribApiCDMWriter");
        } else if (proj->getName() == "rotated_latitude_longitude") {
            throw CDMException("projection " + proj->getName() + " not supported yet by GribApiCDMWriter");
        } else if (proj->getName() == "transverse_mercator") {
            throw CDMException("projection " + proj->getName() + " not supported yet by GribApiCDMWriter");
        } else {
            throw CDMException("projection " + proj->getName() + " not supported yet by GribApiCDMWriter");
        }
    } else {
        throw CDMException("Cannot find projection or coordinate-system of variable " + varName);
    }
}

void GribApiCDMWriter_Impl1::setLevel(const std::string& varName, double levelValue)
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

boost::shared_ptr<Data> GribApiCDMWriter_Impl1::handleTypeScaleAndMissingData(const std::string& varName, const FimexTime& fTime, double levelValue, boost::shared_ptr<Data> inData)
{
	LOG4FIMEX(logger, Logger::DEBUG, "handleTypeScaleAndMissingData(" << varName << ", " << fTime << ", " << levelValue << ")" );
	const CDM& cdm = cdmReader->getCDM();
	double inFillValue = cdm.getFillValue(varName);
	double outFillValue = inFillValue;
	GRIB_CHECK(grib_set_double(gribHandle.get(), "missingValue", outFillValue), "setting missing value");
	// need bitmap to represent missing values in grib1
	GRIB_CHECK(grib_set_long(gribHandle.get(), "bitmapPresent", 1), "setting bitmap");

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
