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
#include "fimex/TimeLevelDataSliceFetcher.h"

namespace MetNoFimex
{

GribApiCDMWriter_Impl1::GribApiCDMWriter_Impl1(const boost::shared_ptr<CDMReader>& cdmReader, const std::string& outputFile, const std::string& configFile)
: GribApiCDMWriter_ImplAbstract(1, cdmReader, outputFile, configFile)
{
	// TODO: set global attributes

	const CDM& cdm = cdmReader->getCDM();
	const CDM::VarVec& vars = cdm.getVariables();
	// iterator over all variables
	for (CDM::VarVec::const_iterator vi = vars.begin(); vi != vars.end(); ++vi) {
		const std::string& varName = vi->getName();
		std::vector<FimexTime> times = getTimes(varName);
		std::vector<double> levels = getLevels(varName);
		TimeLevelDataSliceFetcher tld(cdmReader, varName);
		try {
			setProjection(varName);
		} catch (CDMException& e) {
			std::cerr << "cannot write variable " << varName << " due to projection problems: " << e.what() << std::endl;
			continue;
		}
		for (size_t t = 0; t < times.size(); t++) {
			for (size_t l = 0; l < levels.size(); l++) {
				boost::shared_ptr<Data> data = tld.getTimeLevelSlice(t, l);

			}
		}
	}
}

GribApiCDMWriter_Impl1::~GribApiCDMWriter_Impl1()
{
}


void GribApiCDMWriter_Impl1::setProjection(const std::string& varName) throw(CDMException)
{
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

}
