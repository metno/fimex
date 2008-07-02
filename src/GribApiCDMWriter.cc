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

#include "fimex/GribApiCDMWriter.h"
#include "fimex/interpolation.h"
#include "fimex/Utils.h"
#include <fcntl.h>
#include <grib_api.h>
#include <fstream>
#include <iostream>
#include <boost/shared_ptr.hpp>


namespace MetNoFimex
{

GribApiCDMWriter::GribApiCDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile, const std::string& configFile)
: CDMWriter(cdmReader, outputFile), configFile(configFile)
{
	// open the file
	std::ofstream gribFile(outputFile.c_str(), std::ios::binary|std::ios::out);
	if (!gribFile.is_open()) throw CDMException("Cannot write grib-file: "+outputFile);

	// get the major grib-handle, including projection and x/y axes
	boost::shared_ptr<grib_handle> mainGH(grib_handle_new_from_template(0, "GRIB1"), grib_handle_delete);
	if (mainGH.get() == 0) throw CDMException("unable to open grib_handle_from_template");
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
			if (projIt != projAttrs.end()) {
				const std::string projection(projIt->getData()->asString());
				const std::string x = cdm.getHorizontalXAxis(varName);
				const std::string y = cdm.getHorizontalXAxis(varName);
				const boost::shared_ptr<Data> xData = cdmReader->getDataSlice(x);
				const boost::shared_ptr<Data> yData = cdmReader->getDataSlice(y);
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
							if (lat != 90. || lat != -90) {
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
								throw CDMException("scale_factor_at_projection_origin not defined propperly: abs(2*scale-1) >= 1: " + type2string(x));
							}
						}
						// get lon0
						ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("longitude_of_projection_origin"));
						if (ait != projAttrs.end()) {
							orientationOfTheGridInDegrees = ait->getData()->asDouble()[0];
						}
					}
					GRIB_CHECK(grib_set_double(gh.get(), "numberOfPointsAlongXAxis", xData->size()),"");
					GRIB_CHECK(grib_set_double(gh.get(), "numberOfPointsAlongYAxis", yData->size()),"");
					std::string latitude, longitude;
					if (cdm.getLatitudeLongitude(varName, latitude, longitude)) {
						GRIB_CHECK(grib_set_double(gh.get(), "latitudeOfFirstGridPointInDegrees", cdmReader->getDataSlice(latitude)->asConstDouble()[0]),"");
						GRIB_CHECK(grib_set_double(gh.get(), "longitudeOfFirstGridPointInDegrees", cdmReader->getDataSlice(longitude)->asConstDouble()[0]),"");
					} else {
						throw CDMException("unable to find latitude/longitude for variable " + varName);
					}
					GRIB_CHECK(grib_set_double(gh.get(), "orientationOfTheGridInDegrees", orientationOfTheGridInDegrees),"");
					GRIB_CHECK(grib_set_double(gh.get(), "latitudeWhereDxAndDyAreSpecifiedInDegrees", latitudeWhereDxAndDyAreSpecifiedInDegrees),"");
					const boost::shared_array<double> xArray = xData->asConstDouble();
					GRIB_CHECK(grib_set_double(gh.get(), "xDirectionGridLengthInMetres", xArray[1] - xArray[0]),"");
					const boost::shared_array<double> yArray = yData->asConstDouble();
					GRIB_CHECK(grib_set_double(gh.get(), "yDirectionGridLengthInMetres", yArray[1] - yArray[0]),"");
				} else if (projection == "latitude_longitude") {
					throw CDMException("grid_mapping_name " + projection + " not supportet yet by GribApiCDMWriter" );
				} else if (projection == "rotated_latitude_longitude") {
					throw CDMException("grid_mapping_name " + projection + " not supportet yet by GribApiCDMWriter" );
				} else if (projection == "transverse_mercator") {
					throw CDMException("grid_mapping_name " + projection + " not supportet yet by GribApiCDMWriter" );
				} else {
					throw CDMException("grid_mapping_name " + projection + " not supportet yet by GribApiCDMWriter" );
				}
			} else {
				throw CDMException("Cannot find grid_mapping_name for projection of variable " + vi->getName());
			}


			// TODO: add data, add vertical axis, add time

			// write data to file
	        size_t size;
	        const void* buffer;
	        /* get the coded message in a buffer */
	        GRIB_CHECK(grib_get_message(gh.get(),&buffer,&size),0);
	        gribFile.write(reinterpret_cast<const char*>(buffer), size);


		} else {
			// skip var, no grid?
			// TODO: message/log
		}
		gribFile.close();
	}
}

GribApiCDMWriter::~GribApiCDMWriter()
{
}

}
