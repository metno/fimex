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

#ifndef CDMINTERPOLATOR_H_
#define CDMINTERPOLATOR_H_

#include <vector>
#include "fimex/CDMReader.h"
#include "fimex/CachedInterpolation.h"
#include "fimex/CachedVectorReprojection.h"

namespace MetNoFimex
{

class CDMInterpolator : public MetNoFimex::CDMReader
{
private:
	boost::shared_ptr<CDMReader> dataReader;
	std::vector<std::string> projectionVariables;
	boost::shared_ptr<CachedInterpolationInterface> cachedInterpolation;
	CachedVectorReprojection cachedVectorReprojection;
	std::string latitudeName;
	std::string longitudeName;
	/** converter for axes-strings */
	void axisString2Vector(const std::string& axis, std::vector<double>& axis_vals, int axisId);
	void changeProjectionByProjectionParameters(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit) throw(CDMException);
	void changeProjectionByCoordinates(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit) throw(CDMException);
	void changeProjectionByForwardInterpolation(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit) throw(CDMException);
public:
	CDMInterpolator(boost::shared_ptr<CDMReader> dataReader);
	virtual ~CDMInterpolator();
	/**
	 * @brief retrieve data from the underlying dataReader and interpolate the values due to the current projection
	 *
	 */
	virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos = 0) throw(CDMException);
	/**
	 * @ brief change the (main) projection of the dataReaders cdm to this new projection
	 *
	 * @param method Interpolation method
	 * @param proj_input input-string for proj4, used as output projection
	 * @param out_x_axis values of the output x-axis
	 * @param out_y_axis values of the output y-axis
	 * @param out_x_axis_unit unit of the output x-axis
	 * @param out_y_axis_unit unit of the output y-axis
	 */
	virtual void changeProjection(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit) throw(CDMException);
	/**
	 * @ brief change the (main) projection of the dataReaders cdm to this new projection
	 *
	 * @param method Interpolation method
	 * @param proj_input input-string for proj4, used as output projection
	 * @param out_x_axis config-string for x_axis, either '1,2,...,5' or 'auto' or 'auto,distance=3.5'
	 * @param out_y_axis config-string for y_axis, either '1,2,...,5' or 'auto' or 'auto,distance=3.5'
	 * @param out_x_axis_unit unit of the output x-axis
	 * @param out_y_axis_unit unit of the output y-axis
	 */
	virtual void changeProjection(int method, const std::string& proj_input, const std::string& out_x_axis, const std::string& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit) throw(CDMException);
	/**
	 * set the name for the automatically generated latitude coordinate axis. This must be set before changeProjection is called.
	 * @param latName name for latitude
	 */
	virtual void setLatitudeName(const std::string& latName) {this->latitudeName = latName;}
	/**
	 * @return the name used for latitude in the automatic coordinate generation
	 */
	virtual const std::string& getLatitudeName() const {return latitudeName;}
	/**
	 * set the name for the automatically generated longitude coordinate axis. This must be set before changeProjection is called.
	 * @param latName name for longitude
	 */
	virtual void setLongitudeName(const std::string& lonName) {this->longitudeName = lonName;}
	/**
	 * @return the name used for longitude in the automatic coordinate generation
	 */
	virtual const std::string& getLongitudeName() const {return longitudeName;}
};

}

#endif /*CDMINTERPOLATOR_H_*/
