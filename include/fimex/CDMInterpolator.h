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
// forward decl
class CoordinateSystem;

/**
 * operator interface to work on 2d arrays of size nx*ny
 */
class InterpolatorProcess2d {
public:
    virtual void operator()(float* array, size_t nx, size_t ny) = 0;
};

class InterpolatorFill2d : public InterpolatorProcess2d {
private:
    float relaxCrit_;
    float corrEff_;
    size_t maxLoop_;
public:
    InterpolatorFill2d(float relaxCrit, float corrEff, size_t maxLoop)
        : relaxCrit_(relaxCrit), corrEff_(corrEff), maxLoop_(maxLoop) {}
    virtual void operator()(float* array, size_t nx, size_t ny) {size_t nChanged; mifi_fill2d_f(nx, ny, array, relaxCrit_, corrEff_, maxLoop_, &nChanged);};
};

class InterpolatorCreepFill2d : public InterpolatorProcess2d {
private:
    unsigned short repeat_;
    char setWeight_;
public:
    InterpolatorCreepFill2d(unsigned short repeat, char setWeight)
        : repeat_(repeat), setWeight_(setWeight) {}
    virtual void operator()(float* array, size_t nx, size_t ny) {size_t nChanged; mifi_creepfill2d_f(nx, ny, array, repeat_, setWeight_, &nChanged);};
};


class CDMInterpolator : public MetNoFimex::CDMReader
{
private:
    boost::shared_ptr<CDMReader> dataReader;
    std::vector<std::string> projectionVariables;
    std::vector<boost::shared_ptr<InterpolatorProcess2d> > preprocesses;
    boost::shared_ptr<CachedInterpolationInterface> cachedInterpolation;
    boost::shared_ptr<CachedVectorReprojection> cachedVectorReprojection;
    std::string latitudeName;
    std::string longitudeName;
    /** converter for axes-strings */
    void axisString2Vector(const std::string& axis, std::vector<double>& axis_vals, int axisId);
    void changeProjectionByProjectionParameters(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit);
    void changeProjectionByCoordinates(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit);
    void changeProjectionByForwardInterpolation(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit);
    boost::shared_ptr<const CoordinateSystem> findBestCoordinateSystemAndProjectionVars(bool withProjection);
    bool hasSpatialVectors() const;
public:
	CDMInterpolator(boost::shared_ptr<CDMReader> dataReader);
	virtual ~CDMInterpolator();
	/**
	 * @brief retrieve data from the underlying dataReader and interpolate the values due to the current projection
	 *
	 */
	virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos = 0);
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
	virtual void changeProjection(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit);
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
	virtual void changeProjection(int method, const std::string& proj_input, const std::string& out_x_axis, const std::string& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit);
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
	/**
	 * add a process to the internal list of preprocesses
	 *
	 * @warning this function is not completely thought through and might change
	 */
	virtual void addPreprocess(boost::shared_ptr<InterpolatorProcess2d> process);
};

}

#endif /*CDMINTERPOLATOR_H_*/
