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
#include <map>
#include "fimex/CDMReader.h"
#include "fimex/CachedInterpolation.h"
#include "fimex/CachedVectorReprojection.h"
#include "fimex/deprecated.h"


namespace MetNoFimex
{
// forward decl
class CoordinateSystem;
struct CDMInterpolatorInternals;

/**
 * operator interface to work on 2d arrays of size nx*ny
 */
class InterpolatorProcess2d {
public:
    virtual void operator()(float* array, size_t nx, size_t ny) = 0;
    virtual ~InterpolatorProcess2d() {};
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
    // the pimpl
    struct boost::shared_ptr<CDMInterpolatorInternals> p_;
    /** converter for axes-strings */
    void axisString2Vector(const std::string& axis, std::vector<double>& axis_vals, int axisId);
    void changeProjectionByProjectionParameters(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit, CDMDataType out_x_axis_type, CDMDataType out_y_axis_type);
    void changeProjectionByCoordinates(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit, CDMDataType out_x_axis_type, CDMDataType out_y_axis_type);
    void changeProjectionByForwardInterpolation(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit, CDMDataType out_x_axis_type, CDMDataType out_y_axis_type);

    void changeProjectionByProjectionParametersToLatLonTemplate(int method,
                                                                const std::string& proj_input,
                                                                const std::vector<double>& out_x_axis,
                                                                const std::vector<double>& out_y_axis,
                                                                const std::string& out_x_axis_unit,
                                                                const std::string& out_y_axis_unit,
                                                                CDMDataType out_x_axis_type,
                                                                CDMDataType out_y_axis_type,
                                                                DataPtr templateLatValues,
                                                                DataPtr templateLonValues);

    /**
     * map of CoordinateSystem::horizontalId() and the CoordinateSystem
     */
    std::map<std::string, boost::shared_ptr<const CoordinateSystem> > findBestCoordinateSystemsAndProjectionVars(bool withProjection);
    bool hasSpatialVectors() const;
public:
    CDMInterpolator(boost::shared_ptr<CDMReader> dataReader);
    virtual ~CDMInterpolator();
    /**
     * @brief retrieve data from the underlying dataReader and interpolate the values due to the current projection
     *
     */
    virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0);
    /**
     * @ brief change the (main) projection of the dataReaders cdm to this new projection
     *
     * @param method Interpolation method
     * @param proj_input input-string for proj4, used as output projection
     * @param out_x_axis values of the output x-axis
     * @param out_y_axis values of the output y-axis
     * @param out_x_axis_unit unit of the output x-axis
     * @param out_y_axis_unit unit of the output y-axis
     * @param out_x_axis_type type of CDM_TYPE (DOUBLE, FLOAT, ...) of x-axis
     * @param out_y_axis_type type of CDM_TYPE (DOUBLE, FLOAT, ...) of y-axis
     */
    virtual void changeProjection(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit, CDMDataType out_x_axis_type, CDMDataType out_y_axis_type);
    /**
     * @ brief change the (main) projection of the dataReaders cdm to this new projection
     *
     * @param method Interpolation method
     * @param proj_input input-string for proj4, used as output projection
     * @param out_x_axis values of the output x-axis
     * @param out_y_axis values of the output y-axis
     * @param out_x_axis_unit unit of the output x-axis
     * @param out_y_axis_unit unit of the output y-axis
     *
     * @deprecated use version changeProjection(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit)
     */
    DEPRECATED(virtual void changeProjection(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit));
    /**
     * @ brief change the (main) projection of the dataReaders cdm to this new projection
     *
     * @param method Interpolation method
     * @param proj_input input-string for proj4, used as output projection
     * @param out_x_axis config-string for x_axis, either '1,2,...,5' or 'auto' or 'auto,distance=3.5'
     * @param out_y_axis config-string for y_axis, either '1,2,...,5' or 'auto' or 'auto,distance=3.5'
     * @param out_x_axis_unit unit of the output x-axis
     * @param out_y_axis_unit unit of the output y-axis
     * @param out_x_axis_type type (double, float, int, short) of x-axis
     * @param out_y_axis_type type of MIFI_TYPE (double, float, int, short) of y-axis
     *
     */
    virtual void changeProjection(int method, const std::string& proj_input, const std::string& out_x_axis, const std::string& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit, const std::string& out_x_axis_type = "double", const std::string& out_y_axis_type = "double");
    /**
     * @brief change the (main) projection of the dataReaders cdm to this new projection
     *
     * Interpolate/extract latitude/longitude values from regularly gridded
     * input to latitude/longitude values given from a netcdf-CF-template. The
     * template must at least contain the information given in this example:
     *
     * @verbinclude ../share/etc/template4interpolation.cdl
     *
     * @param method Interpolation method, only nearestneighbor, bilinear and bicubic supported
     * @param netcdf_template_file input-string for netcf template filename
     *
     */
    virtual void changeProjection(int method, const std::string& netcdf_template_file);
    /**
     * @brief extract/interpolate a list of lat/lon points
     *
     * @param method Interpolation method, only nearestneighbor, bilinear and bicubic supported
     * @param lonVals longitude values in degree
     * @param latVals latitude values in degree (lonVals.size() == latVals.size())
     */
    virtual void changeProjection(int method, const std::vector<double>& lonVals, const std::vector<double>& latVals);
    /**
     * set the name for the automatically generated latitude coordinate axis. This must be set before changeProjection is called.
     * @param latName name for latitude
     */
    virtual void setLatitudeName(const std::string& latName);
    /**
     * @return the name used for latitude in the automatic coordinate generation
     */
    virtual const std::string& getLatitudeName() const;
    /**
     * set the name for the automatically generated longitude coordinate axis. This must be set before changeProjection is called.
     * @param latName name for longitude
     */
    virtual void setLongitudeName(const std::string& lonName);
    /**
     * @return the name used for longitude in the automatic coordinate generation
     */
    virtual const std::string& getLongitudeName() const;
    /**
     * get the maximum distance allowed between the center of two cells
     * to still influence each others
     *
     * @param out_x_axis the available new x-axis (in meter or radian)
     * @param out_y_axis the available new y-axis (in meter or radian)
     * @param isMetric indicate if y-axis is already given in m, or is in degree
     *
     * @return the maximum distance (in m) between adjacent points,
     *         or the value given with setDistanceOfInterest()
     */
    virtual double getMaxDistanceOfInterest(const std::vector<double>& out_y_axis, const std::vector<double>& out_x_axis, bool isMetric) const;
    /**
     * set the distance of interest, usually the radius of input-points + ~1/2 cell-size of output
     * Negative values invalidate the distance. To have effect, this function must be set before calling changeProjection()
     *
     * @param dist distance in meter
     */
    virtual void setDistanceOfInterest(double dist);
    /**
     * add a process to the internal list of preprocesses
     *
     * @warning this function is not completely thought through and might change
     */
    virtual void addPreprocess(boost::shared_ptr<InterpolatorProcess2d> process);
};

/**
 * convert the data inplace to an array useful for interpolation (i.e. badValue->nan)
 * @param inData
 * @param badValue
 * @return
 */
extern boost::shared_array<float> data2InterpolationArray(const DataPtr& inData, double badValue);
/**
 * convert the data inplace from an interpolation-array (with badValue) to one used within data
 * @param iData
 * @param size
 * @param badValue
 * @return
 */
extern DataPtr interpolationArray2Data(boost::shared_array<float> iData, size_t size, double badValue);


}

#endif /*CDMINTERPOLATOR_H_*/
