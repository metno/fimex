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

#include "fimex/CDMDataType.h"
#include "fimex/CDMReader.h"
#include "fimex/CachedInterpolation.h"
#include "fimex/CachedVectorReprojection.h"
#include "fimex/CrossSectionDefinition.h"
#include "fimex/coordSys/CoordSysDecl.h"
#include "fimex/deprecated.h"

#include <map>
#include <vector>

namespace MetNoFimex
{

/**
 * @headerfile fimex/CDMInterpolator.h
 */
/**
 * operator interface to work on 2d arrays of size nx*ny
 */
class InterpolatorProcess2d {
public:
    virtual void operator()(float* array, size_t nx, size_t ny) = 0;
    virtual ~InterpolatorProcess2d();
};

class InterpolatorFill2d : public InterpolatorProcess2d {
private:
    float relaxCrit_;
    float corrEff_;
    size_t maxLoop_;
public:
    InterpolatorFill2d(float relaxCrit, float corrEff, size_t maxLoop)
        : relaxCrit_(relaxCrit), corrEff_(corrEff), maxLoop_(maxLoop) {}
    void operator()(float* array, size_t nx, size_t ny) override;
};

class InterpolatorCreepFill2d : public InterpolatorProcess2d {
private:
    unsigned short repeat_;
    char setWeight_;
public:
    InterpolatorCreepFill2d(unsigned short repeat, char setWeight)
        : repeat_(repeat), setWeight_(setWeight) {}
    void operator()(float* array, size_t nx, size_t ny) override;
};

class InterpolatorCreepFillVal2d : public InterpolatorProcess2d {
private:
    unsigned short repeat_;
    char setWeight_;
    float defVal_;
public:
    InterpolatorCreepFillVal2d(unsigned short repeat, char setWeight, float defaultValue)
        : repeat_(repeat), setWeight_(setWeight), defVal_(defaultValue) {}
    void operator()(float* array, size_t nx, size_t ny) override;
};

typedef std::shared_ptr<InterpolatorProcess2d> InterpolatorProcess2d_p;

/**
 * This class is responsible for horizontal reprojections and selection
 * of latitude longitude points.
 *
 * Vectors in the direction of x/y axes are automatically reprojected if:
 *   - the vector is marked by ''spatial_direction'' markup
 *   - the vector can be detected by the coordinate-system, e.g. CF standard-name
 *   - the method allows detection of x and y axes, e.g. changeProjectionByProjectionParameters or changeProjectionByProjectionParametersToLatLonTemplate
 */
class CDMInterpolator : public CDMReader
{
private:
    struct Impl;
    std::unique_ptr<Impl> p_;

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
    std::map<std::string, CoordinateSystem_cp> findBestCoordinateSystemsAndProjectionVars(bool withProjection);

    /**
     * check if the input-data has spatial vectors
     * @return
     */
    bool hasXYSpatialVectors() const;

    /**
     * check if the spatial vectors in x/y direction have all the same horizontal domain (e.g. not staggered)
     * @param horizontalId horizontal id to check
     * @return false if one example with horizontaId mismatch is found. true if all vectors share same horizontalId. Also true if no vectors exists.
     */
    void warnUnlessAllXYSpatialVectorsHaveSameHorizontalId(const std::string& horizontalId) const;

public:
    CDMInterpolator(CDMReader_p dataReader);
    virtual ~CDMInterpolator();
    using CDMReader::getDataSlice;
    /**
     * @brief retrieve data from the underlying dataReader and interpolate the values due to the current projection
     *
     */
    virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0);
    /**
     * @brief retrieve data from the underlying dataReader and interpolate the values due to the current projection
     *
     */
    virtual DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb);

    /**
     * @brief change the (main) projection of the dataReaders cdm to this new projection
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
     * @brief change the (main) projection of the dataReaders cdm to this new projection
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
    MIFI_DEPRECATED(virtual void changeProjection(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit));
    /**
     * @brief change the (main) projection of the dataReaders cdm to this new projection
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
     * @brief change the (main) projection of the dataReaders cdm to new projection from netcdf template file
     *
     * Opens the NetCDF file and calls changeProjection(int, CDMReader_p, const std::string&).
     * The reference variable in the file must be called "referenceVariable".
     *
     * @param method Interpolation method; passed on unchanged
     * @param netcdf_template_file input-string for netcf template filename
     */
    virtual void changeProjection(int method, const std::string& netcdf_template_file);

    /**
     * @brief change the (main) projection of the dataReaders cdm to new projection from other reader
     *
     * Interpolate/extract latitude/longitude values from regularly gridded
     * input to latitude/longitude values given from a netcdf CF-template . The
     * template must at least contain the information given in this example:
     *
     * @verbinclude ../share/etc/template4interpolation.cdl
     *
     * @param method Interpolation method, only nearestneighbor, bilinear and bicubic supported
     * @param tmplReader reader for template
     * @param tmplRefVarName name of reference variable in template
     */
    virtual void changeProjection(int method, CDMReader_p tmplReader, const std::string& tmplRefVarName);

    /**
     * @brief extract/interpolate a list of lat/lon points
     *
     * @param method Interpolation method, only nearestneighbor, bilinear and bicubic supported
     * @param lonVals longitude values in degree
     * @param latVals latitude values in degree (lonVals.size() == latVals.size())
     */
    virtual void changeProjection(int method, const std::vector<double>& lonVals, const std::vector<double>& latVals);
    /**
     * Change the projection to latitude-longitude points defined by the
     * points in the CrossSections. In between the CrossSection points,
     * latitude/longitude points will be added on a straight line (in the
     * original projection plane). The distance of the points will be in the
     * magnitude of the grid-distance.
     *
     * In addition, the cross-section names and the cross-section position in
     * the output-latitude/longitude will be added to the resulting file.
     *
     * @param method one of the MIFI_INTERPOL_* methods defined in mifi_constants.h
     * @param crossSections
     */
    virtual void changeProjectionToCrossSections(int method, const std::vector<CrossSectionDefinition>& crossSections);
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
     * @param lonName name for longitude
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
     * add a process to the internal list of preprocesses, run on fields before interpolation
     *
     * @warning this function is not completely thought through and might change
     */
    virtual void addPreprocess(InterpolatorProcess2d_p process);
    /**
     * add a process to the internal list of postprocesses, run after interpolation
     *
     * @warning this function is not completely thought through and might change
     */
    virtual void addPostprocess(InterpolatorProcess2d_p process);
};

/**
 * convert the data inplace to an array useful for interpolation (i.e. badValue->nan)
 * @param inData
 * @param badValue
 * @return
 */
extern shared_array<float> data2InterpolationArray(const DataPtr& inData, double badValue);

/**
 * convert the data from an interpolation-array (with NaNs) to one used as DataPtr, e.g. correct datatype and badvalue/fillvalue
 * @param newType
 * @param iData
 * @param size
 * @param badValue
 * @return
 */
extern DataPtr interpolationArray2Data(CDMDataType newType, shared_array<float> iData, size_t size, double badValue);

typedef std::shared_ptr<CDMInterpolator> CDMInterpolator_p;

} // namespace MetNoFimex

#endif /*CDMINTERPOLATOR_H_*/
