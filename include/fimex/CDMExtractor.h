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

#ifndef CDMEXTRACTOR_H_
#define CDMEXTRACTOR_H_

#include <map>
#include <set>
#include <boost/array.hpp>
#include <boost/shared_ptr.hpp>

#include "fimex/CDMReader.h"
#include "fimex/CDMDataType.h"
#include "fimex/TimeUnit.h"
#include "fimex/coordSys/CoordinateAxis.h"

namespace MetNoFimex
{

/**
 * @headerfile fimex/CDMExtractor.h
 */
class CDMExtractor : public MetNoFimex::CDMReader
{
private:
    boost::shared_ptr<CDMReader> dataReader;
    typedef std::map<std::string, boost::array<size_t, 2> > DimChangeMap;
    DimChangeMap dimChanges;
    /**
     * all extractors need to have another Reader with input-data
     */
    CDMExtractor();

public:
    CDMExtractor(boost::shared_ptr<CDMReader> dataReader);
    virtual ~CDMExtractor();

//    using CDMReader::getDataSlice;
    virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0);
    virtual DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb);

    /**
     * @brief Remove a variable from the CDM
     *
     * @param varName name of the variable
     * @warning ignores removal of non-existing variable
     */
    virtual void removeVariable(std::string varName);
    /**
     * @brief select only a set of variables
     *
     * This function will remove all variables except the ones selected plus
     * eventually some auxiliary variables needed by the selected variables
     * (if keepLogical is true)
     *
     * @param variables list of variables-names
     * @param keepLogical keep logical auxiliary variables, i.e. coordinates or formula_terms
     * @warning ignores selection of non-existing variable
     */
    virtual void selectVariables(std::set<std::string> variables, bool keepLogical = false);
    /**
     * @brief Reduce a dimension of the file
     *
     * @param dimName  dimension to change
     * @param start start-position corresponding to the original dimension
     * @param length  size of the new dimension
     * @throw CDMException if dimension doesn't exist or start+size outside range of the original dimension
     */
    virtual void reduceDimension(std::string dimName, size_t start, size_t length);
    /**
     * @brief Reduce a dimension of the file
     *
     * @param dimName  dimension to change
     * @param start start-position corresponding to the original dimension, defaults to 0
     * @param end end-position of dimension, 0 means full size, negative values start from end
     * @throw CDMException if dimension doesn't exist or start+size outside range of the original dimension
     */
    virtual void reduceDimensionStartEnd(std::string dimName, size_t start = 0, long long end = 0);
    /**
     * @brief reduce the axes of a file with an explicit unit
     *
     * In contrast to #reduceDimension, this method allows the usage of
     * absolute values, not positions on the dimension. It will try to detect the
     * reduction of dimensions as needed.
     *
     * @warning reduceAxes requires the times to be monotonic
     * @warning reduceAxes requires the file to come with a known convention, e.g. CF, see listCoordinateSystems()
     * @warning reduceAxes is not able to reduce multi-dimensional axes-dimensions, e.g. time(time, station), yet
     *
     */
    virtual void reduceAxes(const std::vector<CoordinateAxis::AxisType>& types, const std::string& aUnits, double startVal, double endVal);
    /**
     * @brief reduce the time explicitly by a timestamp
     *
     * In contrast to #reduceDimension, this method allows the usage of
     * absolute times. It will try to detect the reduction of dimensions as needed
     *
     * This is implemented using reduceAxes() and the TimeAxis type.
     * @warning see warnings in reduceAxes()
     */
    virtual void reduceTime(const FimexTime& startTime, const FimexTime& endTime);

    /**
     * @brief reduce a vertical axis by value
     *
     * In contrast to #reduceDimension, this method allows the usage of vertical axes values
     * having a compatible unit to units.
     * It will try to detect the reduction of dimensions as needed.
     *
     * @param units the units of the start and end value. Only vertical axes with compatible units will be reduced.
     * @param startVal the lower value of the axis (included)
     * @param endVal the upper value of the axis (included)
     *
     * This is implemented using reduceAxes() and the axis types: pressure, height, geoZ.
     * @warning see warnings in reduceAxes()
     */
    virtual void reduceVerticalAxis(const std::string& units, double startVal, double endVal);

    /**
     * @brief reduce the horizontal layer to the latitude-longitude bounding box
     *
     * This method will try to reduce the horizontal layer to the given latitude/longitude
     * bounding box. It requires the original data to have a simple geospatial gridded CoordinateSystem,
     * i.e. CoordinateSystem::isSimpleSpatialGridded() and a projection mapping to lat/lon
     *
     * @param south southernmost border in dec. degree, -90 < south < north < 90
     * @param north northernmost border in dec. degree, -90 < south < north < 90
     * @param west westernmost border in dec. degree, -180 < west < east < 180
     * @param east easternmost border in dec. degree, -180 < west < east < 180
     */
    virtual void reduceLatLonBoundingBox(double south, double north, double west, double east);

    /**
     * @brief change the datatype of the variable
     *
     * a change of the variable will also change the datatype of the _FillValue attribute
     *
     * @param variable name of the variable
     * @param datatype new datatype
     * @throw CDMException if variable doesn't exist or conversion to datatype is not supported
     */
    virtual void changeDataType(std::string variable, CDMDataType datatype);

};

}

#endif /*CDMEXTRACTOR_H_*/
