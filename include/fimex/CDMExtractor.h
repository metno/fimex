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

#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateAxis.h"

#include <map>
#include <set>
#include <string>

namespace MetNoFimex
{

class FimexTime;

/**
 * @headerfile fimex/CDMExtractor.h
 */
class CDMExtractor : public CDMReader
{
private:
    CDMReader_p dataReader_;
    typedef std::map<std::string, std::vector<size_t> > DimSlicesMap;
    DimSlicesMap dimSlices_;

    /**
     * Helper functions to read slices of slices from the input-reader. Slices might be build with reduce-dimension
     * or similar, and then reduced further through the getDataSlice(string, SliceBuilder) interface. The slices should be
     * joined before returning with the joinSlices static function.
     *
     * @param varName the variable name to fetch data from
     * @param sb the request of the dataslice
     * @return slices return value, list of subslices building all data requested
     */
    DataPtr getDataSlice_(const std::string& varName, const SliceBuilder& sb);

    //! all extractors need to have another Reader with input-data
    CDMExtractor() = delete;

public:
    CDMExtractor(CDMReader_p dataReader);
    ~CDMExtractor();

    DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0) override;
    DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb) override;

    /**
     * @brief Remove a variable from the CDM
     *
     * @param varName name of the variable
     * @warning ignores removal of non-existing variable
     */
    virtual void removeVariable(const std::string& varName);

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
      * @param slices  slices to pick from the original dimension
      * @throw CDMException if dimension doesn't exist or start+size outside range of the original dimension
      */
    void reduceDimension(const std::string& dimName, const std::set<std::size_t>& slices);

    /**
     * @brief Reduce a dimension of the file
     *
     * @param dimName  dimension to change
     * @param start start-position corresponding to the original dimension
     * @param length  size of the new dimension
     * @throw CDMException if dimension doesn't exist or start+size outside range of the original dimension
     */
    virtual void reduceDimension(const std::string& dimName, size_t start, size_t length);

    /**
     * @brief Reduce a dimension of the file
     *
     * @param dimName  dimension to change
     * @param start start-position corresponding to the original dimension, defaults to 0
     * @param end end-position of dimension, 0 means full size, negative values start from end
     * @throw CDMException if dimension doesn't exist or start+size outside range of the original dimension
     */
    virtual void reduceDimensionStartEnd(const std::string& dimName, size_t start = 0, long long end = 0);

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
};

}

#endif /*CDMEXTRACTOR_H_*/
