/*
 * Fimex, GridDefinition.h
 *
 * (C) Copyright 2009-2022, met.no
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
 *
 *  Created on: Sep 10, 2009
 *      Author: Heiko Klein
 */

#ifndef GRIDDEFINITION_H_
#define GRIDDEFINITION_H_

#include "fimex/binaryConstants.h"

#include <memory>
#include <string>

namespace MetNoFimex
{

class GridDefinition
{
public:
    /**
     *  Use these flags to build an Orientation.
     *  Left, Upper, Horizontal and not Alternating are defaults and don't require flags.
     *
     */
    enum OrientationFlags {
        ScanStartRight =    binary<010000000>::value,
        ScanStartBottom =   binary<001000000>::value,
        ScanIsVertical =    binary<000100000>::value,
        /// change direction between succeeding rows (horizontal) or columns (vertical)
        ScanIsAlternating = binary<000010000>::value
    };
    // grib2 binary representations
    enum Orientation {
        LeftUpperHorizontal =             binary<000000000>::value,
        RightUpperHorizontal =            binary<010000000>::value,
        LeftLowerHorizontal =             binary<001000000>::value,
        RightLowerHorizontal =            binary<011000000>::value,
        LeftUpperVertical =               binary<000100000>::value,
        RightUpperVertical =              binary<010100000>::value,
        LeftLowerVertical =               binary<001100000>::value,
        RightLowerVertical =              binary<011100000>::value,

        LeftUpperHorizontalAlternating =  binary<000010000>::value,
        RightUpperHorizontalAlternating = binary<010010000>::value,
        LeftLowerHorizontalAlternating =  binary<001010000>::value,
        RightLowerHorizontalAlternating = binary<011010000>::value,
        LeftUpperVerticalAlternating =    binary<000110000>::value,
        RightUpperVerticalAlternating =   binary<010110000>::value,
        LeftLowerVerticalAlternating =    binary<001110000>::value,
        RightLowerVerticalAlternating =   binary<011110000>::value,
    };

    GridDefinition();
    GridDefinition(std::string projDefinition_, bool isDegree, size_t xSize, size_t ySize, double xIncr, double yIncr, double xStart, double yStart,
                   double lonStart, double latStart, double lonLatResolution, Orientation orient);

    /// return a proj4 string
    std::string getProjDefinition() const { return projDefinition_; }
    void setProjDefinition(const std::string& proj) { projDefinition_ = proj; }

    // check if start and increment are degree or metric
    bool isDegree() const { return isDegree_; }
    void setDegree(bool isDegree) { isDegree_ = isDegree; }

    /// number of points in x or longitude direction
    size_t getXSize() const { return xSize_; }
    void setXSize(size_t xSize) { xSize_ = xSize; }

    /// number of points in y or latitude direction
    size_t getYSize() const { return ySize_; }
    void setYSize(size_t ySize) { ySize_ = ySize; }

    /// x or longitude increment in m or degree
    double getXIncrement() const { return xIncr_; }
    void setXIncrement(double xIncr) { xIncr_ = xIncr; }

    /// y or latitude increment in m or degree
    double getYIncrement() const { return yIncr_; }
    void setYIncrement(double yIncr) { yIncr_ = yIncr; }

    /// x or longitude start in m or degree
    double getXStart() const { return xStart_; }
    void setXStart(double xStart) { xStart_ = xStart; }

    /// y or latitude start in m or degree
    double getYStart() const { return yStart_; }
    void setYStart(double yStart) { yStart_ = yStart; }

    /// longitude start in degree
    double getLonStart() const { return lonStart_; }
    void setLonStart(double lonStart) { lonStart_ = lonStart; }

    /// latitude start in degree
    double getLatStart() const { return latStart_; }
    void setLatStart(double latStart) { latStart_ = latStart; }

    Orientation getScanMode() const { return orientation_; }
    void setScanMode(Orientation orient) { orientation_ = orient; }

    /**
     * Compare two GridDefinitions. They are comparable if they have
     * \li same size(XY)
     * \li same incr(XY) within the delta
     * \li same start(XY) within the delta
     *
     * @param rhs the other gridDefinition
     * @param delta the relative delta to compare to (a == 0) ? (abs(b) <= delta) : abs((b-a)/a) <= delta
     */
    virtual bool comparableTo(const GridDefinition& rhs, double delta = 0.) const;

    /**
     * Return a id/hash-key for the grid-definition.
     *
     * The id is generated with a 0.001 degree/m accuracy for the start-position and increment.
     * Depending on construction, two GridDefinitions can still be identical, even if
     * they have different id's.
     */
    std::string id() const;

    /**
     * Less than operator, for usabity of GridDefinition in maps. It does
     * not have any other meaning than a useful sort-order.
     *
     * If lonLatResolution is > 0, the comparison uses lonStart and latStart and ignores differences smaller than this resolution.
     *
     * If lonLatResolution is <= 0, the comparison uses xStart and yStart and ignores differences smaller then 5% of the grid step.
     */
    bool operator<(const GridDefinition& rhs) const;

    bool operator==(const GridDefinition& rhs) const { return !(*this < rhs || rhs < *this); }
    bool operator!=(const GridDefinition& rhs) const { return !(*this == rhs); }

private:
    std::string projDefinition_;
    bool isDegree_;
    size_t xSize_;
    size_t ySize_;
    double xIncr_;
    double yIncr_;
    double xStart_;
    double yStart_;
    double lonStart_;
    double latStart_;
    double lonLatResolution_;
    Orientation orientation_;
};

}

#endif /* GRIDDEFINITION_H_ */
