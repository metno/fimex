/*
 * Fimex, GridDefinition.h
 *
 * (C) Copyright 2009, met.no
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
#include <string>
#include "boost/shared_ptr.hpp"

namespace MetNoFimex
{

// forward declaration
struct GridDefImpl;

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
    GridDefinition(
            std::string projDefinition,
            bool isDegree,
            size_t xSize,
            size_t ySize,
            double xIncr,
            double yIncr,
            double xStart,
            double yStart,
            Orientation orient);
    virtual ~GridDefinition();
    /// return a proj4 string
    virtual std::string getProjDefinition() const;
    virtual void setProjDefinition(std::string proj);
    // check if start and increment are degree or metric
    virtual bool isDegree() const;
    virtual void setDegree(bool isDegree);
    /// number of points in x or longitude direction
    virtual size_t getXSize() const;
    virtual void setXSize(size_t xSize);
    /// number of points in y or latitude direction
    virtual size_t getYSize() const;
    virtual void setYSize(size_t ySize);
    /// x or longitude increment in m or degree
    virtual double getXIncrement() const;
    virtual void setXIncrement(double xIncr);
    /// y or latitude increment in m or degree
    virtual double getYIncrement() const;
    virtual void setYIncrement(double yIncr);
    /// x or longitude start in m or degree
    virtual double getXStart() const;
    virtual void setXStart(double startX);
    /// y or latitude start in m or degree
    virtual double getYStart() const;
    virtual void setYStart(double startY);
    virtual Orientation getScanMode() const;
    virtual void setScanMode(Orientation orient);

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
     */
    bool operator<(const GridDefinition& rhs) const;

private:
    boost::shared_ptr<GridDefImpl> gridDef;

};

}

#endif /* GRIDDEFINITION_H_ */
