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

namespace MetNoFimex
{



class GridDefinition
{
public:
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
    virtual ~GridDefinition();
    /// return a proj4 string
    virtual std::string getProjDefinition() const;
    /// number of points in x or longitude direction
    virtual size_t getXSize() const;
    /// number of points in y or latitude direction
    virtual size_t getYSize() const;
    /// x or longitude increment in m or degree
    virtual double getXIncrement() const;
    /// y or latitude increment in m or degree
    virtual double getYIncrement() const;
    /// x or longitude start in m or degree
    virtual double getStartX() const;
    /// y or latitude start in m or degree
    virtual double getStartY() const;
    virtual Orientation getScanMode() const;
};

}

#endif /* GRIDDEFINITION_H_ */
