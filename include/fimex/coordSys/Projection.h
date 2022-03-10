/*
 * Fimex, Projection.h
 *
 * (C) Copyright 2010-2022, met.no
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
 *  Created on: Apr 27, 2010
 *      Author: Heiko Klein
 */

#ifndef PROJECTION_H_
#define PROJECTION_H_

#include "CoordSysDecl.h"
#include "fimex/CDMAttribute.h"

#include <iosfwd>

namespace MetNoFimex
{

/**
 * @headerfile fimex/coordSys/Projection.h
 */
/**
 * A projection describes a projection of the earth surface from one system
 * to another. If not mentioned otherwise, all parameters should follow CF-1.x
 */
class Projection : public CDMNamedEntity
{
public:
    ~Projection();
    virtual std::vector<CDMAttribute> getParameters() const = 0;
    virtual void addParameter(const CDMAttribute& attribute) = 0;
    virtual void addParameters(const std::vector<CDMAttribute>& attributes) = 0;
    virtual void removeParameter(const std::string& paramName) = 0;

    /** check if the coordinates belonging to this projection are in degree (otherwise metrical) */
    virtual bool isDegree() const = 0;

    /** get a proj4 string */
    virtual std::string getProj4String() const = 0;

    /**
     * get the parts of the proj4 string defining the earth.
     */
    virtual std::string getProj4EarthString() const = 0;

    /**
     * convert two vectors of values in the projection to latitude and longitude
     * in degree. The earth ellipsoid will be the same as the input.
     *
     * @param xVals input of the projection axis in meter or degree, output is longitude
     * @param yVals input of the projection axis in meter or degree, output is latitude
     *
     * @warning the values will be converted in place
     */
    virtual void convertToLonLat(std::vector<double>& xVals, std::vector<double>& yVals) const;

    /**
     * convert two vectors of values in the projection to latitude and longitude
     * in degree. The earth ellipsoid will be the same as the input.
     *
     * @param xVals longitude input in degree, output of the projection axis in meter or degree
     * @param yVals latitude input in degree, output of the projection axis in meter or degree
     *
     * @warning the values will be converted in place
     */
    virtual void convertFromLonLat(std::vector<double>& xVals, std::vector<double>& yVals) const;

    /**
     * get a string representation
     * @note this should be implemented as unique as possible, i.e.
     */
    virtual std::string toString() const = 0;

    /**
     * Comparison of two projections, implemented using the toString() function.
     * This function does not guarantee that two projections are physically equal.
     */
    virtual bool operator==(const  Projection& b) const;

    /** create a projection from some CDMAttributes */
    static Projection_p create(const std::vector<CDMAttribute>&);

    /** create a projection from a proj4 string */
    static Projection_p createByProj4(const std::string& projStr);

protected:
    Projection() {}
};

/** output-stream for projections, implemented using toString() */
std::ostream& operator<<(std::ostream& out, const Projection& proj);

/**
 *
 * helper function to
 * replace the earth figure of the given proj4 string with a new earth-figure
 *
 * @param proj4 original proj-string
 * @param newEarthfigure earth-figure in proj-format, e.g. +ellps=sphere +R=6371000 +towgs84=0,0,0
 */
std::string replaceProj4Earthfigure(const std::string& proj4, const std::string& newEarthfigure);

} /* namespace MetNoFimex */

#endif /* PROJECTION_H_ */
