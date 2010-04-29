/*
 * Fimex, Projection.h
 *
 * (C) Copyright 2010, met.no
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

#include <vector>
#include <iostream>
#include <boost/shared_ptr.hpp>
#include "fimex/CDMAttribute.h"

namespace MetNoFimex
{

/**
 * A projection describes a projection of the earth surface from one system
 * to another. If not mentioned otherwise, all parameters should follow CF-1.x
 */
class Projection : public CDMNamedEntity
{
public:
    virtual ~Projection() {}
    virtual std::vector<CDMAttribute> getParameters() const = 0;
    virtual void addParameter(CDMAttribute attribute) = 0;
    virtual void addParameters(std::vector<CDMAttribute> attributes) = 0;
    virtual void removeParameter(std::string paramName) = 0;
    /** get the projection name */
    virtual const std::string& getName() const = 0;
    /** check if the coordinates belonging to this projection are in degree (otherwise metrical) */
    virtual const bool isDegree() const = 0;
    /** get a proj4 string */
    virtual std::string getProj4String() const = 0;
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
    static boost::shared_ptr<Projection> create(std::vector<CDMAttribute>);
    /** create a projection from a proj4 string */
    static boost::shared_ptr<Projection> createByProj4(const std::string& projStr);
protected:
    Projection() {};
};

/** output-stream for projections, implemented using toString() */
std::ostream& operator<<(std::ostream& out, const Projection& proj);

} /* namespace MetNoFimex */

#endif /* PROJECTION_H_ */
