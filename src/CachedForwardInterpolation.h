/*
 * Fimex, CachedForwardInterpolation.h
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
 *  Created on: May 15, 2009
 *      Author: Heiko Klein
 */

#ifndef CACHEDFORWARDINTERPOLATION_H_
#define CACHEDFORWARDINTERPOLATION_H_

#include "fimex/CachedInterpolation.h"
#include <boost/shared_array.hpp>
#include <vector>

namespace MetNoFimex
{

class CachedForwardInterpolation : public CachedInterpolationInterface
{
private:
//    All defined in CachedInterpolation
    std::vector<int> pointsOnXAxis; // x*inX+y maps to x in out
    std::vector<int> pointsOnYAxis; // x*inX+y maps to y in out
    size_t inX;
    size_t inY;
    size_t outX;
    size_t outY;
    float (*aggrFunc)(std::vector<float>& vec);
    bool undefAggr;

public:
    CachedForwardInterpolation(const std::string& xDimName, const std::string& yDimName, int funcType,
                               const std::vector<double>& pointsOnXAxis, const std::vector<double>& pointsOnYAxis,
                               size_t inX, size_t inY, size_t outX, size_t outY);
    virtual boost::shared_array<float> interpolateValues(boost::shared_array<float> inData, size_t size, size_t& newSize) const;
    virtual size_t getInX() const {return inX;}
    virtual size_t getInY() const {return inY;}
    virtual size_t getOutX() const {return outX;}
    virtual size_t getOutY() const {return outY;}
};

}

#endif /* CACHEDFORWARDINTERPOLATION_H_ */
