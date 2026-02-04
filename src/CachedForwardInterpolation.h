/*
 * Fimex, CachedForwardInterpolation.h
 *
 * (C) Copyright 2009-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

namespace MetNoFimex {

struct Aggregator;

class CachedForwardInterpolation : public CachedInterpolationInterface
{
private:
    shared_array<std::vector<size_t>> pointsInIn;
    size_t maxPointsInIn;
    std::unique_ptr<Aggregator> agg;
    bool undefAggr;

public:
    CachedForwardInterpolation(const std::string& xDimName, const std::string& yDimName, int funcType, shared_array<double> pointsOnXAxis,
                               shared_array<double> pointsOnYAxis, size_t inX, size_t inY, size_t outX, size_t outY);
    ~CachedForwardInterpolation();
    shared_array<float> interpolateValues(shared_array<float> inData, size_t size, size_t& newSize) const override;
};

} // namespace MetNoFimex

#endif /* CACHEDFORWARDINTERPOLATION_H_ */
