/*
 * Fimex, GridDefinition.cc
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

#include "fimex/GridDefinition.h"

#include <cmath>
#include <sstream>

namespace MetNoFimex
{

GridDefinition::GridDefinition()
    : isDegree_(true)
    , xSize_(0)
    , ySize_(0)
    , xIncr_(0.)
    , yIncr_(0.)
    , xStart_(0.)
    , yStart_(0.)
    , orientation_(GridDefinition::LeftLowerHorizontal)
{
}

GridDefinition::GridDefinition(std::string projDefinition, bool isDegree, size_t xSize, size_t ySize, double xIncr, double yIncr, double xStart, double yStart,
                               Orientation orient)
    : projDefinition_(projDefinition)
    , isDegree_(isDegree)
    , xSize_(xSize)
    , ySize_(ySize)
    , xIncr_(xIncr)
    , yIncr_(yIncr)
    , xStart_(xStart)
    , yStart_(yStart)
    , orientation_(orient)
{
}

static bool deltaCompare(double a, double b, double delta)
{
    return (a == 0) ? (fabs(b) <= delta) : (fabs(1-b/a) <= delta);
}

bool GridDefinition::comparableTo(const GridDefinition& rhs, double delta) const
{
    if (xSize_ != rhs.xSize_)
        return false;
    if (ySize_ != rhs.ySize_)
        return false;
    if (!deltaCompare(xIncr_, rhs.xIncr_, delta))
        return false;
    if (!deltaCompare(yIncr_, rhs.yIncr_, delta))
        return false;
    if (!deltaCompare(xStart_, rhs.xStart_, delta))
        return false;
    if (!deltaCompare(yStart_, rhs.yStart_, delta))
        return false;
    return true;
}

std::string GridDefinition::id() const
{
    std::stringstream ss;
    ss << projDefinition_ << "_";
    ss << xSize_ << "_";
    ss << ySize_ << "_";
    ss << static_cast<long long>(xIncr_ * 1000LL) << "_";
    ss << static_cast<long long>(yIncr_ * 1000LL) << "_";
    ss << static_cast<long long>(xStart_ * 1000LL) << "_";
    ss << static_cast<long long>(yStart_ * 1000LL);
    return ss.str();
}

bool GridDefinition::operator<(const GridDefinition& rhs) const
{
    return id() < rhs.id();
}

}
