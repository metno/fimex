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

#include "fimex/config.h"
#ifdef HAVE_GRIBAPI_H
#include "fimex/GridDefinition.h"
#include <cmath>

namespace MetNoFimex
{

struct GridDefImpl {
    std::string projDefinition;
    size_t xSize;
    size_t ySize;
    double xIncr;
    double yIncr;
    double xStart;
    double yStart;
    GridDefinition::Orientation orientation;
};



GridDefinition::GridDefinition()
: gridDef(new GridDefImpl())
{
    gridDef->projDefinition = "";
    gridDef->xSize = 0;
    gridDef->ySize = 0;
    gridDef->xIncr = 0.;
    gridDef->yIncr = 0.;
    gridDef->xStart = 0.;
    gridDef->yStart = 0.;
    gridDef->orientation = GridDefinition::LeftLowerHorizontal;
}
GridDefinition::GridDefinition(
        std::string projDefinition,
        size_t xSize,
        size_t ySize,
        double xIncr,
        double yIncr,
        double xStart,
        double yStart,
        Orientation orient)
: gridDef(new GridDefImpl())
{
            gridDef->projDefinition = projDefinition;
            gridDef->xSize = xSize;
            gridDef->ySize = ySize;
            gridDef->xIncr = xIncr;
            gridDef->yIncr = yIncr;
            gridDef->xStart = xStart;
            gridDef->yStart = yStart;
            gridDef->orientation = orient;
}
GridDefinition::~GridDefinition()
{
}

std::string GridDefinition::getProjDefinition() const
{
    return gridDef->projDefinition;
}
void GridDefinition::setProjDefinition(std::string proj)
{
    gridDef->projDefinition = proj;
}


size_t GridDefinition::getXSize() const
{
    return gridDef->xSize;
}
void GridDefinition::setXSize(size_t xSize)
{
    gridDef->xSize = xSize;
}

size_t GridDefinition::getYSize() const
{
    return gridDef->ySize;

}
void GridDefinition::setYSize(size_t ySize)
{
    gridDef->ySize = ySize;
}
double GridDefinition::getXIncrement() const
{
    return gridDef->xIncr;
}
void GridDefinition::setXIncrement(double xIncr)
{
    gridDef->xIncr = xIncr;
}
double GridDefinition::getYIncrement() const
{
    return gridDef->yIncr;
}
void GridDefinition::setYIncrement(double yIncr)
{
    gridDef->yIncr = yIncr;
}
double GridDefinition::getXStart() const
{
    return gridDef->xStart;

}
void GridDefinition::setXStart(double xStart)
{
    gridDef->xStart = xStart;
}
double GridDefinition::getYStart() const
{
    return gridDef->yStart;

}
void GridDefinition::setYStart(double yStart)
{
    gridDef->yStart = yStart;
}
GridDefinition::Orientation GridDefinition::getScanMode() const
{
    return gridDef->orientation;
}
void GridDefinition::setScanMode(Orientation orient)
{
    gridDef->orientation = orient;
}


static bool deltaCompare(double a, double b, double delta)
{
    return (a == 0) ? (fabs(b) <= delta) : (fabs(1-b/a) <= delta);
}
bool GridDefinition::comparableTo(const GridDefinition& rhs, double delta) const
{
    if (gridDef->xSize != rhs.gridDef->xSize) return false;
    if (gridDef->ySize != rhs.gridDef->ySize) return false;
    if (!deltaCompare(gridDef->xIncr, rhs.gridDef->xIncr, delta)) return false;
    if (!deltaCompare(gridDef->yIncr, rhs.gridDef->yIncr, delta)) return false;
    if (!deltaCompare(gridDef->xStart, rhs.gridDef->xStart, delta)) return false;
    if (!deltaCompare(gridDef->yStart, rhs.gridDef->yStart, delta)) return false;
    return true;
}


}

#endif /* HAVE_GRIB_API */
