/*
 * Fimex, CDMMerger_LinearSmoothing.cc
 *
 * (C) Copyright 2012, met.no
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
 *  Created on: Aug 28, 2012
 *      Author: Alexander Bürger
 */

#include "fimex/CDMMerger_LinearSmoothing.h"

#include <boost/make_shared.hpp>

namespace { // anonymous

inline double dist(int x1, int x2, int y1, int y2) {
    const double dx = x1-x2, dy = y1-y2;
    return sqrt( dx*dx + dy*dy )/sqrt(2);
}

} // anonymous namespace

namespace MetNoFimex {

double CDMMerger_LinearSmoothing::operator()(size_t curX, size_t curY, double valueI, double valueO)
{
    if( sizeX_ == 0 or sizeY_ == 0 )
        return valueO;

    const size_t xmin1 = borderWidth_, xmax1 = xmin1+transitionWidth_;
    const size_t ymin1 = borderWidth_, ymax1 = ymin1+transitionWidth_;
    const size_t xmax2 = sizeX_ - borderWidth_, xmin2 = xmax2-transitionWidth_;
    const size_t ymax2 = sizeY_ - borderWidth_, ymin2 = ymax2-transitionWidth_;
    
    const size_t x = curX, y = curY;
    if( x < xmin1 or x >= xmax2 or y < ymin1 or y >= ymax2 )
        return valueO;
    if( x >= xmax1 and x < xmin2 and y >= ymax1 and y < ymin2 )
        return valueI;
    const double diff = (valueI - valueO);
    if( diff == 0 )
        return valueO;
    double alpha = 1;
    if( x < xmax1 ) {
        if( y < ymax1 )
            alpha = dist(x, xmin1, y, ymin1);
        else if( y >= ymin2 )
            alpha = dist(x, xmin1, y, ymax2);
        else
            alpha = (x-xmin1);
    } else if( x >= xmin2 ) {
        if( y < ymax1 )
            alpha = dist(x, xmax2, y, ymin1);
        else if( y >= ymin2 )
            alpha = dist(x, xmax2, y, ymax2);
        else
            alpha = (xmax2-x);
    } else if( y < ymax1 ) {
        alpha = (y-ymin1);
    } else if( y >= ymin2 ) {
        alpha = (ymax2-y);
    }
    alpha /= transitionWidth_;
    return valueO + alpha*diff;
}

// ========================================================================

CDMMerger_LinearSmoothingFactory::CDMMerger_LinearSmoothingFactory(size_t transitionWidth, size_t borderWidth)
    : transitionWidth_(transitionWidth)
    , borderWidth_(borderWidth)
{
    if( transitionWidth_ <= 0 or borderWidth_ < 0 )
        throw CDMException("invalid parameter values for linear smoothing");
}

CDMMerger::SmoothingPtr CDMMerger_LinearSmoothingFactory::operator()(std::string /*varName*/)
{
    return boost::make_shared<CDMMerger_LinearSmoothing>(transitionWidth_, borderWidth_);
}

} // namespace MetNoFimex
