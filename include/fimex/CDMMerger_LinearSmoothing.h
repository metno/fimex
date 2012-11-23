/* -*- c++ -*-
 * Fimex, CDMMerger_LinearSmoothing.h
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

#ifndef fimex_CDMMerger_LinearSmoothing_H
#define fimex_CDMMerger_LinearSmoothing_H 1

#include "CDMMerger.h"

namespace MetNoFimex {

/**
 * Defines a linear smooth transition between inner and outer grid for
 * CDMMerger.
 */
class CDMMerger_LinearSmoothing : public CDMMerger::Smoothing {
public:
    CDMMerger_LinearSmoothing(size_t transitionWidth, size_t borderWidth)
        : transitionWidth_(transitionWidth), borderWidth_(borderWidth) { }
    virtual double operator()(size_t curX, size_t curY, double valueI, double valueO);

private:
    size_t transitionWidth_, borderWidth_;
};

// ------------------------------------------------------------------------

class CDMMerger_LinearSmoothingFactory : public CDMMerger::SmoothingFactory {
public:
    enum { DEFAULT_TRANSITIONWIDTH = 5, DEFAULT_BORDERWIDTH = 2 };

    CDMMerger_LinearSmoothingFactory(size_t transitionWidth = DEFAULT_TRANSITIONWIDTH, size_t borderWidth = DEFAULT_BORDERWIDTH);

    CDMMerger::SmoothingPtr operator()(const std::string& varName);

private:
    size_t transitionWidth_, borderWidth_;
};

} // namespace MetNoFimex

#endif /* fimex_CDMMerger_LinearSmoothing_H */
