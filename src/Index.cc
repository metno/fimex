/*
 * Fimex, Index.cc
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
 *  Created on: Jul 18, 2012
 *      Author: Heiko Klein
 */

#include "fimex/Index.h"
#include <algorithm>

namespace MetNoFimex
{

Index::Index(std::vector<std::size_t> dimSizes)
: dims_(dimSizes)
{
    slices_= std::vector<std::size_t>(std::max(dims_.size(),(size_t)5), 0);
    if (dims_.size() > 0)
        slices_[0] = 1;
    for (size_t i = 1; i < dims_.size(); ++i ) {
        slices_.at(i) = slices_.at(i-1) * dims_.at(i-1);
    }
}

Index::~Index()
{}


} /* namespace MetNoFimex */
