/*
 * Fimex, SliceBuilder.cc
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
 *  Created on: Mar 16, 2010
 *      Author: Heiko Klein
 */

#include "fimex/SliceBuilder.h"
#include "fimex/CDM.h"
#include "fimex/CDMVariable.h"
#include "fimex/CDMDimension.h"
#include "fimex/coordSys/CoordinateAxis.h"

namespace MetNoFimex
{

using namespace std;

SliceBuilder::SliceBuilder(const CDM& cdm, const std::string& varName)
{
    const CDMVariable& var = cdm.getVariable(varName);
    const vector<string>& shape = var.getShape();
    start_.resize(shape.size());
    size_.resize(shape.size());
    maxSize_.resize(shape.size());
    size_t pos = 0;
    for (vector<string>::const_iterator dimIt = shape.begin(); dimIt != shape.end(); ++dimIt, ++pos) {
        const CDMDimension& dim = cdm.getDimension(*dimIt);
        dimPos_[dim.getName()] = pos;
        start_.at(pos) = 0;
        size_.at(pos) = dim.getLength();
        maxSize_.at(pos) = dim.getLength();
    }
}

SliceBuilder::~SliceBuilder() {}

size_t SliceBuilder::getDimPos(const std::string& dimName) const
{
    map<string, size_t>::const_iterator posIt = dimPos_.find(dimName);
    if (posIt == dimPos_.end()) {
        throw CDMException("dimName '" + dimName + "' not part of this sliceBuilder");
    }
    return posIt->second;
}


void SliceBuilder::setStartAndSize(const std::string & dimName, size_t start, size_t size)
{
    size_t pos = getDimPos(dimName);
    if (maxSize_.at(pos) < (start + size)) {
        throw out_of_range("slicebuilder: "+ dimName);
    }
    start_.at(pos) = start;
    size_.at(pos) = size;
}

void SliceBuilder::setStartAndSize(const boost::shared_ptr<const CoordinateAxis>& axis, size_t start, size_t size)
{
    if (axis.get() == 0) return;
    setStartAndSize(axis->getName(), start, size);
}

vector<string> SliceBuilder::getDimensionNames() const
{
    vector<string> names;
    names.resize(dimPos_.size());
    for (map<string, size_t>::const_iterator posIt = dimPos_.begin(); posIt != dimPos_.end(); ++posIt) {
       names.at(posIt->second) = posIt->first;
    }
    return names;
}


}
