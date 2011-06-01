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

SliceBuilder::SliceBuilder(const vector<string>& dimNames, const vector<size_t>& dimSize)
{
    init(dimNames, dimSize);
}

SliceBuilder::SliceBuilder(const CDM& cdm, const std::string& varName)
{
    const CDMVariable& var = cdm.getVariable(varName);
    vector<string> shape = var.getShape();
    vector<size_t> dimSizes;
    dimSizes.reserve(shape.size());
    for (vector<string>::const_iterator dimIt = shape.begin(); dimIt != shape.end(); ++dimIt) {
        const CDMDimension& dim = cdm.getDimension(*dimIt);
        dimSizes.push_back(dim.getLength());
    }
    init(shape, dimSizes);
}

void SliceBuilder::init(const vector<string>& dimNames, const vector<size_t>& dimSize)
{
    if (dimNames.size() != dimSize.size()) {
        throw CDMException("dimension mismatch in SliceBuilder::init");
    }
    start_.resize(dimNames.size());
    size_.resize(dimNames.size());
    maxSize_.resize(dimNames.size());
    for (size_t pos = 0; pos < dimNames.size(); ++pos) {
        dimPos_[dimNames.at(pos)] = pos;
        start_.at(pos) = 0;
        size_.at(pos) = dimSize[pos];
        maxSize_.at(pos) = dimSize[pos];
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
    setDims_.insert(dimName);
}

void SliceBuilder::setStartAndSize(const boost::shared_ptr<const CoordinateAxis>& axis, size_t start, size_t size)
{
    if (axis.get() == 0) return;
    if (axis->isExplicit()) {
        setStartAndSize(axis->getName(), start, size);
    } else {
        throw CDMException("Cannot set implicit axis " + axis->getName() + " in SliceBuilder");
    }
}

void SliceBuilder::setAll(const std::string & dimName)
{
    size_t pos = getDimPos(dimName);
    setStartAndSize(dimName, 0, maxSize_.at(pos));
}
void SliceBuilder::setAll(const boost::shared_ptr<const CoordinateAxis>& axis)
{
    if (axis.get() == 0) return;
    if (axis->isExplicit()) {
        setAll(axis->getName());
    } else {
        throw CDMException("Cannot set implicit axis " + axis->getName() + " in SliceBuilder");
    }
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

vector<string> SliceBuilder::getUnsetDimensionNames() const
{
    vector<string> names;
    names.reserve(dimPos_.size());
    for (map<string, size_t>::const_iterator posIt = dimPos_.begin(); posIt != dimPos_.end(); ++posIt) {
        if (setDims_.find(posIt->first) == setDims_.end()) { // not set
            names.push_back(posIt->first);
        }
    }
    return names;

}

}
