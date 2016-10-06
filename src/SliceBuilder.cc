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
#include "fimex/Utils.h"
#include "fimex/coordSys/CoordinateAxis.h"

#include "fimex/Logger.h"

#include <limits>

namespace MetNoFimex
{

using namespace std;

static LoggerPtr logger = getLogger("fimex.SliceBuilder");

SliceBuilder::SliceBuilder(const vector<string>& dimNames, const vector<size_t>& dimSize)
{
    vector<bool> unlimited(dimNames.size(), false);
    init(dimNames, dimSize, unlimited);
}

SliceBuilder::SliceBuilder(const CDM& cdm, const std::string& varName, bool setUnlimited)
{
    LOG4FIMEX(logger, Logger::DEBUG, "slicebuilder ctor varName='" << varName << "'");
    const CDMVariable& var = cdm.getVariable(varName);
    vector<string> shape = var.getShape();
    vector<size_t> dimSizes;
    vector<bool> unlimited;
    dimSizes.reserve(shape.size());
    unlimited.reserve(shape.size());
    for (vector<string>::const_iterator dimIt = shape.begin(); dimIt != shape.end(); ++dimIt) {
        LOG4FIMEX(logger, Logger::DEBUG, "  shape dim '" << *dimIt << "'");
        const CDMDimension& dim = cdm.getDimension(*dimIt);
        dimSizes.push_back(dim.getLength());
        if (setUnlimited && dim.isUnlimited()) {
            unlimited.push_back(true);
        } else {
            unlimited.push_back(false);
        }
    }
    init(shape, dimSizes, unlimited);
}

void SliceBuilder::init(const vector<string>& dimNames, const vector<size_t>& dimSize, const vector<bool>& unlimited)
{
    if (dimNames.size() != dimSize.size()) {
        throw CDMException("dimension mismatch in SliceBuilder::init");
    }
    start_.resize(dimNames.size());
    size_.resize(dimNames.size());
    maxSize_.resize(dimNames.size());
    unlimited_.resize(dimNames.size());
    for (size_t pos = 0; pos < dimNames.size(); ++pos) {
        dimPos_[dimNames.at(pos)] = pos;
        start_.at(pos) = 0;
        size_.at(pos) = dimSize.at(pos);
        maxSize_.at(pos) = dimSize.at(pos);
        unlimited_.at(pos) = unlimited.at(pos);
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

void SliceBuilder::getStartAndSize(const std::string & dimName, std::size_t& start, std::size_t& size) const
{
    size_t pos = getDimPos(dimName);
    start = start_.at(pos);
    size = size_.at(pos);
}

void SliceBuilder::setStartAndSize(const std::string & dimName, size_t start, size_t size)
{
    size_t pos = getDimPos(dimName);
    if ((!unlimited_.at(pos)) && maxSize_.at(pos) < (start + size)) {
        throw out_of_range("slicebuilder: "+ dimName + " too large: start: " + type2string(start) + " size: " + type2string(size));
    }
    start_.at(pos) = start;
    size_.at(pos) = size;
    setDims_.insert(dimName);
}

void SliceBuilder::setStartAndSize(const boost::shared_ptr<const CoordinateAxis>& axis, size_t start, size_t size)
{
    if (axis.get() == 0)
        return;
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
    if (axis.get() == 0)
        return;
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

std::ostream& operator<<(std::ostream& os, const SliceBuilder& sb)
{
    vector<string> dims = sb.getDimensionNames();
    vector<size_t> start = sb.getDimensionStartPositions();
    vector<size_t> size = sb.getDimensionSizes();
    for (size_t i = 0; i < dims.size(); ++i) {
        os << dims[i] << ": " << start[i] << ", " << size[i] << endl;
    }
    return os;
}

}
