/*
 * Fimex, IndexedData.cc
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

#include "fimex/IndexedData.h"
#include "fimex/Data.h"
#include "fimex/Index.h"
#include <numeric>
#include <functional>
#include "fimex/CDMDataType.h"

namespace MetNoFimex
{

class IndexedDataImpl {
public:
    DataPtr data;
    Index idx;
};

IndexedData::IndexedData()
{
    init(createData(CDM_NAT,0,0.), std::vector<size_t>(0));
}

IndexedData::IndexedData(DataPtr data, std::vector<std::size_t> dims)
{
    init(data, dims);
}

IndexedData::~IndexedData()
{}

void IndexedData::init(DataPtr data, std::vector<std::size_t> dims)
{
    p_ = boost::shared_ptr<IndexedDataImpl>(new IndexedDataImpl());
    p_->data = data;
    setDims(dims);
}

void IndexedData::setDims(std::vector<std::size_t> dims)
{
    std::size_t size = std::accumulate(&dims[0], &dims[dims.size()], 1, std::multiplies<size_t>());
    if (size == p_->data->size()) {
        p_->idx = Index(dims);
    } else if (p_->data->size() == 0) {
        p_->idx = Index(std::vector<size_t>(0));
    } else {
        throw std::runtime_error("setDims error: dims.size != data.size: " + type2string(size) + " != " + type2string(p_->data->size()));
    }
}

DataPtr IndexedData::getDataPtr() const
{
    return p_->data;
}

const Index& IndexedData::idx() const
{
    return p_->idx;
}

double IndexedData::getDouble(size_t pos) const
{
    return p_->data->getDouble(pos);
}

long long IndexedData::getLongLong(size_t pos) const
{
    return p_->data->getLongLong(pos);
}

} /* namespace MetNoFimex */
