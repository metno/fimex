/*
  Fimex, src/NativeData.cc

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/

#include "NativeData.h"

#include "fimex/Type2String.h"

namespace MetNoFimex {

void NativeData::setValues(size_t startPos, const Data& data, size_t first, size_t end)
{
    if (data.getDataType() != CDM_NAT)
        throw CDMException("cannot set native data from " + type2string(data.getDataType()) + " datatype");
    if (startPos != 0 || first != 0 || end != size_)
        throw CDMException("cannot set native data from " + type2string(data.getDataType()) + " slice");
    data_ = data.asUChar();
}

void NativeData::setAllValues(double d)
{
    std::fill(data_.get(), data_.get() + size_, static_cast<nat_t>(d));
}

DataPtr NativeData::clone() const
{
    nat_a cloned_data(new nat_t[size_]);
    std::copy(data_.get(), data_.get() + size_, cloned_data.get());
    return std::make_shared<NativeData>(size_, cloned_data);
}

DataPtr NativeData::slice(std::vector<size_t> max_dim_sizes, std::vector<size_t> /*dim_start_positions*/, std::vector<size_t> dim_sizes)
{
    if (max_dim_sizes != dim_sizes)
        throw CDMException("cannot sclice native data");
    return clone();
}

DataPtr NativeData::convertDataType(double, double, double, CDMDataType newType, double, double, double)
{
    if (newType == CDM_NAT) {
        return clone();
    } else {
        throw CDMException("cannot convert CDM_NAT to " + type2string(newType) + " datatype");
    }
}

} // namespace MetNoFimex
