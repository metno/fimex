/*
  Fimex, src/StringData.cc

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

#include "StringData.h"

#include "fimex/CDMDataType.h"

namespace MetNoFimex {

void StringData::setValues(size_t startPos, const Data& data, size_t first, size_t end)
{
    std::string text;
    if (data.getDataType() == CDM_STRING)
        text = data.asString();
    else if (data.getDataType() == CDM_STRINGS && data.size() == 1)
        text = data.asStrings()[0];
    else
        throw CDMException("cannot set string data from " + datatype2string(data.getDataType()) + " datatype");
    if (end == size_t(-1))
        end = text.length();
    size_t n = end - first;
    text_.replace(startPos, n, text, first, n);
}

void StringData::setAllValues(double d)
{
    text_.replace(0, text_.size(), text_.size(), static_cast<char>(d));
}

DataPtr StringData::clone() const
{
    return std::make_shared<StringData>(text_);
}

DataPtr StringData::slice(const std::vector<size_t>& max_dim_sizes, const std::vector<size_t>& /*dim_start_positions*/, const std::vector<size_t>& dim_sizes)
{
    if (max_dim_sizes != dim_sizes)
        throw CDMException("cannot sclice string data");
    return clone();
}

DataPtr StringData::convertDataType(double, double, double, CDMDataType newType, double, double, double)
{
    if (newType == CDM_STRING) {
        return std::make_shared<StringData>(text_);
    } else if (newType == CDM_STRINGS) {
        return createData(1, asStrings());
    } else {
        throw CDMException("cannot convert string to " + datatype2string(newType) + " datatype");
    }
}

} // namespace MetNoFimex
