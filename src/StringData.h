/*
  Fimex, src/StringData.h

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

// -*- c++ -*-

#include "fimex/Data.h"

#include "fimex/CDMException.h"

namespace MetNoFimex {

class StringData : public Data
{
public:
    StringData(const std::string& text)
        : text_(text)
    {
    }

    size_t size() const { return text_.size(); }
    int bytes_for_one() const { return 1; }

    void* getDataPtr() { return (void*)(text_.data()); }

    void toStream(std::ostream& out, const std::string&) const { out << text_; };

    shared_array<char> asChar() const { throw CDMException("cannot convert string data to char"); }

    shared_array<short> asShort() const { throw CDMException("cannot convert string data to short"); }

    shared_array<int> asInt() const { throw CDMException("cannot convert string data to int"); }

    shared_array<long long> asInt64() const { throw CDMException("cannot convert string data to long long"); }

    shared_array<unsigned char> asUChar() const { throw CDMException("cannot convert string data to unsigned char"); }

    shared_array<unsigned short> asUShort() const { throw CDMException("cannot convert string data to unsigned short"); }

    shared_array<unsigned int> asUInt() const { throw CDMException("cannot convert string data to unsigned int"); }

    shared_array<unsigned long long> asUInt64() const { throw CDMException("cannot convert string data to unsigned long long"); }

    shared_array<float> asFloat() const { throw CDMException("cannot convert string data to float"); }

    shared_array<double> asDouble() const { throw CDMException("cannot convert string data to double"); }

    shared_array<std::string> asStrings() const
    {
        shared_array<std::string> strings(new std::string[1]);
        strings[0] = text_;
        return strings;
    }

    std::string asString(const std::string&) const { return text_; }

    double getDouble(size_t) { throw CDMException("cannot get string data as double"); }

    long long getLongLong(size_t) { throw CDMException("cannot get string data as long long"); }

    void setValue(size_t, double) { throw CDMException("cannot set string data from double"); }

    void setValues(size_t startPos, const Data& data, size_t first = 0, size_t end = -1);

    void setAllValues(double) { throw CDMException("cannot set string data from double"); }

    DataPtr clone() const;

    DataPtr slice(std::vector<size_t>, std::vector<size_t>, std::vector<size_t>) { throw CDMException("cannot sclice string data"); }

    DataPtr convertDataType(double, double, double, CDMDataType newType, double, double, double);

    DataPtr convertDataType(double, double, double, UnitsConverter_p, CDMDataType newType, double, double, double)
    {
        return convertDataType(0, 0, 0, newType, 0, 0, 0);
    }

    CDMDataType getDataType() const { return CDM_STRING; }

private:
    std::string text_;
};

} // namespace MetNoFimex
