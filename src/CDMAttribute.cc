/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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
 */

#include "fimex/CDMAttribute.h"

#include "fimex/CDMException.h"
#include "fimex/CDMNamedEntity.h"
#include "fimex/CDMconstants.h"
#include "fimex/Data.h"
#include "fimex/Utils.h"
#include "fimex/coordSys/Projection.h"

#include <cmath>
#include <memory>
#include <regex>

namespace MetNoFimex {

namespace {
/* init data arrays for all types */
template <typename T>
DataPtr initDataArray(const std::vector<std::string>& values)
{
    shared_array<T> array(new T[values.size()]);
    std::transform(values.begin(), values.end(), &array[0], &string2type<T>);
    return createData(values.size(), array);
}

DataPtr initDataByArray(CDMDataType datatype, const std::vector<std::string>& values)
{
    switch (datatype) {
    case CDM_FLOAT:
        return initDataArray<float>(values);
    case CDM_DOUBLE:
        return initDataArray<double>(values);
    case CDM_INT64:
        return initDataArray<long long>(values);
    case CDM_UINT64:
        return initDataArray<unsigned long long>(values);
    case CDM_INT:
        return initDataArray<int>(values);
    case CDM_UINT:
        return initDataArray<unsigned int>(values);
    case CDM_SHORT:
        return initDataArray<short>(values);
    case CDM_USHORT:
        return initDataArray<unsigned short>(values);
    case CDM_CHAR:
        return initDataArray<char>(values);
    case CDM_UCHAR:
        return initDataArray<unsigned char>(values);
    case CDM_STRING: {
        /* string may only have one dimension */
        const std::string value = join(values.begin(), values.end(), " ");
        return createData(CDM_STRING, value.begin(), value.end());
    }
    case CDM_STRINGS: {
        shared_array<std::string> array(new std::string[values.size()]);
        std::copy(values.begin(), values.end(), &array[0]);
        return createData(values.size(), array);
    }
    default:
        throw CDMException("Unknown type to generate attribute");
    }
}

} // namespace

CDMAttribute::CDMAttribute()
    : data(createData(CDM_NAT, 0))
{
}

CDMAttribute::CDMAttribute(std::string name, std::string value)
    : name(name)
    , data(createData(value))
{
}

CDMAttribute::CDMAttribute(std::string name, double value)
    : name(name)
    , data(createData(CDM_DOUBLE, &value, (&value) + 1))
{
}

CDMAttribute::CDMAttribute(std::string name, int value)
    : name(name)
    , data(createData(CDM_INT, &value, (&value) + 1))
{
}

CDMAttribute::CDMAttribute(std::string name, short value)
    : name(name)
    , data(createData(CDM_SHORT, &value, (&value) + 1))
{
}

CDMAttribute::CDMAttribute(std::string name, char value)
    : name(name)
    , data(createData(CDM_CHAR, &value, (&value) + 1))
{
}

CDMAttribute::CDMAttribute(std::string name, float value)
    : name(name)
    , data(createData(CDM_FLOAT, &value, (&value) + 1))
{
}

CDMAttribute::CDMAttribute(std::string name, DataPtr data)
    : name(name)
    , data(data)
{
}

CDMAttribute::CDMAttribute(const std::string& name, const std::string& datatype, const std::string& value)
    : name(name)
    , data(initDataByArray(string2datatype(datatype), std::vector<std::string>(1, value)))
{
}

CDMAttribute::CDMAttribute(const std::string& name, CDMDataType datatype, const std::vector<std::string>& values)
    : name(name)
    , data(initDataByArray(datatype, values))
{
}

CDMAttribute::~CDMAttribute()
{
}

std::string CDMAttribute::getStringValue() const
{
    if (!data)
        return std::string();
    if (data->getDataType() == CDM_STRING)
        return data->asString(" ");
    else
        return data->asString(" ");
}

CDMDataType CDMAttribute::getDataType() const
{
    return data->getDataType();
}

void CDMAttribute::toXMLStream(std::ostream& out, const std::string& indent) const
{
    out << indent << "<attribute name=\"" << getName() << "\" type=\"" << datatype2string(getDataType()) << "\" value=\"" << getStringValue() << "\" />"
        << std::endl;
}

} // namespace MetNoFimex
