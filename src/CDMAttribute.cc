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

#include <boost/shared_ptr.hpp>
#include <boost/regex.hpp>
#include <cmath>
#include "fimex/Data.h"
#include "fimex/Utils.h"
#include "fimex/CDMException.h"
#include "fimex/CDMNamedEntity.h"
#include "fimex/CDMconstants.h"
#include "fimex/coordSys/Projection.h"

namespace MetNoFimex
{

CDMAttribute::CDMAttribute()
: name(""), datatype(CDM_NAT)
{
}

CDMAttribute::CDMAttribute(std::string name, std::string value)
: name(name), datatype(CDM_STRING)
{
    boost::shared_array<char> cstr(new char [value.size()]);
    for (size_t i = 0; i < value.size(); i++) {
        cstr[i] = value.at(i);
    }
    data = createData(value.size(), cstr);
}


CDMAttribute::CDMAttribute(std::string name, double value)
: name(name), datatype(CDM_DOUBLE)
{
    boost::shared_array<double> xvalue(new double[1]);
    xvalue[0] = value;
    data = createData(1, xvalue);
}

CDMAttribute::CDMAttribute(std::string name, int value)
: name(name), datatype(CDM_INT)
{
    boost::shared_array<int> xvalue(new int[1]);
    xvalue[0] = value;
    data = createData(1, xvalue);
}

CDMAttribute::CDMAttribute(std::string name, short value)
: name(name), datatype(CDM_SHORT)
{
    boost::shared_array<short> xvalue(new short[1]);
    xvalue[0] = value;
    data = createData(1, xvalue);
}

CDMAttribute::CDMAttribute(std::string name, char value)
: name(name), datatype(CDM_CHAR)
{
    boost::shared_array<char> xvalue(new char[1]);
    xvalue[0] = value;
    data = createData(1, xvalue);
}

CDMAttribute::CDMAttribute(std::string name, float value)
: name(name), datatype(CDM_FLOAT)
{
    boost::shared_array<float> xvalue(new float[1]);
    xvalue[0] = value;
    data = createData(1, xvalue);
}

CDMAttribute::CDMAttribute(std::string name, CDMDataType datatype, DataPtr data)
: name(name), datatype(datatype), data(data)
{
}

CDMAttribute::CDMAttribute(const std::string& name, const std::string& datatype, const std::string& value)
: name(name)
{
    this->datatype = string2datatype(datatype);
    std::vector<std::string> vals;
    vals.push_back(value);
    initDataByArray(vals);
}

CDMAttribute::CDMAttribute(const std::string& name, CDMDataType datatype, const std::vector<std::string>& values)
: name(name), datatype(datatype)
{
    initDataByArray(values);
}

void CDMAttribute::initDataByArray(const std::vector<std::string>& values)
{
    switch (datatype) {
    case CDM_FLOAT: {
        initDataArray<float>(values);
        break;
    }
    case CDM_DOUBLE: {
        initDataArray<double>(values);
        break;
    }
    case CDM_INT:  {
        initDataArray<int>(values);
        break;
    }
    case CDM_SHORT: {
        initDataArray<short>(values);
        break;
    }
    case CDM_CHAR: {
        initDataArray<char>(values);
        break;
    }
    case CDM_STRING: {
        /* string may only have one dimension */
        *this = CDMAttribute(name, join(values.begin(), values.end(), " "));
        break;
    }
    case CDM_STRINGS: {
        boost::shared_array<std::string> array(new std::string[values.size()]);
        std::copy(values.begin(), values.end(), &array[0]);
        data = createData(values.size(), array);
        break;
    }
    default: {
        throw CDMException("Unknown type to generate attribute " + name);
    }
    }
}

CDMAttribute::~CDMAttribute()
{
}

const std::string CDMAttribute::getStringValue() const
{
    if (data.get() != 0) {
        if ((data->getDataType() == CDM_STRING) || (data->getDataType() == CDM_CHAR))
            return data->asString();
        else
            return data->asString(" ");
    } else
        return "";
}

void CDMAttribute::toXMLStream(std::ostream& out, const std::string& indent) const
{
    out << indent << "<attribute name=\"" << getName() << "\" type=\"" << datatype2string(getDataType()) << "\" value=\"" << getStringValue() << "\" />" << std::endl;
}

/* init data arrays for all types */
template<typename T>
void CDMAttribute::initDataArray(const std::vector<std::string>& values) {
    boost::shared_array<T> array(new T[values.size()]);
    std::transform(values.begin(), values.end(), &array[0], &string2type<T>);
    data = createData(values.size(), array);
}


std::vector<CDMAttribute> projStringToAttributes(std::string projStr)
{
    boost::shared_ptr<Projection> proj = Projection::createByProj4(projStr);
    return proj->getParameters();
}

std::string attributesToProjString(const std::vector<CDMAttribute>& attrs)
{
    return Projection::create(attrs)->getProj4String();
}


}
