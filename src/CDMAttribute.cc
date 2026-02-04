/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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
#include "fimex/Data.h"
#include "fimex/DataUtils.h"
#include "fimex/NcmlCDMWriter.h"

namespace MetNoFimex {

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
    return getStringValueWithSeparator(" ");
}

std::string CDMAttribute::getStringValueWithSeparator(const std::string& separator) const
{
    if (!data)
        return std::string();
    return data->asString(separator);
}

CDMDataType CDMAttribute::getDataType() const
{
    return data->getDataType();
}

void CDMAttribute::toXMLStream(std::ostream& out, const std::string& indent) const
{
    NcmlCDMWriter::write(out, *this, indent);
}

} // namespace MetNoFimex
