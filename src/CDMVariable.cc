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

#include "fimex/CDMVariable.h"
#include "fimex/CDMAttribute.h"

#include <algorithm>
#include <ostream>

namespace MetNoFimex
{

// static
CDMVariable::SpatialVectorDirection CDMVariable::vectorDirectionFromString(const std::string& vd)
{
    if (vd.find("x") != std::string::npos)
        return CDMVariable::SPATIAL_VECTOR_X;
    else if (vd.find("y") != std::string::npos)
        return CDMVariable::SPATIAL_VECTOR_Y;
    else if (vd.find("lon") != std::string::npos)
        return CDMVariable::SPATIAL_VECTOR_LON;
    else if (vd.find("lat") != std::string::npos)
        return CDMVariable::SPATIAL_VECTOR_LAT;
    else
        return CDMVariable::SPATIAL_VECTOR_NONE;
}

CDMVariable::CDMVariable(std::string name, CDMDataType datatype, std::vector<std::string> shape)
    : name(name)
    , datatype(datatype)
    , shape(shape)
    , spatialVectorDirection(SPATIAL_VECTOR_NONE)
{
}

CDMVariable::~CDMVariable()
{
}

bool CDMVariable::checkDimension(const std::string& dimension) const
{
    const std::vector<std::string>& shape = getShape();
    return (std::find(shape.begin(), shape.end(), dimension) != shape.end());
}

void CDMVariable::setAsSpatialVector(const std::string& counterpart, SpatialVectorDirection direction)
{
    spatialVectorCounterpart = counterpart;
    spatialVectorDirection = direction;
}


void CDMVariable::shapeToXMLStream(std::ostream& out) const
{
	if (shape.size() > 0) {
		out << " shape=\"";
		for (unsigned int i = 0; i < shape.size(); i++) {
			out << shape[i];
			if (i < (shape.size()-1)) {
				out << " ";
			} else {
				out << "\" ";
			}
		}
	}
}
void CDMVariable::toXMLStream(std::ostream& out, const std::vector<CDMAttribute>& attrs) const
{
	out << "<variable name=\"" << getName() << "\" type=\"" << datatype2string(getDataType()) << "\" ";
	shapeToXMLStream(out);
	out << ">" << std::endl;
	for (std::vector<CDMAttribute>::const_iterator it = attrs.begin(); it != attrs.end(); ++it) {
		it->toXMLStream(out, "  ");
	}
	out << "</variable>" << std::endl;
}

void CDMVariable::toXMLStream(std::ostream& out) const
{
	out << "<variable name=\"" << getName() << "\" type=\"" << datatype2string(getDataType()) << "\"";
	shapeToXMLStream(out);
	out << "/>" << std::endl;
}

}
