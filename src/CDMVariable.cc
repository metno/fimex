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

namespace MetNoFimex
{

CDMVariable::CDMVariable(std::string name, CDMDataType datatype, std::vector<std::string> shape)
: name(name), datatype(datatype), shape(shape)
{
}

CDMVariable::~CDMVariable()
{
}

bool CDMVariable::checkDimension(const std::string& dimension) const {
	const std::vector<std::string>& shape = getShape();
	if (std::find(shape.begin(), shape.end(), dimension) != shape.end()) {
		return true;
	}
	return false;
}

void CDMVariable::setAsSpatialVector(const std::string& counterpart, const std::string& direction) {
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
