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

#include "fimex/CDMDimension.h"

#include <iostream>

namespace MetNoFimex
{

CDMDimension::CDMDimension()
: name("_null"), length(0), unlimited(0)
{
}

CDMDimension::CDMDimension(std::string name, long length)
: name(name), length(length), unlimited(0)
{
}

CDMDimension::~CDMDimension()
{
}

void CDMDimension::toXMLStream(std::ostream& out) const
{
	out << "<dimension name=\"" << getName() << "\" length=\"" << getLength() << "\" ";
	if (isUnlimited()) {
		out << "isUnlimited=\"true\" ";
	}
	out << "/>" << std::endl;
}

}
