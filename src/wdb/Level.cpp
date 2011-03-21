/*
 fimex

 Copyright (C) 2011 met.no

 Contact information:
 Norwegian Meteorological Institute
 Box 43 Blindern
 0313 OSLO
 NORWAY
 E-mail: post@met.no

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA
 */

#include "Level.h"
#include "CdmNameTranslator.h"
#include <fimex/CDM.h>
#include <ostream>

namespace MetNoFimex
{
namespace wdb
{

LevelType::LevelType(const std::string & name, const std::string & unit) :
		name_(name), unit_(unit)
{}

void LevelType::addToCdm(CDM & cdm, long length, const CdmNameTranslator & translator) const
{
	const std::string & cdmName = translator.toCdmName(name_);

	cdm.addDimension(CDMDimension(cdmName, length));

	std::vector<std::string> shape;
	shape.push_back(cdmName);

	cdm.addVariable(CDMVariable(cdmName, CDM_FLOAT, shape));

	cdm.addAttribute(cdmName, CDMAttribute("long_name", name_));
	cdm.addAttribute(cdmName, CDMAttribute("standard_name", cdmName));
	cdm.addAttribute(cdmName, CDMAttribute("units", unit_));
	cdm.addAttribute(cdmName, CDMAttribute("axis", "z"));
}



std::ostream & operator << (std::ostream & s, const LevelType & t)
{
	return s << t.name();
}


Level::Level() :
	type_("", ""), from_(0), to_(0)
{}

Level::Level(const std::string & levelName, const std::string & unit, float from, float to) :
	type_(levelName, unit), from_(from), to_(to)
{
}

bool operator <(const Level & a, const Level & b)
{
	if (a.type() != b.type())
		return a.type() < b.type();
	if (a.from() != b.from())
		return a.from() < b.from();
	return a.to() < b.to();
}

std::ostream & operator << (std::ostream & s, const Level & l)
{
	if (l.from() != l.to())
		s << l.from() << " to ";
	s << l.to() << " " << l.type();

	return s;
}


}
}
