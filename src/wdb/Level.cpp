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
#include <ostream>

namespace MetNoFimex
{
namespace wdb
{

Level::Level() :
		from_(0), to_(0)
{}

Level::Level(const std::string & levelName, float from, float to) :
	levelName_(levelName), from_(from), to_(to)
{
}

bool operator <(const Level & a, const Level & b)
{
	if (a.levelName() != b.levelName())
		return a.levelName() < b.levelName();
	if (a.from() != b.from())
		return a.from() < b.from();
	return a.to() < b.to();
}

std::ostream & operator << (std::ostream & s, const Level & l)
{
	if (l.from() != l.to())
		s << l.from() << " to ";
	s << l.to() << " " << l.levelName();

	return s;
}


}
}
