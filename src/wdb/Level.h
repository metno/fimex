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

#ifndef LEVEL_H_
#define LEVEL_H_

#include "fimex/CDMDimension.h"
#include "fimex/CDMAttribute.h"
#include <string>
#include <iosfwd>


namespace MetNoFimex
{
class CDM;

namespace wdb
{
class CdmNameTranslator;

/**
 * Represent a wdb level type, including level parameter unit.
 */
class LevelType
{
public:
	LevelType(const std::string & name, const std::string & unit);

	/**
	 * get level's name
	 */
	const std::string & name() const { return name_; }

	/**
	 * get level's unit
	 */
	const std::string & unit() const { return unit_; }


private:
	std::string name_;
	std::string unit_;
};

inline bool operator < (const LevelType & a, const LevelType & b)
{
	return a.name() < b.name();
}
inline bool operator == (const LevelType & a, const LevelType & b)
{
	return a.name() == b.name();
}
inline bool operator != (const LevelType & a, const LevelType & b)
{
	return not (a == b);
}
std::ostream & operator << (std::ostream & s, const LevelType & t);



/**
 * A level in the wdb database.
 */
class Level
{
public:
	Level();
	Level(const std::string & levelName, const std::string & unit, float from, float to);

	/**
	 * Get level type
	 */
	const LevelType & type() const { return type_; }

	/**
	 * lowest level this is valid for
	 */
	float from() const { return from_; }

	/**
	 * highest level this is valid for
	 */
	float to() const { return to_; }



private:
	LevelType type_;
	float from_;
	float to_;
};

bool operator < (const Level & a, const Level & b);
std::ostream & operator << (std::ostream & s, const Level & l);

}
}

#endif /* LEVEL_H_ */
