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

#ifndef PARAMETER_H_
#define PARAMETER_H_

#include "fimex/CDMVariable.h"
#include <string>
#include <vector>



namespace MetNoFimex
{
namespace wdb
{

/**
 * Representation of a wdb parameter, with parameter unit information
 */
class Parameter
{
public:
	Parameter() {}
	Parameter(const std::string & name, const std::string & unit);
	~Parameter();

	/**
	 * Get wdb parameter name
	 */
	const std::string & name() const { return name_; }

	/**
	 * get wdb parameter unit
	 */
	const std::string & unit() const { return unit_; }


private:
	std::string name_;
	std::string unit_;

};

inline bool operator < (const Parameter & a, const Parameter & b)
{
	return a.name() < b.name();
}

}

}

#endif /* PARAMETER_H_ */
