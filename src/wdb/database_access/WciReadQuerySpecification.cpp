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

#include "WciReadQuerySpecification.h"
#include "DataSanitizer.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/foreach.hpp>
#include <sstream>

namespace MetNoFimex
{
namespace wdb
{

WciReadQuerySpecification::WciReadQuerySpecification()
{
}

namespace
{
class StringBuilder
{
	const DataSanitizer & sanitizer_;

public:
	StringBuilder(const DataSanitizer & sanitizer) : sanitizer_(sanitizer) {}

	std::ostream & add(std::ostream & s, const std::set<std::string> * t) const
	{
		if ( ! t )
			return s << "NULL";

		s << "'{";
		std::set<std::string>::const_iterator it = t->begin();
		if ( it != t->end() )
		{
			s << sanitizer_(* it);
			while( ++ it != t->end() )
				s << ", " << sanitizer_(* it);
		}
		return s << "}'";
	}

	std::ostream & add(std::ostream & s, const std::set<int> * t) const
	{
		if ( ! t )
			return s << "NULL";

		s << "'{";
		std::set<int>::const_iterator it = t->begin();
		if ( it != t->end() )
		{
			s << * it;
			while( ++ it != t->end() )
				s << ", " << * it;
		}
		return s << "}'";
	}

	std::ostream & add(std::ostream & s, const std::string * text) const
	{
		if ( ! text )
			return s << "NULL";
		return s << '\'' << sanitizer_(* text) << '\'';
	}

	std::ostream & add(std::ostream & s, const WciReadQuerySpecification::Time * time) const
	{
		if ( ! time )
			return s << "NULL";
		return s << '\'' << * time << "+00" << '\'';
	}
};

}


std::string WciReadQuerySpecification::query(const DataSanitizer & sanitizer) const
{
//	const std::string dataProvider = array(sanitizer(querySpec.provider()));
//	const std::string placeName = sqlString(sanitizer(querySpec.place()));
//	const std::string referenceTime = sqlString(sanitizer(querySpec.referenceTime()));

	StringBuilder toString(sanitizer);

	std::ostringstream q;

	q << "SELECT "
		"ValueParameterName, "
		"ValueParameterUnit, "
		"LevelParameterName, "
		"LevelUnitName, "
		"LevelFrom, LevelTo, "
		"DataVersion, "
		"extract(epoch from ReferenceTime),"
		"extract(epoch from ValidTimeFrom), "
		"extract(epoch from ValidTimeTo), "
		"PlaceName, "
		"value"
		" FROM "
		"wci.read(";

	toString.add(q, dataProvider()) << ", ";
	toString.add(q, location()) << ", ";
	toString.add(q, referenceTime()) << ", ";
	q << "NULL, ";
	toString.add(q, parameter()) << ", ";
	q << "NULL, ";
	toString.add(q, dataVersion());
	q << ", NULL::wci.returngid)";

	return q.str();
}

void WciReadQuerySpecification::addDataProvider(const std::string & dataProvider)
{
	dataProvider_.insert(dataProvider);
}

void WciReadQuerySpecification::setLocation(const std::string & location)
{
	location_ = location;
}

void WciReadQuerySpecification::setReferenceTime(const std::string & referenceTime)
{
	referenceTime_ = referenceTime;
}

void WciReadQuerySpecification::addParameter(const std::string & parameter)
{
	parameter_.insert(parameter);
}

void WciReadQuerySpecification::addDataVersion(int version)
{
	dataVersion_.insert(version);
}


}
}
