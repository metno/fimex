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

#include "DataSanitizer.h"
#include <fimex/CDMException.h>
#include "boost/scoped_array.hpp"


namespace MetNoFimex
{
namespace wdb
{

DataSanitizer::DataSanitizer(PGconn * connection) :
		connection_(connection)
{
}

std::string DataSanitizer::operator () (const std::string & unsafeString) const
{
	boost::scoped_array<char> buffer(new char[(unsafeString.size() * 2) +1]);
	int error = 0;
	PQescapeStringConn(connection_, buffer.get(), unsafeString.c_str(), unsafeString.size(), & error);

	if ( error )
		throw CDMException(std::string("Error when creating string for database query: ") + PQerrorMessage(connection_));

	return std::string(buffer.get());
}

}
}
