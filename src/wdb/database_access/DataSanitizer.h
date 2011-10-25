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

#ifndef DATASANITIZER_H_
#define DATASANITIZER_H_


#include <postgresql/libpq-fe.h>
#include <string>


namespace MetNoFimex
{
namespace wdb
{

/**
 * Performs string escaping, for inserting unsafe strings into database queries.
 */
class DataSanitizer
{
public:
	/**
	 * Construct sanitizer, giving the connection the resulting queries are
	 * meant to be used on. Giving connection is required since the underlying
	 * libpq uses the connection for sanitizing data.
	 */
	explicit DataSanitizer(PGconn * connection);

	virtual ~DataSanitizer() {}

	/**
	 * Clean an unsafe string for use in a query
	 */
	virtual std::string operator () (const std::string & unsafeString) const;

private:
	PGconn * connection_;
};

}
}

#endif /* DATASANITIZER_H_ */
