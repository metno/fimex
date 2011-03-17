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

#ifndef WDBCONNECTION_H_
#define WDBCONNECTION_H_

#include "GridData.h"
#include <fimex/CDMException.h>
#include <string>
#include <vector>
#include <libpq-fe.h>


namespace MetNoFimex
{
namespace wdb
{


/**
 * A connection to a wdb database, with useful queries created as methods on
 * this object
 */
class WdbConnection
{
public:

	/**
	 * @throws WdbException on error
	 */
	explicit WdbConnection(const std::string & connectString);
	~WdbConnection();

	/**
	 * Get a list of all available data that matches the given arguments.
	 *
	 * @throws WdbException on error
	 */
	void readGid(std::vector<GridData> & out, const std::string & dataProvider);

private:
	/**
	 * Make a database call.
	 *
	 * You should call PQclear on result to free memory.
	 *
	 * @throws WdbException on error
	 */
	PGresult * call_(const std::string & query);

	PGconn * connection_;

	friend class WdbException;
};


class WdbException : public CDMException
{
public:
	explicit WdbException(PGconn * connection);
};

}

}

#endif /* WDBCONNECTION_H_ */
