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
class WdbCDMReaderParserInfo;

namespace wdb
{
class GridInformation;

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
	explicit WdbConnection(const WdbCDMReaderParserInfo & connectionSpec);
	~WdbConnection();

    /**
	 * Checking if already connected
	 *
	 */
	bool isConnected();


	/**
	 * Get a list of all available data that matches the given arguments.
	 *
	 * @throws WdbException on error
	 */
	void readGid(std::vector<GridData> & out, const WdbCDMReaderParserInfo & connectionSpec);


	typedef boost::shared_ptr<GridInformation> GridInformationPtr;
	/**
	 * Get grid information from database.
	 */
	GridInformationPtr readGridInformation(const std::string & gridName);

	/**
	 * Read a grid from database, placing it in the provided buffer.
	 *
	 * @param buffer Fetched data goes here.
	 * @param gridIdentifier Id of the data to fetch
	 *
	 * @returns a pointer to one-past the stored data
	 */
	float * getGrid(float * buffer, GridData::gid gridIdentifier);

private:

	enum QueryResultFormat
	{
		TextResult, BinaryResult
	};

	/**
	 * Make a database call.
	 *
	 * You should call PQclear on result to free memory.
	 *
	 * @throws WdbException on error
	 */
	PGresult * call_(const std::string & query, QueryResultFormat resultFormat = TextResult);

	PGconn * connection_;

	typedef std::map<std::string, GridInformationPtr> GridList;
	GridList gridsInUse_;
};

/**
 * Exception caused by errors when contacting a wdb database.
 */
class WdbException : public CDMException
{
public:
	explicit WdbException(PGconn * connection);
	explicit WdbException(const std::string & msg);
};

}

}

#endif /* WDBCONNECTION_H_ */
