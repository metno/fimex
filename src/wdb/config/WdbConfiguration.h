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

#ifndef WDBCONFIGURATION_H_
#define WDBCONFIGURATION_H_

#include "../database_access/WciReadQuerySpecification.h"
#include <boost/filesystem/path.hpp>
#include <iosfwd>

namespace MetNoFimex
{
namespace wdb
{

/**
 * Represents a wdb "file". Information about how to connect to a wdb
 * database, and what query to perform on it is given here. See the example
 * file example.wdb.conf for information about the file's syntax.
 */
class WdbConfiguration
{
public:
	/**
	 * Initialize with data read from the given file.
	 *
	 * @throws CDMException on error
	 */
	explicit WdbConfiguration(const boost::filesystem::path & configFile);

	~WdbConfiguration();

	/**
	 * Get a libpq-compatible string for connecting to a wdb database
	 */
	std::string pqDatabaseConnectString() const;

	/**
	 * username for wci call
	 */
	const std::string & wciUser() const { return wciUser_; }

	/**
	 * Get a specification for what wci.read query to perform on the database.
	 */
	const WciReadQuerySpecification & query() const { return querySpec_; }

private:
	void init_(const boost::filesystem::path & configFile);

	std::string database_;
	std::string host_;
	int port_;
	std::string user_;

	std::string wciUser_;

	WciReadQuerySpecification querySpec_;
};

}

}

#endif /* WDBCONFIGURATION_H_ */
