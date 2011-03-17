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

#ifndef GRIDDATA_H_
#define GRIDDATA_H_

#include "Level.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <libpq-fe.h>

namespace MetNoFimex
{
namespace wdb
{

/**
 * Represents a single return row from wci.read(..., returngid)
 *
 * This class should only be instantiated by a WdbConnection object
 */
class GridData
{
public:
	~GridData();

	typedef boost::posix_time::ptime Time;
	typedef long long gid;

	const std::string & parameter() const { return parameter_; };
	const Level & level() const { return level_; }
	int version() const { return version_; };
	const Time & validTo() const { return validTo_; };
	gid gridIdentifier() const { return gridIdentifier_; };

private:
	std::string parameter_;
	Level level_;
	int version_;
	Time validTo_;
	gid gridIdentifier_;

	friend class WdbConnection;
	static std::string query(const std::string & dataProvider);
	GridData(PGresult * result, int row);
};

}
}

#endif /* GRIDDATA_H_ */
