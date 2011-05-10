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

#include "Parameter.h"
#include "Level.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/shared_ptr.hpp>
#include <libpq-fe.h>


namespace MetNoFimex
{
namespace wdb
{
class WdbCDMReaderParserInfo;
class GridInformation;
class DataSanitizer;

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
	typedef boost::shared_ptr<GridInformation> GridInformationPtr;
	typedef long long gid;

	const Parameter & parameter() const { return parameter_; };
	const Level & level() const { return level_; }
	int version() const { return version_; };
	const Time & validTo() const { return validTo_; };
	const Time & referenceTime() const { return referenceTime_; }
	const std::string & placeName() const { return placeName_; }
	const GridInformationPtr & gridInformation() const { return gridInformation_; }
	gid gridIdentifier() const { return gridIdentifier_; };

protected:
	/**
	 * This constructor is meant for artificially generating objects for tests
	 * - via subclasses
	 */
	GridData(const Parameter & param, const Level & lvl, int version, const Time & validTo, gid gridId);

	void setGridInformation(GridInformationPtr gridInformation)
	{
		gridInformation_ = gridInformation;
	}

private:
	Parameter parameter_;
	Level level_;
	int version_;
	Time validTo_;
	Time referenceTime_;
	std::string placeName_;
	GridInformationPtr gridInformation_;
	gid gridIdentifier_;

	friend class WdbConnection;
	static std::string query(const WdbCDMReaderParserInfo & querySpec, const DataSanitizer & sanitizer);
	GridData(PGresult * result, int row);
};

}
}

#endif /* GRIDDATA_H_ */
