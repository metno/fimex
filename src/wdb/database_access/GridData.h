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
#include <postgresql/libpq-fe.h>


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
 * This class should only be instantiated by a WdbConnection object. The
 * protected methods in this class is solely intended used for testing
 * purposes.
 */
class GridData
{
public:
	~GridData();

	typedef boost::posix_time::ptime Time;
	typedef boost::posix_time::time_duration Duration;
	typedef boost::shared_ptr<GridInformation> GridInformationPtr;
	typedef long long gid;

	/**
	 * Get the data's value parameter type
	 */
	const Parameter & parameter() const { return parameter_; };

	/**
	 * Get level information for the data
	 */
	const Level & level() const { return level_; }

	/**
	 * Get data version (most often ensemble member number).
	 */
	int version() const { return version_; };

	/**
	 * Latest validity point for data
	 */
	const Time & validTo() const { return validTo_; };

	Duration validTimeRelativeToReferenceTime() const
	{
		return validTo() - referenceTime();
	}

	/**
	 * Get reference time, also called run time for model.
	 */
	const Time & referenceTime() const { return referenceTime_; }



	/**
	 * Get a name for the grid the data is valid for
	 */
	const std::string & placeName() const { return placeName_; }

	/**
	 * Get detailed information about the data's grid
	 */
	const GridInformationPtr & gridInformation() const { return gridInformation_; }

	/**
	 * Get grid identifier, for retrieving of the actual data.
	 */
	gid gridIdentifier() const { return gridIdentifier_; };

protected:
	/**
	 * This constructor is meant for artificially generating objects for tests
	 * - via subclasses
	 */
	GridData(const Parameter & param, const Level & lvl, int version, const Time & validTo, const Time & referenceTime, gid gridId);

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
	GridData(PGresult * result, int row);
};

}
}

#endif /* GRIDDATA_H_ */
