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

#ifndef WDBINDEX_H_
#define WDBINDEX_H_

#include "ParameterData.h"
#include "database_access/GridData.h"
#include <boost/multi_array.hpp>
#include <vector>
#include <string>
#include <map>
#include <set>


namespace MetNoFimex
{
namespace wdb
{


/**
 * Storing data from wdb in a structured way, so that they can later be
 * retrieved for easy insertion into a CDM object.
 *
 * Data is stored as WdbIndex::gid, which are references to data blobs in a
 * wdb database.
 *
 * @see Wdb2CdmBuilder and MetNoFimex::CDM
 */
class WdbIndex
{
public:
	/**
	 * Create an index, based on the given data.
	 */
	explicit WdbIndex(const std::vector<GridData> & data);
	~WdbIndex();

	/**
	 * Grid identifier - this is what the database needs in order to retrieve
	 * the grid.
	 */
	typedef long long gid;

	/**
	 * A list of grid identifiers.
	 */
	typedef std::vector<gid> GidList;

	/**
	 * Dummy value, for use when there is no grid available for a request.
	 */
	static const gid UNDEFINED_GID;

	/**
	 * Get all data for the given parameter and timestep
	 */
	GidList getData(const std::string & parameter, unsigned unLimDimPos) const;

	GidList getData(const std::string & parameter) const;

	/**
	 * Get a list of all parameters that are stored here
	 */
	std::set<std::string> allParameters() const;

	/**
	 * Get the unit name for the given parameter
	 */
	const std::string & unitForParameter(const std::string & parameter) const;

	/**
	 * Get a list of all times that are in use.
	 */
	std::set<GridData::Duration> allTimes() const;

	/**
	 * Get all available times for the given parameter
	 */
	const std::vector<GridData::Duration> & timesForParameter(const std::string & parameter) const;

	bool hasLevel(const std::string & wdbName) const
	{
		return levels_.find(wdbName) != levels_.end();
	}

	/**
	 * Find a parameter's level type
	 */
	const LevelType & levelTypeForParameter(const std::string & parameter) const;

	/**
	 * Get a list of all available levels for the given parameter
	 */
	const std::vector<float> & levelsForParameter(const std::string & parameter) const;

	/**
	 * Get a list of all dataversions for a given parameter
	 */
	const std::vector<int> & versionsForParameter(const std::string & parameter) const;

	/**
	 * does the given parameter name exist in this object?
	 */
	bool hasParameter(const std::string & parameter) const;

	std::set<float> getLevelValues(const std::string & levelName) const;

	/**
	 * Get data's reference time
	 */
	std::set<GridData::Time> referenceTimes() const;

	const std::vector<GridData::Time> & referenceTimesForParameter(const std::string & parameter) const;

private:

	std::map<std::string, std::set<float> > levels_;
	std::set<GridData::Duration> allValidtimes_;
	std::set<GridData::Time> allReferenceTimes_;

	const ParameterData & parameterData_(const Parameter & p) const;

	typedef	std::map<Parameter, ParameterData> Data;
	Data data_;


	void init_(const std::vector<GridData> & data);
};

}
}

#endif /* WDBINDEX_H_ */
