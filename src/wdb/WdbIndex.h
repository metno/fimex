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

#include "database_access/GridData.h"
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
	explicit WdbIndex(const std::vector<GridData> & data);
	~WdbIndex();

	typedef long long gid;
	typedef std::vector<gid> GidList;


	static const gid UNDEFINED_GID;

	/**
	 * Get all data for the given parameter and timestep
	 */
	GidList getData(const std::string & parameter, unsigned timeIndex) const;

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
	std::set<GridData::Time> allTimes() const;

	/**
	 * Get all available times for the given parameter
	 */
	std::set<GridData::Time> timesForParameter(const std::string & parameter) const;

	/**
	 * Find a parameter's level type
	 */
	const LevelType & levelTypeForParameter(const std::string & parameter) const;

	/**
	 * Get a list of all available levels for the given parameter
	 */
	std::set<float> levelsForParameter(const std::string & parameter) const;

	/**
	 * Get a list of all dataversions for a given parameter
	 */
	std::set<int> versionsForParameter(const std::string & parameter) const;

	/**
	 * does the given parameter name exist in this object?
	 */
	bool hasParameter(const std::string & parameter) const;

	std::set<float> getLevelValues(const std::string & levelName) const;

	/**
	 * Get data's reference time
	 */
	std::set<GridData::Time> referenceTimes() const;

private:

//	typedef std::map<GridData::Time, GidList> Entries;
//	typedef std::map<Parameter, Entries> Data;
//
//	Data data_;


	typedef std::map<int, gid> VersionEntry;
	typedef std::map<Level, VersionEntry> LevelEntry;
	typedef std::map<GridData::Time, LevelEntry> ReferenceTimeEntry;
	typedef std::map<GridData::Time, ReferenceTimeEntry> ValidTimeEntry;
	typedef std::map<Parameter, ValidTimeEntry> Data;

	Data entries;

	std::map<std::string, std::set<float> > levels_;
};

}

}

#endif /* WDBINDEX_H_ */
