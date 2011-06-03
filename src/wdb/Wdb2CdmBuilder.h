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

#ifndef DATAINDEX_H_
#define DATAINDEX_H_

#include "WdbIndex.h"
#include "DataHandler.h"
#include "database_access/GridData.h"
#include <vector>
#include <map>
#include <string>
#include <iosfwd>
#include <set>


namespace MetNoFimex
{
class CDM;


namespace wdb
{
class GlobalWdbConfiguration;
class Parameter;
class Level;


/**
 * Structure data, using it to populate a MetNoFimes::CDM object.
 *
 * The most important method here is populate, which adds data to a given cdm
 * object.
 */
class Wdb2CdmBuilder
{
public:

	/**
	 * Create builder, with the provided data. Configuration in
	 * GlobalWdbConfiguration will be added to any built CDM.
	 */
	Wdb2CdmBuilder(const std::vector<GridData> & data, const GlobalWdbConfiguration & config);
	~Wdb2CdmBuilder();

	/**
	 * Add all data structured by this object to the given cdm object
	 */
	void populate(CDM & cdm) const;

	typedef GridData::gid gid;
	typedef GridData::Time Time;

	/**
	 * Does the given variable name refer to a wdb parameter?
	 */
	bool isDatabaseField(const std::string & variableName) const;

	/**
	 * Does the given name refer to a level?
	 */
	bool isLevel(const std::string & levelName) const
	{
		try
		{
			getLevelValues(levelName);
			return true;
		}
		catch (...)
		{
			return false;
		}
	}

	/**
	 * Get all values for the given level type
	 */
	std::set<float> getLevelValues(const std::string & levelName) const;

	/**
	 * get all grid idenitfiers for the given variable name and timestep
	 */
	std::vector<gid> getGridIdentifiers(const std::string & variableName, int timeIndex) const
	{
		return index_.getData(variableName, timeIndex);
	}

	/**
	 * Get information about the grid in use.
	 *
	 * @note atm we only support a single grid in each Wdb2CdmBuilder
	 */
	const GridInformation & gridInformation() const;

	/**
	 * Get a list of all times in use
	 */
	std::set<GridData::Duration> allTimes() const
	{
		return index_.allTimes();
	}

	/**
	 * Get the reference time for the data in this object.
	 *
	 * @note Only one reference time may exist in the sate Wdb2CdmBuilder
	 * object.
	 */
	std::set<GridData::Time> referenceTimes() const;

	const std::vector<DataHandler::Ptr> & dataHandlers() const { return dataHandlers_; }

private:

	void addDimensions_(CDM & cdm) const;
	void addVersionDimension_(CDM & cdm) const;

	void addParameterVariables_(CDM & cdm) const;


	const WdbIndex index_;
	const GlobalWdbConfiguration & config_;

	typedef std::map<std::string, GridData::GridInformationPtr> GridSpecMap;
	GridSpecMap grids_;

	std::vector<DataHandler::Ptr> dataHandlers_;
};

}

}

#endif /* DATAINDEX_H_ */
