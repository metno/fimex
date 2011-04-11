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

#include "GridData.h"
#include <vector>
#include <string>
#include <map>
#include <set>


namespace MetNoFimex
{
namespace wdb
{

class WdbIndex
{
public:
	explicit WdbIndex(const std::vector<GridData> & data);
	~WdbIndex();

	typedef long long gid;
	typedef std::vector<gid> GidList;

	static const gid UNDEFINED_GID;

	GidList getData(const std::string & parameter, unsigned timeIndex) const;

	std::set<std::string> allParameters() const;
	const std::string & unitForParameter(const std::string & parameter) const;

	const std::set<GridData::Time> & allTimes() const;
	std::set<GridData::Time> timesForParameter(const std::string & parameter) const;

	const std::string & levelNameForParameter(const std::string & parameter) const;
	const std::string & unitForLevel(const std::string & level) const;
	std::set<float> levelsForParameter(const std::string & parameter) const;

	std::set<int> versionsForParameter(const std::string & parameter) const;

	bool hasParameter(const std::string & parameter) const
	{
		return data_.find(parameter) != data_.end();
	}

	const GridData::Time & referenceTime() const
	{
		return referenceTime_;
	}

private:

	typedef std::map<int, gid> VersionEntry;
	struct LevelEntry : public std::map<float, VersionEntry>
	{
		std::string levelName;
	};
	typedef std::map<GridData::Time, LevelEntry> TimeEntry;
	typedef std::map<std::string, TimeEntry> Data;


    void extractGids_(WdbIndex::GidList & out, const LevelEntry & levelEntry, const std::string & parameter) const;
    void extractMissingGidsForLevel_(WdbIndex::GidList & out, const std::string & parameter) const;


	Data data_;

	std::map<std::string, std::string> parameterUnits_;

	// all times in use
	std::set<GridData::Time> allTimes_;

	// level names to level values
	std::map<std::string, std::set<float> > allLevels_;
	std::map<std::string, std::string> levelUnits_;

	// parameter name to level name
	std::map<std::string, std::string> levelsForParameters_;

	bool hasMoreThanOneLevel_(const std::string & parameter) const;
	std::set<std::string> parametersWithMoreThanOneLevel_;
	std::set<int> allVersions_;

	GridData::Time referenceTime_;
};

}

}

#endif /* WDBINDEX_H_ */
