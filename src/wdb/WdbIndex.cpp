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

#include "WdbIndex.h"
#include <boost/foreach.hpp>
#include <numeric>
#include <iterator>

namespace MetNoFimex
{
namespace wdb
{

const WdbIndex::gid WdbIndex::UNDEFINED_GID = std::numeric_limits<gid>::max();


WdbIndex::WdbIndex(const std::vector<GridData> & data)
{
	BOOST_FOREACH(const GridData & d, data)
	{
		if ( referenceTime_.is_not_a_date_time() )
			referenceTime_ = d.referenceTime();
		else if ( referenceTime_ != d.referenceTime() )
			throw CDMException("multiple reference times in data from wdb");

		parameterUnits_[d.parameter().name()] = d.parameter().unit();

		LevelEntry & levelEntry = data_[d.parameter().name()] [d.validTo()];
		if ( levelEntry.levelName.empty() )
		{
			levelEntry.levelName = d.level().type().name();
			levelsForParameters_[d.parameter().name()] = levelEntry.levelName;
			levelUnits_[d.level().type().name()] = d.level().type().unit();
		}
		else if ( levelEntry.levelName != d.level().type().name() )
			throw CDMException("Only one level type is allowed for each parameter");

		levelEntry [d.level().to()] [d.version()] = d.gridIdentifier();
		if ( levelEntry.size() > 1 )
			parametersWithMoreThanOneLevel_.insert(d.parameter().name());


		allTimes_.insert(d.validTo());
		allLevels_[d.level().type().name()].insert(d.level().to());
		allVersions_.insert(d.version());
	}
}

WdbIndex::~WdbIndex()
{
}

WdbIndex::GidList WdbIndex::getData(const std::string & parameter, unsigned timeIndex) const
{
	WdbIndex::GidList ret;
	Data::const_iterator param = data_.find(parameter);
	if ( param == data_.end() )
		throw CDMException(parameter + ": No such parameter in index");

	const TimeEntry & timeEntry = param->second;

	if ( allTimes_.size() < timeIndex)
		throw CDMException("Invalid time index");

	if ( timeIndex == 0 )
	{
		TimeEntry::const_iterator find = timeEntry.begin();
		if ( find != timeEntry.end() )
			extractGids_(ret, find->second, parameter);
		else
			extractMissingGidsForLevel_(ret, parameter);
	}
	else
	{
		std::set<GridData::Time>::const_iterator allTimesIt = allTimes_.begin();
		std::advance(allTimesIt, timeIndex -1);
		TimeEntry::const_iterator find = timeEntry.find(* allTimesIt);
		if ( find != timeEntry.end() )
			extractGids_(ret, find->second, parameter);
		else
			extractMissingGidsForLevel_(ret, parameter);
	}

	return ret;
}

std::set<std::string> WdbIndex::allParameters() const
{
	std::set<std::string> ret;
	BOOST_FOREACH(const Data::value_type & d, data_)
		ret.insert(d.first);
	return ret;
}

const std::string & WdbIndex::unitForParameter(const std::string & parameter) const
{
	std::map<std::string, std::string>::const_iterator find = parameterUnits_.find(parameter);
	if ( find == parameterUnits_.end() )
		throw CDMException(parameter + ": no such parameter");
	return find->second;
}

const std::set<GridData::Time> & WdbIndex::allTimes() const
{
	return allTimes_;
}

std::set<GridData::Time> WdbIndex::timesForParameter(const std::string & parameter) const
{
	Data::const_iterator find = data_.find(parameter);
	if ( find == data_.end() )
		throw CDMException(parameter + ": no such parameter");

	const TimeEntry & timeEntry = find->second;
	if ( timeEntry.size() == 1 )
	{
		std::set<GridData::Time> time;
		time.insert(timeEntry.begin()->first);
		return time;
	}
	else
		return allTimes_;
}

const std::string & WdbIndex::levelNameForParameter(const std::string & parameter) const
{
	std::map<std::string, std::string>::const_iterator find = levelsForParameters_.find(parameter);
	if ( find == levelsForParameters_.end() )
		throw CDMException(parameter + ": no such parameter");
	return find->second;
}

const std::string & WdbIndex::unitForLevel(const std::string & level) const
{
	std::map<std::string, std::string>::const_iterator find = levelUnits_.find(level);
	if ( find == levelUnits_.end() )
		throw CDMException(level + ": no such level");
	return find->second;
}

std::set<float> WdbIndex::levelsForParameter(const std::string & parameter) const
{
	if ( hasMoreThanOneLevel_(parameter) )
	{
		const std::string & levelName = levelNameForParameter(parameter);
		std::map<std::string, std::set<float> >::const_iterator find = allLevels_.find(levelName);
		if ( find == allLevels_.end() )
			throw CDMException("internal error: cannot find level");
		return find->second;
	}
	else
	{
		Data::const_iterator find = data_.find(parameter);
		if ( find == data_.end() )
			throw CDMException(parameter + ": no such parameter");
		for ( TimeEntry::const_iterator it = find->second.begin(); it != find->second.end(); ++ it )
		{
			const LevelEntry & levelEntry = it->second;
			if ( not levelEntry.empty() )
			{
				std::set<float> ret;
				ret.insert(levelEntry.begin()->first);
				return ret;
			}
		}
		throw CDMException("intenal error: parameter has no levels");
	}
}

std::set<int> WdbIndex::versionsForParameter(const std::string & parameter) const
{
	std::set<int> versions;

	BOOST_FOREACH(const Data::value_type & d, data_)
		BOOST_FOREACH(const TimeEntry::value_type & te, d.second)
			BOOST_FOREACH(const LevelEntry::value_type & le, te.second)
				if ( le.second.size() > 1 )
					return allVersions_;
				else if ( not le.second.empty() )
					versions.insert(le.second.begin()->first);

	if ( versions.size() > 1 )
		return allVersions_;

	return versions;
}

void WdbIndex::extractGids_(WdbIndex::GidList & out, const LevelEntry & levelEntry, const std::string & parameter) const
{
	std::map<std::string, std::set<float> >::const_iterator levelType = allLevels_.find(levelEntry.levelName);
	if ( levelType == allLevels_.end() )
		throw CDMException("internal error: level lookup"); // should never happen


	std::set<float> levels = levelsForParameter(parameter);
	BOOST_FOREACH(float lvl, levels)
	{
		LevelEntry::const_iterator find = levelEntry.find(lvl);
		if ( find != levelEntry.end() )
		{
			const VersionEntry & versionEntry = find->second;
			BOOST_FOREACH(int version, allVersions_)
			{
				VersionEntry::const_iterator find = versionEntry.find(version);
				if ( find != versionEntry.end() )
					out.push_back(find->second);
				else
					out.push_back(UNDEFINED_GID);
			}
		}
		else
			out.push_back(UNDEFINED_GID);
	}
}

void WdbIndex::extractMissingGidsForLevel_(WdbIndex::GidList & out, const std::string & parameter) const
{
	std::set<float> levels = levelsForParameter(parameter);
	BOOST_FOREACH( float lvl, levels )
		BOOST_FOREACH( int v, versionsForParameter(parameter) )
			out.push_back(UNDEFINED_GID);
}

bool WdbIndex::hasMoreThanOneLevel_(const std::string & parameter) const
{
	return parametersWithMoreThanOneLevel_.find(parameter) != parametersWithMoreThanOneLevel_.end();
}

}
}
