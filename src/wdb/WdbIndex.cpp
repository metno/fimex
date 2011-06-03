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
	std::set<int> allVersions;
	std::set<Level> allLevels;
	std::map<Parameter, LevelType> levelsForParameter;

	// First, pass through all data, and store it in structure
	BOOST_FOREACH(const GridData & d, data)
	{
		VersionEntry & ve = entries
				[d.parameter()]
				 [d.referenceTime()]
				  [d.validTo() - d.referenceTime()]
				   [d.level()];

		if ( ve.find(d.version()) != ve.end() )
			throw CDMException("Duplicate data in data set!");

		ve[d.version()] = d.gridIdentifier();

		// temp variables
		allVersions.insert(d.version());
		allLevels.insert(d.level());

		allValidtimes_.insert(d.validTo() - d.referenceTime());
		allReferenceTimes_.insert(d.referenceTime());
		levels_[d.level().type().name()].insert(d.level().to());


		// Veryfy that each parameter has only one level
		std::map<Parameter, LevelType>::const_iterator find = levelsForParameter.find(d.parameter());
		if ( find == levelsForParameter.end() )
			levelsForParameter.insert(std::make_pair(d.parameter(), d.level().type()));
		else if ( find->second != d.level().type() )
			throw CDMException("Many levels for same parameter");
	}

	// then; fill in missing entries
	for ( Data::iterator it = entries.begin(); it != entries.end(); ++ it )
	{
		ReferenceTimeEntry & referenceTimeEntry = it->second;

		// Locate all existing entries of each type for each dimension
		std::set<GridData::Time> referenceTimes;
		std::set<GridData::Duration> validTimes;
		std::set<Level> levels;
		std::set<int> versions;
		for ( ReferenceTimeEntry::const_iterator referenceTime = referenceTimeEntry.begin(); referenceTime != referenceTimeEntry.end(); ++ referenceTime )
		{
			referenceTimes.insert(referenceTime->first);
			const ValidTimeEntry & validTimeEntry = referenceTime->second;
			for ( ValidTimeEntry::const_iterator validTime = validTimeEntry.begin(); validTime != validTimeEntry.end(); ++ validTime )
			{
				validTimes.insert(validTime->first);
				const LevelEntry & levelEntry = validTime->second;
				for ( LevelEntry::const_iterator level = levelEntry.begin(); level != levelEntry.end(); ++ level )
				{
					levels.insert(level->first);
					const VersionEntry & versionEntry = level->second;
					for ( VersionEntry::const_iterator version = versionEntry.begin(); version != versionEntry.end(); ++ version )
						versions.insert(version->first);
				}
			}
		}

		// Entries with more than one reference time should have all reference times available
		if ( referenceTimes.size() > 1 )
		{
			BOOST_FOREACH( const GridData::Time & t, allReferenceTimes_ )
				referenceTimeEntry[t];
		}

		for ( ReferenceTimeEntry::iterator referenceTime = referenceTimeEntry.begin(); referenceTime != referenceTimeEntry.end(); ++ referenceTime )
		{
			ValidTimeEntry & validTimeEntry = referenceTime->second;
			if (validTimes.size() > 1 )
				BOOST_FOREACH( const GridData::Duration & d, allValidtimes_ )
					validTimeEntry[d];
			else
				validTimeEntry[* validTimes.begin()];

			for ( ValidTimeEntry::iterator validTime = validTimeEntry.begin(); validTime != validTimeEntry.end(); ++ validTime )
			{
				LevelEntry & levelEntry = validTime->second;
				if ( levels.size() > 1 )
				{
					const LevelType & lvlType = levels.begin()->type();
					BOOST_FOREACH( const Level & l, allLevels )
					{
						if ( l.type() == lvlType )
							levelEntry[l];
					}
				}
				else
					levelEntry[* levels.begin()];

				for ( LevelEntry::iterator level = levelEntry.begin(); level != levelEntry.end(); ++ level )
				{
					VersionEntry & versionEntry = level->second;
					if ( versions.size() > 1 )
					{
						BOOST_FOREACH( int v, allVersions )
							if ( versionEntry.find(v) == versionEntry.end() )
								versionEntry[v] = UNDEFINED_GID;
					}
					else if ( versionEntry.empty() )
						versionEntry[* versions.begin()] = UNDEFINED_GID;
				}
			}
		}
	}
}

WdbIndex::~WdbIndex()
{
}

WdbIndex::GidList WdbIndex::getData(const std::string & parameter, unsigned unLimDimPos) const
{
	GidList ret;

	Data::const_iterator p = entries.find(parameter);

	if ( p == entries.end() )
		throw CDMException(parameter + ": no such parameter");

	ReferenceTimeEntry::const_iterator rt = p->second.begin();
	if ( allReferenceTimes_.size() > 1 )
	{
		if ( p->second.size() <= unLimDimPos )
			throw CDMException("Invalid time index");
		std::advance(rt, unLimDimPos);
		for ( ValidTimeEntry::const_iterator v = rt->second.begin(); v != rt->second.end(); ++ v )
			for ( LevelEntry::const_iterator l = v->second.begin(); l != v->second.end(); ++ l )
				for ( VersionEntry::const_iterator ver = l->second.begin(); ver != l->second.end(); ++ ver)
					ret.push_back(ver->second);
	}
	else
	{
		if ( rt->second.size() <= unLimDimPos )
			throw CDMException("Invalid time index");


		ValidTimeEntry::const_iterator v = rt->second.begin();
		std::advance(v, unLimDimPos);
		for ( LevelEntry::const_iterator l = v->second.begin(); l != v->second.end(); ++ l )
			for ( VersionEntry::const_iterator ver = l->second.begin(); ver != l->second.end(); ++ ver)
				ret.push_back(ver->second);
	}

	return ret;
}

std::set<std::string> WdbIndex::allParameters() const
{
	std::set<std::string> ret;

	for ( Data::const_iterator it = entries.begin(); it != entries.end(); ++ it )
		ret.insert(it->first.name());

	return ret;
}

const std::string & WdbIndex::unitForParameter(const std::string & parameter) const
{
	Data::const_iterator find = entries.find(Parameter(parameter));
	if ( find == entries.end() )
		throw CDMException(parameter + ": no such parameter");

	return find->first.unit();
}

std::set<GridData::Duration> WdbIndex::allTimes() const
{
	return allValidtimes_;
}

std::set<GridData::Duration> WdbIndex::timesForParameter(const std::string & parameter) const
{
	std::set<GridData::Duration> t;

	Data::const_iterator find = entries.find(parameter);
	if ( find == entries.end() )
		throw CDMException(parameter + ": no such parameter");

	for ( ReferenceTimeEntry::const_iterator it2 = find->second.begin(); it2 != find->second.end(); ++ it2 )
		for ( ValidTimeEntry::const_iterator it3 = it2->second.begin(); it3 != it2->second.end(); ++ it3 )
			t.insert(it3->first);
	return t;
}

const LevelType & WdbIndex::levelTypeForParameter(const std::string & parameter) const
{
	Data::const_iterator p = entries.find(parameter);
	if ( p == entries.end() )
		throw CDMException(parameter + ": no such parameter");

	ReferenceTimeEntry::const_iterator v = p->second.begin();
	ValidTimeEntry::const_iterator r = v->second.begin();
	LevelEntry::const_iterator l = r->second.begin();
	const Level & lvl = l->first;
	return lvl.type();
}


std::set<float> WdbIndex::levelsForParameter(const std::string & parameter) const
{
	std::set<float> ret;

	Data::const_iterator p = entries.find(parameter);
	if ( p == entries.end() )
		throw CDMException(parameter + ": no such parameter");

	ReferenceTimeEntry::const_iterator v = p->second.begin();
	ValidTimeEntry::const_iterator r = v->second.begin();

	// All level entries are equal in structure, so we only have to find one for the given parameter

	for ( LevelEntry::const_iterator l = r->second.begin(); l != r->second.end(); ++ l )
		ret.insert(l->first.to());

	return ret;
}

std::set<int> WdbIndex::versionsForParameter(const std::string & parameter) const
{
	std::set<int> ret;

	Data::const_iterator p = entries.find(parameter);
	if ( p == entries.end() )
		throw CDMException(parameter + ": no such parameter");

	ReferenceTimeEntry::const_iterator v = p->second.begin();
	ValidTimeEntry::const_iterator r = v->second.begin();
	LevelEntry::const_iterator l = r->second.begin();

	for ( VersionEntry::const_iterator ver = l->second.begin(); ver != l->second.end(); ++ ver )
		ret.insert(ver->first);

	return ret;
}

bool WdbIndex::hasParameter(const std::string & parameter) const
{
	return entries.find(parameter) != entries.end();
}

std::set<float> WdbIndex::getLevelValues(const std::string & levelName) const
{
	std::map<std::string, std::set<float> >::const_iterator find = levels_.find(levelName);
	if ( find == levels_.end() )
		throw CDMException("Request for nonexisting level");

	return find->second;
}

std::set<GridData::Time> WdbIndex::referenceTimes() const
{
	return allReferenceTimes_;
}

std::set<GridData::Time> WdbIndex::referenceTimesForParameter(const std::string & parameter) const
{
	std::set<GridData::Time> t;

	Data::const_iterator find = entries.find(parameter);
	if ( find == entries.end() )
		throw CDMException(parameter + ": no such parameter");

	for ( ReferenceTimeEntry::const_iterator it2 = find->second.begin(); it2 != find->second.end(); ++ it2 )
		t.insert(it2->first);

	return t;
}

}
}
