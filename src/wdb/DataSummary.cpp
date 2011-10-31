/*
 wdbindex

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

#include "DataSummary.h"
#include "database_access/GridData.h"
#include <boost/foreach.hpp>
#include <stdexcept>

namespace MetNoFimex
{
namespace wdb
{


DataSummary::DataSummary() :
	level_(0)
{
}

DataSummary::~DataSummary()
{
	delete level_;
}


void DataSummary::add(const GridData & gridData)
{
	parameter_ = gridData.parameter();

	referenceTimes_.insert(gridData.referenceTime());
	validTimes_.insert(gridData.validTo() - gridData.referenceTime());
	if ( level_ )
	{
		if ( (* level_) != gridData.level().type() )
		{
			std::ostringstream msg;
			msg << "Multiple level types in same parameter: " << gridData.parameter().name();
			msg << "\t" << (* level_) << " and " << gridData.level().type();
			throw CDMException(msg.str());
		}
	}
	else
		level_ = new LevelType(gridData.level().type());
	levelValues_.insert(gridData.level().to());
	versions_.insert(gridData.version());
}

void DataSummary::mergeWith(const DataSummary & other)
{
	if ( referenceTimes_.size() > 1 )
	{
		const std::set<boost::posix_time::ptime> & toMerge = other.referenceTimes();
		referenceTimes_.insert(toMerge.begin(), toMerge.end());
	}

	if ( validTimes_.size() > 1 and other.validTimes().size() > 1 )
	{
		const std::set<boost::posix_time::time_duration> & toMerge = other.validTimes();
		validTimes_.insert(toMerge.begin(), toMerge.end());
	}

	if ( level_ == other.level_ and levelValues_.size() > 1 )
	{
		const std::set<float> & lvls = other.levelValues();
		levelValues_.insert(lvls.begin(), lvls.end());
	}

	if ( versions_.size() > 1 )
	{
		const std::set<int> & vers = other.versions();
		versions_.insert(vers.begin(), vers.end());
	}
}

namespace
{
template <typename T>
int indexOf(T element, const std::set<T> & container)
{
	int ret = 0;
	BOOST_FOREACH(const T & t, container)
		if ( element == t )
			return ret;
		else
			++ ret;

	throw CDMException("Internal error: index lookup"); // should never happen
}
}

int DataSummary::refereneceTimeIndex(const boost::posix_time::ptime & t) const
{
	return indexOf(t, referenceTimes_);
}

int DataSummary::validTimeIndex(const boost::posix_time::time_duration & d) const
{
	return indexOf(d, validTimes_);
}

const LevelType & DataSummary::level() const
{
	if ( ! level_ )
		throw CDMException("Level is not known about parameter");
	return * level_;
}

int DataSummary::levelIndex(float level) const
{
	return indexOf(level, levelValues_);
}

int DataSummary::versionIndex(int index) const
{
	return indexOf(index, versions_);
}

}
}
