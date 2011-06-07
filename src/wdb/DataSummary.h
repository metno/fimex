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

#ifndef DATASUMMARY_H_
#define DATASUMMARY_H_

#include "database_access/Parameter.h"
#include "database_access/Level.h"
#include <boost/date_time/posix_time/ptime.hpp>
#include <set>


namespace MetNoFimex
{
namespace wdb
{
class GridData;


class DataSummary
{
public:
	DataSummary();
	~DataSummary();

	void add(const GridData & gridData);

	void mergeWith(const DataSummary & other);

	const std::set<boost::posix_time::ptime> & referenceTimes() const { return referenceTimes_; }
	const std::set<boost::posix_time::time_duration> & validTimes() const { return validTimes_; }
	const std::set<float> & levelValues() const { return levelValues_; }
	const std::set<int> & versions() const { return versions_; }

	const Parameter & parameter() const { return parameter_; }
    int refereneceTimeIndex(const boost::posix_time::ptime & t) const;
    int validTimeIndex(const boost::posix_time::time_duration & d) const;
    const LevelType & level() const;
    int levelIndex(float level) const;
    int versionIndex(int index) const;


private:

    Parameter parameter_;
	std::set<boost::posix_time::ptime> referenceTimes_;
	std::set<boost::posix_time::time_duration> validTimes_;
	LevelType * level_;
	std::set<float> levelValues_;
	std::set<int> versions_;
};

}
}

#endif /* DATASUMMARY_H_ */
