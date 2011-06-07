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

#ifndef PARAMETERDATA_H_
#define PARAMETERDATA_H_

#include "database_access/GridData.h"
#include <boost/multi_array.hpp>


namespace MetNoFimex
{
namespace wdb
{
class DataSummary;


class ParameterData
{
public:
	ParameterData(const DataSummary & s, const std::vector<GridData> & gridData);
	~ParameterData();

	enum DataIndices
	{
		ReferenceTimeIndex, ValidTimeIndex, LevelIndex, VersionIndex
	};
	typedef boost::multi_array<GridData::gid, 4> DataArray;

	const DataArray & data() const { return data_; }
	const std::vector<GridData::Time> & referenceTimes() const { return referenceTimes_; }
	const std::vector<GridData::Duration> & validTimes() const { return validTimes_; }
	const LevelType & levelType() const { return level_; }
	const std::vector<float> & levels() const { return levels_; }
	const std::vector<int> & versions() const { return versions_; }


private:
	DataArray data_;
	std::vector<GridData::Time> referenceTimes_;
	std::vector<GridData::Duration> validTimes_;
	LevelType level_;
	std::vector<float> levels_;
	std::vector<int> versions_;
};

}

}

#endif /* PARAMETERDATA_H_ */
