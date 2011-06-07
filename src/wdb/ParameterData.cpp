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
#include "DataSummary.h"
#include <boost/foreach.hpp>

namespace MetNoFimex
{

namespace wdb
{

namespace
{
template<typename T>
std::vector<typename T::value_type> convert(const T & t)
{
	return std::vector<typename T::value_type>(t.begin(), t.end());
}
}

ParameterData::ParameterData(const DataSummary & s, const std::vector<GridData> & gridData) :
		referenceTimes_(convert(s.referenceTimes())),
		validTimes_(convert(s.validTimes())),
		level_(s.level()),
		levels_(convert(s.levelValues())),
		versions_(convert(s.versions()))
{
	DataArray::index refTimeSize = referenceTimes_.size();
	size_t validTimeSize = validTimes_.size();
	size_t levelSize = levels_.size();
	size_t versionSize = versions_.size();

	data_.resize(boost::extents[refTimeSize][validTimeSize][levelSize][versionSize]);

	GridData::gid * start = data_.data();
	GridData::gid * stop = start + data_.num_elements();
	std::fill(start, stop, WdbIndex::UNDEFINED_GID);

	BOOST_FOREACH(const GridData & d, gridData)
	{
		if ( s.parameter() == d.parameter() )
		{
			GridData::gid & entry =
					data_[s.refereneceTimeIndex(d.referenceTime())]
					      [s.validTimeIndex(d.validTo() - d.referenceTime())]
					       [s.levelIndex(d.level().to())]
					        [s.versionIndex(d.version())];

			if ( entry != WdbIndex::UNDEFINED_GID )
				throw CDMException("duplicate data in souce");

			entry = d.gridIdentifier();
			level_ = s.level();
		}
	}
}

ParameterData::~ParameterData()
{
}

}

}
