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
#include <numeric>
#include <iterator>

namespace MetNoFimex
{
namespace wdb
{

const WdbIndex::gid WdbIndex::UNDEFINED_GID = std::numeric_limits<gid>::max();


void WdbIndex::init_(const std::vector<GridData> & gridData)
{
	std::map<Parameter, DataSummary> dataSummary;

	// index all elements
	BOOST_FOREACH(const GridData & d, gridData)
		dataSummary[d.parameter()].add(d);

	// extend indexes, so that parameters with (eg.) several versions  gets all nonunique versions
	for ( std::map<Parameter, DataSummary>::iterator a = dataSummary.begin(); a != dataSummary.end(); ++ a )
		for ( std::map<Parameter, DataSummary>::const_iterator b = dataSummary.begin(); b != dataSummary.end(); ++ b )
			a->second.mergeWith(b->second);

	for ( std::map<Parameter, DataSummary>::const_iterator it = dataSummary.begin(); it != dataSummary.end(); ++ it )
		data_.insert(std::make_pair(it->first, ParameterData(it->second, gridData)));

	for ( std::map<Parameter, DataSummary>::const_iterator it = dataSummary.begin(); it != dataSummary.end(); ++ it )
	{
		const std::set<boost::posix_time::time_duration> & validTimes = it->second.validTimes();
		if ( validTimes.size() > 1 )
			allValidtimes_.insert(validTimes.begin(), validTimes.end());
	}
}

WdbIndex::WdbIndex(const std::vector<GridData> & data)
{
	init_(data);

	BOOST_FOREACH(const GridData & d, data)
	{
//		allValidtimes_.insert(d.validTo() - d.referenceTime());
		allReferenceTimes_.insert(d.referenceTime());
		levels_[d.level().type().name()].insert(d.level().to());
	}
}

WdbIndex::~WdbIndex()
{
}


namespace
{
template<int N>
void getSubGids(WdbIndex::GidList & out, const boost::multi_array<GridData::gid, N> & a)
{
	typedef boost::multi_array<GridData::gid, N-1> NextArray;
	BOOST_FOREACH(const NextArray & sub, a)
		getSubGids<N-1>(out, sub);
}
template<>
void getSubGids<1>(WdbIndex::GidList & out, const boost::multi_array<GridData::gid, 1> & a)
{
	std::copy(a.begin(), a.end(), std::back_inserter(out));
}
template<int N>
void getGids(WdbIndex::GidList & out, const boost::multi_array<GridData::gid, N> & a, unsigned idx)
{
	getSubGids<N-1>(out, a[idx]);
}

}


WdbIndex::GidList WdbIndex::getData(const std::string & parameter, unsigned unLimDimPos) const
{
	const ParameterData::DataArray & gids = parameterData_(parameter).data();
	const ParameterData::DataArray::size_type * shape = gids.shape();

	int unlimitedDimension = 0;
	while ( unlimitedDimension < 3 and shape[unlimitedDimension] == 1 )
		++ unlimitedDimension;
	if ( shape[unlimitedDimension] <= unLimDimPos )
		throw CDMException("invalid index");

	GidList ret;
	switch ( unlimitedDimension )
	{
//	case 3:
//		ret.push_back(gids[0][0][0][unLimDimPos]);
//		break;
//	case 2:
//		getGids<2>(ret, gids[0][0], unLimDimPos);
//		break;
	case 3:
	case 2:
	{
		if ( unLimDimPos != 0 )
			throw CDMException("Variable has no unlimited dimension");
		// Skip unlimited dimensions for these
		const gid * array = gids.data();
		std::copy(array, array + gids.num_elements(), std::back_inserter(ret));
		break;
	}
	case 1:
		getGids<3>(ret, gids[0], unLimDimPos);
		break;
	case 0:
		getGids<4>(ret, gids, unLimDimPos);
		break;
	default:
		std::ostringstream msg;
		msg << "internal error: Invalid unlimited dimension selected: " << unlimitedDimension;
		throw CDMException(msg.str());
	}
	return ret;
}

WdbIndex::GidList WdbIndex::getData(const std::string & parameter, const std::vector<size_t> & startPositions, const std::vector<size_t> & sizes) const
{
	if ( startPositions.size() != 6 or sizes.size() != 6 )
		throw CDMException("Invalid index size");

	WdbIndex::GidList ret;

	const ParameterData::DataArray & data = parameterData_(parameter).data();

	for ( std::size_t r = startPositions[0]; r < startPositions[0] + sizes[0]; ++ r )
		for ( std::size_t t = startPositions[1]; t < startPositions[1] + sizes[1]; ++ t )
			for ( std::size_t l = startPositions[2]; l < startPositions[2] + sizes[2]; ++ l )
				for ( std::size_t v = startPositions[3]; v < startPositions[3] + sizes[3]; ++ v )
					ret.push_back(data[r][t][l][v]);

	return ret;
}

std::set<std::string> WdbIndex::allParameters() const
{
	std::set<std::string> ret;

	for ( Data::const_iterator it = data_.begin(); it != data_.end(); ++ it )
		ret.insert(it->first.name());

	return ret;
}

const std::string & WdbIndex::unitForParameter(const std::string & parameter) const
{
	Data::const_iterator find = data_.find(Parameter(parameter));
	if ( find == data_.end() )
		throw CDMException(parameter + ": no such parameter");

	return find->first.unit();
}

std::set<GridData::Duration> WdbIndex::allTimes() const
{
	return allValidtimes_;
}

const std::vector<GridData::Duration> & WdbIndex::timesForParameter(const std::string & parameter) const
{
	return parameterData_(parameter).validTimes();
}

const LevelType & WdbIndex::levelTypeForParameter(const std::string & parameter) const
{
	return parameterData_(parameter).levelType();
}


const std::vector<float> & WdbIndex::levelsForParameter(const std::string & parameter) const
{
	return parameterData_(parameter).levels();
}

const std::vector<int> & WdbIndex::versionsForParameter(const std::string & parameter) const
{
	return parameterData_(parameter).versions();
}

bool WdbIndex::hasParameter(const std::string & parameter) const
{
	return data_.find(parameter) != data_.end();
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

const std::vector<GridData::Time> & WdbIndex::referenceTimesForParameter(const std::string & parameter) const
{
	return parameterData_(parameter).referenceTimes();
}

bool WdbIndex::hasManyReferenceTimes(const std::string & parameter) const
{
	return parameterData_(parameter).referenceTimes().size() > 1;
}

bool WdbIndex::hasManyValidTimeOffsets(const std::string & parameter) const
{
	return parameterData_(parameter).validTimes().size() > 1;
}

bool WdbIndex::hasManyLevels(const std::string & parameter) const
{
	return parameterData_(parameter).levels().size() > 1;
}

bool WdbIndex::hasManyVersions(const std::string & parameter) const
{
	return parameterData_(parameter).versions().size() > 1;
}

const ParameterData & WdbIndex::parameterData_(const Parameter & p) const
{
	Data::const_iterator find = data_.find(p);
	if ( find == data_.end() )
		throw CDMException(p.name() + ": no such parameter");
	return find->second;
}

}
}
