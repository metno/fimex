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

#include "DataIndex.h"
#include "GridInformation.h"
#include "CdmNameTranslator.h"
#include "fimex/CDM.h"
#include "fimex/CDMDimension.h"
#include "fimex/coordSys/Projection.h"
#include <set>
#include <boost/foreach.hpp>
#include <boost/assign/list_of.hpp>

namespace MetNoFimex
{
namespace wdb
{

DataIndex::DataIndex(const std::vector<wdb::GridData> & data, const CdmNameTranslator & translator) :
		translator_(translator)
{
	for ( std::vector<wdb::GridData>::const_iterator d = data.begin(); d != data.end(); ++ d )
	{
		data_[d->parameter()] [d->level()] [d->version()] [d->validTo()] = d->gridIdentifier();
		grids_[d->parameter()] = d->gridInformation();
	}
}

DataIndex::~DataIndex()
{
}

// projection after z-axis
// x(x) and y(y)
// longitude(x, y)
// latitude(x, y)
// forecast_reference_time

// global attributes

void DataIndex::populate(CDM & cdm) const
{
	addProjectionInformation_(cdm);
	addDimensions_(cdm);
	addParameterVariables_(cdm);

	cdm.toXMLStream(std::cout);
}

namespace
{
CDMVariable getSelfReferencingVariable(const std::string & name, CDMDataType dataType = CDM_FLOAT)
{
	std::vector<std::string> dims;
	dims.push_back(name);
	return CDMVariable(name, dataType, dims);
}
}

void DataIndex::addDimensions_(CDM & cdm) const
{
	addLevelDimensions_(cdm);
	addVersionDimension_(cdm);
	addTimeDimensions_(cdm);
}

void DataIndex::addLevelDimensions_(CDM & cdm) const
{
	typedef std::map<LevelType, std::set<std::pair<float, float> > > LevelMap;
	LevelMap dimensions;

	for ( ParameterEntry::const_iterator pe = data_.begin(); pe != data_.end(); ++ pe )
	{
		for ( LevelEntry::const_iterator le = pe->second.begin(); le != pe->second.end(); ++ le )
		{
			const Level & lvl = le->first;
			dimensions[lvl.type()].insert(std::make_pair(lvl.from(), lvl.to()));
		}
	}
	for ( LevelMap::const_iterator it = dimensions.begin(); it != dimensions.end(); ++ it )
	{
		if ( it->second.size() > 1 )
		{
			const LevelType & lvl = it->first;
			lvl.addToCdm(cdm, it->second.size(), translator_);
		}
	}
}

void DataIndex::addVersionDimension_(CDM & cdm) const
{
	std::size_t maxVersionCount = 0;
	for ( ParameterEntry::const_iterator pe = data_.begin(); pe != data_.end(); ++ pe )
		for ( LevelEntry::const_iterator le = pe->second.begin(); le != pe->second.end(); ++ le )
			maxVersionCount = std::max(maxVersionCount, le->second.size());

	if ( maxVersionCount > 1 )
	{
		std::string dimesion = "version";
		cdm.addDimension(CDMDimension(dimesion, maxVersionCount));

		cdm.addVariable(getSelfReferencingVariable(dimesion, CDM_INT));
		cdm.addAttribute(dimesion, CDMAttribute("long_name", "data version"));
		cdm.addAttribute(dimesion, CDMAttribute("standard_name", "version"));
	}
}

void DataIndex::addTimeDimensions_(CDM & cdm) const
{
	std::set<Time> times;

	for ( ParameterEntry::const_iterator pe = data_.begin(); pe != data_.end(); ++ pe )
	{
		std::set<Time> timesForParameter;
		for ( LevelEntry::const_iterator le = pe->second.begin(); le != pe->second.end(); ++ le )
		{
			for ( VersionEntry::const_iterator ve = le->second.begin(); ve != le->second.end(); ++ ve )
			{
				for ( TimeEntry::const_iterator te = ve->second.begin(); te != ve->second.end(); ++ te )
					timesForParameter.insert(te->first);
			}
		}
		if ( timesForParameter.size() > 1 )
			times.insert(timesForParameter.begin(), timesForParameter.end());
	}

	CDMDimension time("time", times.size());
	time.setUnlimited(true);
	cdm.addDimension(time);

	cdm.addVariable(getSelfReferencingVariable("time", CDM_DOUBLE));
	cdm.addAttribute("time", CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
	cdm.addAttribute("time", CDMAttribute("long_name", "time"));
	cdm.addAttribute("time", CDMAttribute("standard_name", "time"));
}


void DataIndex::addProjectionInformation_(CDM & cdm) const
{
	std::set<GridData::GridInformationPtr> grids;
	for ( GridSpecMap::const_iterator it = grids_.begin(); it != grids_.end(); ++ it )
		grids.insert(it->second);
	BOOST_FOREACH(GridData::GridInformationPtr grid, grids)
	{
		const boost::shared_ptr<Projection> projection = grid->getProjection();
		std::string projectionName = grid->getProjectionName();
		CDMVariable projectionSpec(projectionName, CDM_FLOAT, std::vector<std::string>());
		cdm.addVariable(projectionSpec);
		BOOST_FOREACH(const CDMAttribute & a, projection->getParameters())
			cdm.addAttribute(projectionName, a);
	}

	if ( grids.size() > 1 )
		throw CDMException("Several grid types in same wdb is not supported (yet)");

	cdm.addDimension(CDMDimension("x", (*grids.begin())->numberX()));
	cdm.addDimension(CDMDimension("y", (*grids.begin())->numberY()));

	cdm.addVariable(CDMVariable("x", CDM_FLOAT, std::vector<std::string>(1, "x")));
	cdm.addAttribute("x", CDMAttribute("long_name", "x-coordinate in Cartesian system"));
	cdm.addAttribute("x", CDMAttribute("standard_name", "grid longitude"));
	cdm.addAttribute("x", CDMAttribute("units", "degree_east"));

	cdm.addVariable(CDMVariable("y", CDM_FLOAT, std::vector<std::string>(1, "y")));
	cdm.addAttribute("y", CDMAttribute("long_name", "y-coordinate in Cartesian system"));
	cdm.addAttribute("y", CDMAttribute("standard_name", "grid latitude"));
	cdm.addAttribute("y", CDMAttribute("units", "degree_north"));

	std::vector<std::string> dims = boost::assign::list_of("longitude")("latitude");
	cdm.addVariable(CDMVariable("longitude", CDM_FLOAT, dims));
	cdm.addAttribute("longitude", CDMAttribute("long_name", "longitude"));
	cdm.addAttribute("longitude", CDMAttribute("standard_name", "longitude"));
	cdm.addAttribute("longitude", CDMAttribute("units", "degree_east"));

	cdm.addVariable(CDMVariable("latitude", CDM_FLOAT, dims));
	cdm.addAttribute("latitude", CDMAttribute("long_name", "latitude"));
	cdm.addAttribute("latitude", CDMAttribute("standard_name", "latitude"));
	cdm.addAttribute("latitude", CDMAttribute("units", "degree_north"));
}


void DataIndex::addParameterVariables_(CDM & cdm) const
{
	for ( ParameterEntry::const_iterator it = data_.begin(); it != data_.end(); ++ it )
	{
		const Parameter & parameter = it->first;

		std::vector<std::string> dimensions;
		getDimensionsForParameter_(dimensions, it->second);

		const std::string cdmName = translator_.toCdmName(parameter.name());

		cdm.addVariable(CDMVariable(cdmName, CDM_FLOAT, dimensions));

		GridSpecMap::const_iterator find = grids_.find(parameter);
		if ( find == grids_.end() )
			throw CDMException("Internal error - unable to find grid mapping"); // should never happen
		cdm.addAttribute(cdmName, CDMAttribute("grid_mapping", find->second->getProjectionName()));
		cdm.addAttribute(cdmName, CDMAttribute("units", parameter.unit()));
		cdm.addAttribute(cdmName, CDMAttribute("_FillValue", std::numeric_limits<float>::quiet_NaN()));
		cdm.addAttribute(cdmName, CDMAttribute("coordinates", "longitude latitude"));
	}
}

void DataIndex::getDimensionsForParameter_(std::vector<std::string> & out, const LevelEntry & levelEntry) const
{
	getTimeDimensionForParameter_(out, levelEntry);
	getLevelDimensionsForParameter_(out, levelEntry);
	getVersionDimensionsForParameter_(out, levelEntry);

	// x/y dimensions
	out.push_back("longitude");
	out.push_back("latitude");
}

void DataIndex::getTimeDimensionForParameter_(std::vector<std::string> & out, const LevelEntry & levelEntry) const
{
	for ( LevelEntry::const_iterator le = levelEntry.begin(); le != levelEntry.end(); ++ le )
		for ( VersionEntry::const_iterator ve = le->second.begin(); ve != le->second.end(); ++ ve )
			if ( ve->second.size() > 1 )
			{
				out.push_back("time");
				break;
			}
}

void DataIndex::getLevelDimensionsForParameter_(std::vector<std::string> & out, const LevelEntry & levelEntry) const
{
	typedef std::map<LevelType, std::set<std::pair<float, float> > > LevelEntries;
	LevelEntries levels;
	for ( LevelEntry::const_iterator it = levelEntry.begin(); it != levelEntry.end(); ++ it )
	{
		const Level & level = it->first;
		levels[level.type()].insert(std::make_pair(level.from(), level.to()));
	}

	for ( LevelEntries::const_iterator it = levels.begin(); it != levels.end(); ++ it )
	{
		if ( it->second.size() > 1 )
		{
			out.push_back(translator_.toCdmName(it->first.name()));
			break; // We only support a single type of level atm
		}
	}
}

void DataIndex::getVersionDimensionsForParameter_(std::vector<std::string> & out, const LevelEntry & levelEntry) const
{
	for ( LevelEntry::const_iterator it = levelEntry.begin(); it != levelEntry.end(); ++ it )
		if ( it->second.size() > 1 )
			out.push_back("version");
}

}
}
