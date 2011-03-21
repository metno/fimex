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
#include "CdmNameTranslator.h"
#include "fimex/CDM.h"
#include "fimex/CDMDimension.h"
#include <set>

namespace MetNoFimex
{
namespace wdb
{

DataIndex::DataIndex(const std::vector<wdb::GridData> & data, const CdmNameTranslator & translator) :
		translator_(translator)
{
	for ( std::vector<wdb::GridData>::const_iterator d = data.begin(); d != data.end(); ++ d )
		data_[d->parameter()] [d->level()] [d->version()] [d->validTo()] = d->gridIdentifier();
}

DataIndex::~DataIndex()
{
}

std::ostream & DataIndex::summary(std::ostream & s) const
{
	for ( ParameterEntry::const_iterator pe = data_.begin(); pe != data_.end(); ++ pe )
	{
		s << pe->first.name() << " (" << pe->first.unit() << ")" << '\n';
		for ( LevelEntry::const_iterator le = pe->second.begin(); le != pe->second.end(); ++ le )
		{
			s << " " << le->first << '\n';
			for ( VersionEntry::const_iterator ve = le->second.begin(); ve != le->second.end(); ++ ve )
			{
				s << "  " << ve->first << '\n';
				for ( TimeEntry::const_iterator te = ve->second.begin(); te != ve->second.end(); ++ te )
					s << "   " << te->first << ":\t" << std::showbase << std::hex << te->second << '\n';
			}
		}
	}
	std::flush(s);

	return s;
}


void DataIndex::populate(CDM & cdm) const
{
	addDimensions_(cdm);
	addParameters_(cdm);

	//cdm.toXMLStream(std::cout);
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
	addTimes_(cdm);

	typedef std::map<LevelType, std::set<std::pair<float, float> > > LevelMap;
	LevelMap dimensions;

	std::size_t maxVersionCount = 0;
	for ( ParameterEntry::const_iterator pe = data_.begin(); pe != data_.end(); ++ pe )
	{
		for ( LevelEntry::const_iterator le = pe->second.begin(); le != pe->second.end(); ++ le )
		{
			const Level & lvl = le->first;
			dimensions[lvl.type()].insert(std::make_pair(lvl.from(), lvl.to()));

			maxVersionCount = std::max(maxVersionCount, le->second.size());
		}
	}
	for ( LevelMap::const_iterator it = dimensions.begin(); it != dimensions.end(); ++ it )
		if ( it->second.size() > 1 )
		{
			const LevelType & lvl = it->first;
			lvl.addToCdm(cdm, it->second.size(), translator_);

		}

	if ( maxVersionCount > 1 )
	{
		std::string dimesion = "version";
		cdm.addDimension(CDMDimension(dimesion, maxVersionCount));

		cdm.addVariable(getSelfReferencingVariable(dimesion, CDM_INT));
		cdm.addAttribute(dimesion, CDMAttribute("long_name", "data version"));
		cdm.addAttribute(dimesion, CDMAttribute("standard_name", "version"));
	}

	cdm.addDimension(CDMDimension("latitude", 100));
	cdm.addDimension(CDMDimension("longitude", 100));
}

void DataIndex::addTimes_(CDM & cdm) const
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

	CDMDimension time("time", times.size()); // get correct number
	time.setUnlimited(true);
	cdm.addDimension(time);

	cdm.addVariable(getSelfReferencingVariable("time", CDM_DOUBLE));
	cdm.addAttribute("time", CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
	cdm.addAttribute("time", CDMAttribute("long_name", "time"));
	cdm.addAttribute("time", CDMAttribute("standard_name", "time"));
}


void DataIndex::addParameters_(CDM & cdm) const
{
	for ( ParameterEntry::const_iterator it = data_.begin(); it != data_.end(); ++ it )
	{
		const Parameter & parameter = it->first;

		std::vector<std::string> dimensions;
		getDimensions_(dimensions, it->second);

		const std::string cdmName = translator_.toCdmName(parameter.name());

		cdm.addVariable(CDMVariable(cdmName, CDM_FLOAT, dimensions));
		cdm.addAttribute(cdmName, CDMAttribute("grid_mapping", "unknown"));
		cdm.addAttribute(cdmName, CDMAttribute("units", parameter.unit()));
		cdm.addAttribute(cdmName, CDMAttribute("_FillValue", std::numeric_limits<float>::quiet_NaN()));
		cdm.addAttribute(cdmName, CDMAttribute("coordinates", "longitude latitude"));
	}
}

void DataIndex::getDimensions_(std::vector<std::string> & out, const LevelEntry & levelEntry) const
{
	for ( LevelEntry::const_iterator le = levelEntry.begin(); le != levelEntry.end(); ++ le )
		for ( VersionEntry::const_iterator ve = le->second.begin(); ve != le->second.end(); ++ ve )
			if ( ve->second.size() > 1 )
				out.push_back("time");

	getLevelDimensions_(out, levelEntry);

	for ( LevelEntry::const_iterator it = levelEntry.begin(); it != levelEntry.end(); ++ it )
		getVersionDimensions_(out, it->second);

	out.push_back("longitude");
	out.push_back("latitude");
}

void DataIndex::getLevelDimensions_(std::vector<std::string> & out, const LevelEntry & levelEntry) const
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

void DataIndex::getVersionDimensions_(std::vector<std::string> & out, const VersionEntry & versionEntry) const
{
	if ( versionEntry.size() > 1 )
		out.push_back("version");
}

}
}
