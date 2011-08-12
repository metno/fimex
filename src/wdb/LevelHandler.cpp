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

#include "LevelHandler.h"
#include "WdbIndex.h"
#include "config/GlobalWdbConfiguration.h"
#include <fimex/CDM.h>
#include <fimex/Data.h>
#include <boost/foreach.hpp>
#include <set>
#include <string>

namespace MetNoFimex
{

namespace wdb
{

LevelHandler::LevelHandler(const WdbIndex & index, const GlobalWdbConfiguration & config) :
		index_(index), config_(config)
{
}

LevelHandler::~LevelHandler()
{
}

void LevelHandler::addToCdm(CDM & cdm) const
{
	std::set<std::string> addedLevels;
	BOOST_FOREACH(const std::string & parameter, index_.allParameters())
	{
		const LevelType & levelType = index_.levelTypeForParameter(parameter);
		std::string wdbLevelName = levelType.name();
		if ( addedLevels.find(wdbLevelName) == addedLevels.end() )
		{
			const std::vector<float> & levels = index_.levelsForParameter(parameter);
			if ( levels.size() > 1 )
			{
				const std::string & cfDimension = config_.cfName(wdbLevelName);

				levels_[cfDimension] = wdbLevelName;

				cdm.addDimension(CDMDimension(cfDimension, levels.size()));

				cdm.addVariable(CDMVariable(cfDimension, CDM_FLOAT, std::vector<std::string>(1, cfDimension)));

				BOOST_FOREACH( const CDMAttribute & attribute, config_.getAttributes(wdbLevelName, levelType.unit()) )
					cdm.addAttribute(cfDimension, attribute);
				cdm.addAttribute(cfDimension, CDMAttribute("axis", "Z"));

				addedLevels.insert(wdbLevelName);
			}
		}
	}
}

boost::shared_ptr<Data> LevelHandler::getData(const CDMVariable & variable, size_t unLimDimPos) const
{
	const std::set<float> & levels =  index_.getLevelValues(wdbFromCf_(variable.getName()));

	boost::shared_ptr<Data> ret = createData(CDM_FLOAT, levels.size());

	float * dataIdx = reinterpret_cast<float *>(ret->getDataPtr());
	std::copy(levels.begin(), levels.end(), dataIdx);

	return ret;
}

bool LevelHandler::canHandle(const std::string & cfName) const
{
	std::string wdbName = wdbFromCf_(cfName);
	return index_.hasLevel(wdbName);
}

std::string LevelHandler::wdbFromCf_(const std::string & cfName) const
{
	std::map<std::string, std::string>::const_iterator find = levels_.find(cfName);
	if ( find == levels_.end() )
		return std::string();
	return find->second;
}

}
}
