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

#include "Wdb2CdmBuilder.h"
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

Wdb2CdmBuilder::Wdb2CdmBuilder(const std::vector<wdb::GridData> & data, const CdmNameTranslator & translator) :
		index_(data), translator_(translator)
{
	BOOST_FOREACH(const wdb::GridData & d, data)
		grids_[d.parameter().name()] = d.gridInformation();
}

Wdb2CdmBuilder::~Wdb2CdmBuilder()
{
}


void Wdb2CdmBuilder::populate(CDM & cdm) const
{
	// TODO: global attributes

	addProjectionInformation_(cdm);
	addReferenceTimeInformation_(cdm);
	addDimensions_(cdm);
	addParameterVariables_(cdm);

	//cdm.toXMLStream(std::cout);
}


bool Wdb2CdmBuilder::isDatabaseField(const std::string & variableName) const
{
	return index_.hasParameter(variableName);
}

void Wdb2CdmBuilder::addProjectionInformation_(CDM & cdm) const
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

	std::vector<std::string> dims = boost::assign::list_of("x")("y");
	cdm.addVariable(CDMVariable("longitude", CDM_FLOAT, dims));
	cdm.addAttribute("longitude", CDMAttribute("long_name", "longitude"));
	cdm.addAttribute("longitude", CDMAttribute("standard_name", "longitude"));
	cdm.addAttribute("longitude", CDMAttribute("units", "degree_east"));

	cdm.addVariable(CDMVariable("latitude", CDM_FLOAT, dims));
	cdm.addAttribute("latitude", CDMAttribute("long_name", "latitude"));
	cdm.addAttribute("latitude", CDMAttribute("standard_name", "latitude"));
	cdm.addAttribute("latitude", CDMAttribute("units", "degree_north"));
}

void Wdb2CdmBuilder::addReferenceTimeInformation_(CDM & cdm) const
{
	const std::string reftime = "forecast_reference_time";
	cdm.addVariable(CDMVariable(reftime, CDM_DOUBLE, std::vector<std::string>()));
	cdm.addAttribute(reftime, CDMAttribute("long_name", reftime));
	cdm.addAttribute(reftime, CDMAttribute("standard_name", reftime));
	cdm.addAttribute(reftime, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
}

void Wdb2CdmBuilder::addDimensions_(CDM & cdm) const
{
	addLevelDimensions_(cdm);
	addVersionDimension_(cdm);
	addTimeDimensions_(cdm);
}

void Wdb2CdmBuilder::addLevelDimensions_(CDM & cdm) const
{
	std::set<std::string> addedLevels;
	BOOST_FOREACH(const std::string & parameter, index_.allParameters())
	{
		const std::string & levelName = index_.levelNameForParameter(parameter);
		if ( addedLevels.find(levelName) == addedLevels.end() )
		{
			std::set<float> levels = index_.levelsForParameter(parameter);
			if ( levels.size() > 1 )
			{
				const std::string & dimension = translator_.toCdmName(levelName);

				cdm.addDimension(CDMDimension(dimension, levels.size()));

				cdm.addVariable(CDMVariable(dimension, CDM_FLOAT, std::vector<std::string>(1, dimension)));
				cdm.addAttribute(dimension, CDMAttribute("long_name", levelName));
				cdm.addAttribute(dimension, CDMAttribute("standard_name", dimension));
				cdm.addAttribute(dimension, CDMAttribute("units", index_.unitForLevel(levelName)));
				cdm.addAttribute(dimension, CDMAttribute("axis", "z"));

				addedLevels.insert(levelName);
			}
		}
	}
}

void Wdb2CdmBuilder::addVersionDimension_(CDM & cdm) const
{
	BOOST_FOREACH(const std::string & parameter, index_.allParameters())
	{
		const std::set<int> & versions = index_.versionsForParameter(parameter);
		if ( versions.size() > 1 )
		{
			std::string dimension = "version";
			cdm.addDimension(CDMDimension(dimension, versions.size()));

			cdm.addVariable(CDMVariable(dimension, CDM_INT, std::vector<std::string>(1, dimension)));
			cdm.addAttribute(dimension, CDMAttribute("long_name", "data version"));
			cdm.addAttribute(dimension, CDMAttribute("standard_name", "version"));

			return;
		}
	}
}

void Wdb2CdmBuilder::addTimeDimensions_(CDM & cdm) const
{
	std::string dimension = "time";

	cdm.addVariable(CDMVariable(dimension, CDM_DOUBLE, std::vector<std::string>(1, dimension)));
	cdm.addAttribute(dimension, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
	cdm.addAttribute(dimension, CDMAttribute("long_name", dimension));
	cdm.addAttribute(dimension, CDMAttribute("standard_name", dimension));
	cdm.addAttribute(dimension, CDMAttribute("axis", "T"));

	BOOST_FOREACH(const std::string & parameter, index_.allParameters())
	{
		const std::set<GridData::Time> & times = index_.timesForParameter(parameter);
		if ( times.size() > 1 )
		{
			CDMDimension time(dimension, times.size());
			time.setUnlimited(true);
			cdm.addDimension(time);
			return;
		}
	}

	CDMDimension time(dimension, 1);
	time.setUnlimited(true);
	cdm.addDimension(time);
}

void Wdb2CdmBuilder::addParameterVariables_(CDM & cdm) const
{
	BOOST_FOREACH( const std::string & parameter, index_.allParameters() )
	{
		std::string dimension = translator_.toCdmName(parameter);

		std::vector<std::string> dimensions;
		dimensions.push_back("x");
		dimensions.push_back("y");
		if ( index_.versionsForParameter(parameter).size() > 1 )
			dimensions.push_back("version");
		if ( index_.levelsForParameter(parameter).size() > 1 )
			dimensions.push_back(translator_.toCdmName(index_.levelNameForParameter(parameter)));
		if ( index_.timesForParameter(parameter).size() > 1 )
			dimensions.push_back("time");

		cdm.addVariable(CDMVariable(dimension, CDM_FLOAT, dimensions));

		GridSpecMap::const_iterator find = grids_.find(parameter);
		if ( find == grids_.end() )
			throw CDMException("Internal error - unable to find grid mapping"); // should never happen
		cdm.addAttribute(dimension, CDMAttribute("grid_mapping", find->second->getProjectionName()));

		cdm.addAttribute(dimension, CDMAttribute("units", index_.unitForParameter(parameter)));
		cdm.addAttribute(dimension, CDMAttribute("_FillValue", std::numeric_limits<float>::quiet_NaN()));
		cdm.addAttribute(dimension, CDMAttribute("coordinates", "longitude latitude"));
	}
}


}
}
