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
#include "config/GlobalWdbConfiguration.h"
#include "gridInformation/GridInformation.h"
#include <fimex/CDM.h>
#include <fimex/CDMDimension.h>
#include <fimex/coordSys/Projection.h>
#include <set>
#include <boost/foreach.hpp>
#include <boost/assign/list_of.hpp>

namespace MetNoFimex
{
namespace wdb
{

Wdb2CdmBuilder::Wdb2CdmBuilder(const std::vector<wdb::GridData> & data, const GlobalWdbConfiguration & config) :
		index_(data), config_(config)
{
	BOOST_FOREACH(const wdb::GridData & d, data)
		grids_[d.parameter().name()] = d.gridInformation();
}

Wdb2CdmBuilder::~Wdb2CdmBuilder()
{
}

void Wdb2CdmBuilder::populate(CDM & cdm) const
{
	addProjectionInformation_(cdm);
	addReferenceTimeInformation_(cdm);
	addDimensions_(cdm);
	addParameterVariables_(cdm);
}


bool Wdb2CdmBuilder::isDatabaseField(const std::string & variableName) const
{
	return index_.hasParameter(variableName);
}

const std::set<float> * Wdb2CdmBuilder::getLevelValues(const std::string & levelName) const
{
	return index_.getLevelValues(levelName);
}

const GridInformation & Wdb2CdmBuilder::gridInformation() const
{
	// Several grid types in same wdb is not supported (yet)
	return * grids_.begin()->second;
}

const GridData::Time & Wdb2CdmBuilder::referenceTime() const
{
	return index_.referenceTime();
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

	if ( grids.empty() )
		throw CDMException("No grids");
	if ( grids.size() > 1 )
		throw CDMException("Several grid types in same wdb reader is not supported (yet)");

	GridData::GridInformationPtr gridInfo = * grids.begin();
	gridInfo->addToCdm(cdm);
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
				const std::string & dimension = config_.cfName(levelName);

				cdm.addDimension(CDMDimension(dimension, levels.size()));

				cdm.addVariable(CDMVariable(dimension, CDM_FLOAT, std::vector<std::string>(1, dimension)));

				BOOST_FOREACH( const CDMAttribute & attribute, config_.getAttributes(levelName, index_.unitForLevel(levelName)) )
					cdm.addAttribute(dimension, attribute);
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
			cdm.addAttribute(dimension, CDMAttribute("axis", "Ensemble"));

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

namespace
{
	template <typename T>
	void setAttribute(GlobalWdbConfiguration::AttributeList & out, const std::string & name, T value)
	{
		for ( GlobalWdbConfiguration::AttributeList::const_iterator find = out.begin(); find != out.end(); ++ find )
			if ( find->getName() == name )
				return;
		out.push_back(CDMAttribute(name, value));
	}
}


void Wdb2CdmBuilder::addParameterVariables_(CDM & cdm) const
{
	BOOST_FOREACH( const std::string & parameter, index_.allParameters() )
	{
		GridSpecMap::const_iterator find = grids_.find(parameter);
		if ( find == grids_.end() )
			throw CDMException("Internal error - unable to find grid mapping"); // should never happen
		GridData::GridInformationPtr gridInfo = find->second;

		std::string dimension = config_.cfName(parameter);

		std::vector<std::string> spatialDimensions;
		gridInformation().addSpatialDimensions(spatialDimensions);


		std::vector<std::string> dimensions = spatialDimensions;
		if ( index_.versionsForParameter(parameter).size() > 1 )
			dimensions.push_back("version");
		if ( index_.levelsForParameter(parameter).size() > 1 )
			dimensions.push_back(config_.cfName(index_.levelNameForParameter(parameter)));
		if ( index_.timesForParameter(parameter).size() > 1 )
			dimensions.push_back("time");

		cdm.addVariable(CDMVariable(dimension, CDM_FLOAT, dimensions));

		GlobalWdbConfiguration::AttributeList attributes = config_.getAttributes(parameter, index_.unitForParameter(parameter));
		setAttribute(attributes, "_FillValue", std::numeric_limits<float>::quiet_NaN());

//		grids_[d.parameter().name()]

	   std::string coordinates = gridInfo->getCoordinatesAttribute();
	   if ( not coordinates.empty() )
		   setAttribute(attributes, "coordinates", coordinates);
	   setAttribute(attributes, "grid_mapping", gridInfo->getProjectionName());


		BOOST_FOREACH( const CDMAttribute & attribute, attributes )
			cdm.addAttribute(dimension, attribute);
	}
}


}
}
