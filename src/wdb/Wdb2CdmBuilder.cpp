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
#include "TimeHandler.h"
#include "LevelHandler.h"
#include "VersionHandler.h"
#include "GridHandler.h"
#include "config/GlobalWdbConfiguration.h"
#include "gridInformation/GridInformation.h"
#include <fimex/CDM.h>
#include <fimex/SliceBuilder.h>
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

	dataHandlers_.push_back(DataHandler::Ptr(new TimeHandler(index_)));
	dataHandlers_.push_back(DataHandler::Ptr(new LevelHandler(index_, config_)));
	dataHandlers_.push_back(DataHandler::Ptr(new VersionHandler(index_)));
	dataHandlers_.push_back(DataHandler::Ptr(new GridHandler(grids_)));
}

Wdb2CdmBuilder::~Wdb2CdmBuilder()
{
}

void Wdb2CdmBuilder::populate(CDM & cdm) const
{
	std::string globalns = cdm.globalAttributeNS();
	BOOST_FOREACH( const CDMAttribute & attr, config_.getGlobalAttributes() )
		cdm.addAttribute(globalns, attr);

	addDimensions_(cdm);
	addParameterVariables_(cdm);
}


bool Wdb2CdmBuilder::isDatabaseField(const std::string & variableName) const
{
	const std::string & wdbName = getWdbName(variableName);
	return index_.hasParameter(wdbName);
}

std::vector<Wdb2CdmBuilder::gid> Wdb2CdmBuilder::getGridIdentifiers(const std::string & varName, unsigned unLimDimPos) const
{
	return index_.getData(getWdbName(varName), unLimDimPos);
}


std::vector<Wdb2CdmBuilder::gid> Wdb2CdmBuilder::getGridIdentifiers(const std::string & varName, const SliceBuilder & slicer, const CDM & cdm) const
{
	const std::string & wdbName = getWdbName(varName);

	std::vector<std::string> dimensions;
	getDimensionList(dimensions, wdbName);

	std::vector<size_t> start = slicer.getDimensionStartPositions();
	std::vector<size_t> size = slicer.getDimensionSizes();


	std::vector<size_t>::iterator st = start.begin();
	std::advance(st, 2); // skip x/y dimension
	std::vector<size_t>::iterator sz = size.begin();
	std::advance(sz, 2); // skip x/y dimension
	if ( not index_.hasManyVersions(wdbName) )
	{
		st = start.insert(st, 0);
		sz = size.insert(sz, 1);
	}
	++ st;
	++ sz;
	if ( not index_.hasManyLevels(wdbName) )
	{
		st = start.insert(st, 0);
		sz = size.insert(sz, 1);
	}
	++ st;
	++ sz;
	if ( not index_.hasManyValidTimeOffsets(wdbName) )
	{
		st = start.insert(st, 0);
		sz = size.insert(sz, 1);
	}
	++ st;
	++ sz;
	if ( not index_.hasManyReferenceTimes(wdbName) )
	{
		st = start.insert(st, 0);
		sz = size.insert(sz, 1);
	}
	++ st;
	++ sz;

	if ( start.size() != 6 or size.size() != 6 )
		throw CDMException("Internal error: Generating indices failed");

	std::reverse(start.begin(), start.end());
	std::reverse(size.begin(), size.end());

	return index_.getData(wdbName, start, size);
}


const GridInformation & Wdb2CdmBuilder::gridInformation() const
{
	// Several grid types in same wdb is not supported (yet)
	return * grids_.begin()->second;
}

std::set<GridData::Time> Wdb2CdmBuilder::referenceTimes() const
{
	return index_.referenceTimes();
}

void Wdb2CdmBuilder::addDimensions_(CDM & cdm) const
{
	BOOST_FOREACH( DataHandler::Ptr handler, dataHandlers_ )
		handler->addToCdm(cdm);
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

void Wdb2CdmBuilder::getDimensionList(std::vector<std::string> & out, const std::string & wdbParameter) const
{
	gridInformation().addSpatialDimensions(out);

	if ( index_.versionsForParameter(wdbParameter).size() > 1 )
		out.push_back(VersionHandler::cfName());
	if ( index_.levelsForParameter(wdbParameter).size() > 1 )
		out.push_back(getCfName(index_.levelTypeForParameter(wdbParameter).name()));
	if ( index_.timesForParameter(wdbParameter).size() > 1 )
	{
		if ( index_.referenceTimesForParameter(wdbParameter).size() > 1 )
			out.push_back(TimeHandler::timeOffsetName);
		else
			out.push_back(TimeHandler::validTimeName);
	}
	if ( index_.referenceTimesForParameter(wdbParameter).size() > 1 )
		out.push_back(TimeHandler::referenceTimeName);
}

void Wdb2CdmBuilder::addParameterVariables_(CDM & cdm) const
{
	BOOST_FOREACH( const std::string & parameter, index_.allParameters() )
	{
		GridSpecMap::const_iterator find = grids_.find(parameter);
		if ( find == grids_.end() )
			throw CDMException("Internal error - unable to find grid mapping"); // should never happen
		GridData::GridInformationPtr gridInfo = find->second;

		std::string dimension = getCfName(parameter);

		std::vector<std::string> dimensions;
		getDimensionList(dimensions, parameter);
		cdm.addVariable(CDMVariable(dimension, CDM_FLOAT, dimensions));

		GlobalWdbConfiguration::AttributeList attributes = config_.getAttributes(parameter, index_.unitForParameter(parameter));
		setAttribute(attributes, "_FillValue", std::numeric_limits<float>::quiet_NaN());

	   std::string coordinates = gridInfo->getCoordinatesAttribute();
	   BOOST_FOREACH(const DataHandler::Ptr & handler, dataHandlers_)
	   	   handler->addToCoordinatesAttribute(coordinates, parameter);
	   if ( not coordinates.empty() )
		   setAttribute(attributes, "coordinates", coordinates);
	   setAttribute(attributes, "grid_mapping", gridInfo->getProjectionName());


		BOOST_FOREACH( const CDMAttribute & attribute, attributes )
			cdm.addAttribute(dimension, attribute);
	}
}

const std::string & Wdb2CdmBuilder::getCfName(const std::string & wdbName) const
{
	std::string cfName = config_.cfName(wdbName);

	// We store all translations for later retrieval by the getWdbName method.
	CfNameToWdbName::iterator find = usedTranslations.find(cfName);
	if ( find == usedTranslations.end() )
		find = usedTranslations.insert(std::make_pair(cfName, wdbName)).first;
	else // Sanity check translation
		if( find->second != wdbName )
			throw std::runtime_error("Multiple translations for same variable: " + cfName);

	return find->first;
}

const std::string & Wdb2CdmBuilder::getWdbName(const std::string & cfName) const
{
	CfNameToWdbName::const_iterator find = usedTranslations.find(cfName);
	if ( find == usedTranslations.end() )
		return cfName;
		//throw std::runtime_error("internal error: No translation registered internally for " + cfName);

	return find->second;
}


}
}
