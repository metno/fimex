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

#include "GridHandler.h"
#include "gridInformation/GridInformation.h"
#include "database_access/GridData.h"
#include <fimex/CDM.h>
#include <boost/foreach.hpp>
#include <set>


namespace MetNoFimex
{
namespace wdb
{

GridHandler::GridHandler(const GridSpecMap & grids) :
		grids_(grids)
{
}

GridHandler::~GridHandler()
{
}

void GridHandler::addToCdm(CDM & cdm) const
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

DataPtr GridHandler::getData(const CDMVariable & variable, size_t unLimDimPos) const
{
	return gridInfo_().getField(variable);
}

bool GridHandler::canHandle(const std::string & cfName) const
{
	return gridInfo_().canHandle(cfName);
}

const wdb::GridInformation & GridHandler::gridInfo_() const
{
	GridSpecMap::const_iterator find = grids_.begin();
	if ( find == grids_.end() )
		throw CDMException("No grids");

	const wdb::GridInformation & gridInfo = * find->second;

	return gridInfo;
}

}
}
