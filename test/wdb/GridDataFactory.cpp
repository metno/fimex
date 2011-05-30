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

#include "GridDataFactory.h"
#include "TestingGridData.h"
#include <wdb/gridInformation/GridInformation.h>

namespace MetNoFimex
{

namespace wdb
{

GridDataFactory::GridDataFactory() :
		nextGid_(0)
{
}

GridDataFactory::~GridDataFactory()
{
}

namespace
{
static boost::posix_time::ptime t(const std::string & time)
{
	return boost::posix_time::time_from_string(time);
}
}

void GridDataFactory::add(const wdb::Parameter & parameter, const std::string & time, int version)
{
	gridData_.push_back(TestingGridData(parameter, defaultLevel, version, t(time), t(defaultReferenceTime), defaultGrid, nextGid()));
}

void GridDataFactory::add(const wdb::Parameter & parameter, const wdb::Level & lvl, const std::string & time)
{
	gridData_.push_back(TestingGridData(parameter, lvl, 0, t(time), t(defaultReferenceTime), defaultGrid, nextGid()));
}

void GridDataFactory::add(const wdb::Level & lvl, const std::string & time)
{
	gridData_.push_back(TestingGridData(defaultParameter, lvl, 0, t(time), t(defaultReferenceTime), defaultGrid, nextGid()));
}

void GridDataFactory::add(const std::string & time, const std::string & referenceTime)
{
	gridData_.push_back(TestingGridData(defaultParameter, defaultLevel, 0, t(time), t(referenceTime), defaultGrid, nextGid()));
}

void GridDataFactory::add(int dataVersion, const std::string & time)
{
	gridData_.push_back(TestingGridData(defaultParameter, defaultLevel, dataVersion, t(time), t(defaultReferenceTime), defaultGrid, nextGid()));
}

void GridDataFactory::add(const wdb::GridData::GridInformationPtr & gridInfo)
{
	gridData_.push_back(TestingGridData(defaultParameter, defaultLevel, 0, t(defaultTime), t(defaultReferenceTime), gridInfo, nextGid()));
}


const Parameter GridDataFactory::defaultParameter("air temperature", "C");
const Level GridDataFactory::defaultLevel("distance above ground", "m", 0, 0);
const std::string GridDataFactory::defaultTime = "2011-03-18 06:00:00";
const std::string GridDataFactory::defaultReferenceTime = "2011-03-18 00:00:00";
const GridData::GridInformationPtr GridDataFactory::defaultGrid(GridInformation::get("+proj=longlat +a=6367470.0 +towgs84=0,0,0 +no_defs", 30, 20));

}

}
