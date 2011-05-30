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

#ifndef GRIDDATAFACTORY_H_
#define GRIDDATAFACTORY_H_

#include <wdb/database_access/GridData.h>
//#include <wdb/WdbIndex.h>

namespace MetNoFimex
{

namespace wdb
{

class GridDataFactory
{
public:
	GridDataFactory();
	~GridDataFactory();

	void add(const wdb::Parameter & parameter = defaultParameter, const std::string & time = defaultTime, int version = 0);
	void add(const wdb::Parameter & parameter, const wdb::Level & lvl, const std::string & time = defaultTime);
	void add(const wdb::Level & lvl, const std::string & time = defaultTime);
	void add(const std::string & time, const std::string & referenceTime = defaultReferenceTime);
	void add(int dataVersion, const std::string & time = defaultTime);
	void add(const wdb::GridData::GridInformationPtr & gridInfo);

	const std::vector<GridData> & gridData() const { return gridData_; }

	static const Parameter defaultParameter;
	static const Level defaultLevel;
	static const std::string defaultTime;
	static const std::string defaultReferenceTime;
	static const GridData::GridInformationPtr defaultGrid;

private:
	GridData::gid nextGid()
	{
		return nextGid_ ++;
	}

	GridData::gid nextGid_;

	std::vector<GridData> gridData_;
};

}

}

#endif /* GRIDDATAFACTORY_H_ */
