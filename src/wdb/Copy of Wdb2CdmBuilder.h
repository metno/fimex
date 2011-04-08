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

#ifndef DATAINDEX_H_
#define DATAINDEX_H_

#include "WdbIndex.h"
#include "GridData.h"
#include <vector>
#include <map>
#include <string>
#include <iosfwd>
#include <set>


namespace MetNoFimex
{
class CDM;


namespace wdb
{
class CdmNameTranslator;
class Parameter;
class Level;


class Wdb2CdmBuilder
{
public:
	Wdb2CdmBuilder(const std::vector<GridData> & data, const CdmNameTranslator & translator);
	~Wdb2CdmBuilder();

	void populate(CDM & cdm) const;


	typedef GridData::gid gid;
	typedef GridData::Time Time;

	bool isDatabaseField(const std::string & variableName) const;
	std::vector<gid> getGridIdentifiers(const std::string & variableName, int timeIndex) const;


private:

	void addDimensions_(CDM & cdm) const;
	void addLevelDimensions_(CDM & cdm) const;
	void addVersionDimension_(CDM & cdm) const;
	void addTimeDimensions_(CDM & cdm) const;

	void addProjectionInformation_(CDM & cdm) const;

	void addReferenceTimeInformation_(CDM & cdm) const;

	void addParameterVariables_(CDM & cdm) const;
	void getDimensionsForParameter_(std::vector<std::string> & out, const TimeEntry & levelEntry) const;
	void getTimeDimensionForParameter_(std::vector<std::string> & out, const TimeEntry & levelEntry) const;
	void getLevelDimensionsForParameter_(std::vector<std::string> & out, const TimeEntry & levelEntry) const;
	void getVersionDimensionsForParameter_(std::vector<std::string> & out, const TimeEntry & levelEntry) const;

	WdbIndex index_;
	const CdmNameTranslator & translator_;

	typedef std::map<Parameter, GridData::GridInformationPtr> GridSpecMap;
	GridSpecMap grids_;
};

}

}

#endif /* DATAINDEX_H_ */
