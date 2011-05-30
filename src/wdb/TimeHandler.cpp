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

#include "TimeHandler.h"
#include "WdbIndex.h"
#include <fimex/CDM.h>
#include <fimex/Data.h>


namespace MetNoFimex
{
namespace wdb
{

const std::string TimeHandler::referenceTimeName = "forecast_reference_time";
const std::string TimeHandler::validTimeName = "time";


TimeHandler::TimeHandler(const WdbIndex & index) :
		index_(index)
{
}

TimeHandler::~TimeHandler()
{
}

void TimeHandler::addToCdm(CDM & cdm) const
{
	const std::set<GridData::Time> & referenceTimes = index_.referenceTimes();
	const std::set<GridData::Time> & validTimes = index_.allTimes();

	std::vector<std::string> runShape;
	const std::string reftime = referenceTimeName;
	if ( referenceTimes.size() > 1 )
	{
		throw CDMException("Only one reference time is allowed");
		//cdm.addDimension(CDMDimension(reftime, referenceTimes.size()));
		//runShape.push_back(reftime);
	}
	cdm.addVariable(CDMVariable(reftime, CDM_DOUBLE, runShape));
	cdm.addAttribute(reftime, CDMAttribute("long_name", "Run time for model"));
	cdm.addAttribute(reftime, CDMAttribute("standard_name", "forecast_reference_time"));
	cdm.addAttribute(reftime, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
	cdm.addAttribute(reftime, CDMAttribute("_CoordinateAxisType", "RunTime"));


	std::string validTime = validTimeName;
	std::vector<std::string> timeShape;
	if ( referenceTimes.size() > 1)
		timeShape.push_back(reftime);
	timeShape.push_back(validTime);

	cdm.addVariable(CDMVariable(validTime, CDM_DOUBLE, timeShape));
	cdm.addAttribute(validTime, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
	cdm.addAttribute(validTime, CDMAttribute("long_name", "forecast (valid) time"));
	cdm.addAttribute(validTime, CDMAttribute("standard_name", "time"));
	cdm.addAttribute(validTime, CDMAttribute("axis", "T"));

	CDMDimension time(validTime, validTimes.size());
	time.setUnlimited(true);
	cdm.addDimension(time);
}

boost::shared_ptr<Data> TimeHandler::getData(const CDMVariable & variable, size_t unLimDimPos) const
{
	boost::shared_ptr<Data> ret;

	std::string varName = variable.getName();

	if ( varName == referenceTimeName )
	{
		const std::set<GridData::Time> & referenceTimes = index_.referenceTimes();
		ret = createData(CDM_DOUBLE, referenceTimes.size());

		int idx = 0;
		for ( std::set<GridData::Time>::const_iterator it = referenceTimes.begin(); it != referenceTimes.end(); ++ it )
		{
			std::tm t = to_tm(* it);
			ret->setValue(idx ++, std::mktime(& t));
		}
	}
	else if ( varName == validTimeName )
	{
		// fix time functions: TimeUnit.h
		const std::set<wdb::GridData::Time> & allTimes = index_.allTimes();
		std::set<wdb::GridData::Time>::const_iterator thisTime = allTimes.begin();
		std::advance(thisTime, unLimDimPos -1);
		std::tm t = to_tm(* thisTime);
		ret = createData(CDM_DOUBLE, 1, std::mktime(& t));
	}
	else
		throw CDMException("Unrecognized variable name for time: " + varName);

	return ret;
}

bool TimeHandler::canHandle(const std::string & wdbName) const
{
	return wdbName == validTimeName or wdbName == referenceTimeName;
}

}
}
