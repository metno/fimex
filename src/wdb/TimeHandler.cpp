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
#include <boost/foreach.hpp>


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
	const std::set<GridData::Duration> & validTimes = index_.allTimes();

	Dimension unlimited = unlimitedDimension();

	std::vector<std::string> runShape;
	const std::string reftime = referenceTimeName;
	if ( referenceTimes.size() > 1 )
	{
		//throw CDMException("Only one reference time is allowed");
		CDMDimension referenceTimeDimension(reftime, referenceTimes.size());
		if ( unlimited == ReferenceTime )
			referenceTimeDimension.setUnlimited(true);

		cdm.addDimension(referenceTimeDimension);
		runShape.push_back(reftime);
	}
	cdm.addVariable(CDMVariable(reftime, CDM_DOUBLE, runShape));
	cdm.addAttribute(reftime, CDMAttribute("long_name", "Run time for model"));
	cdm.addAttribute(reftime, CDMAttribute("standard_name", "forecast_reference_time"));
	cdm.addAttribute(reftime, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
	cdm.addAttribute(reftime, CDMAttribute("_CoordinateAxisType", "RunTime"));


	std::string validTime = validTimeName;
	std::vector<std::string> timeShape;
	timeShape.push_back(validTime);
	if ( referenceTimes.size() > 1)
		timeShape.push_back(reftime);

	cdm.addVariable(CDMVariable(validTime, CDM_DOUBLE, timeShape));
	cdm.addAttribute(validTime, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
	cdm.addAttribute(validTime, CDMAttribute("long_name", "forecast (valid) time"));
	cdm.addAttribute(validTime, CDMAttribute("standard_name", "time"));
	cdm.addAttribute(validTime, CDMAttribute("axis", "T"));

	CDMDimension time(validTime, validTimes.size());
	if ( unlimited == ValidTime )
		time.setUnlimited(true);
	cdm.addDimension(time);
}

namespace
{
std::time_t secondsSinceEpoch(const GridData::Time & time)
{
	using namespace boost::posix_time;

	static const ptime epoch(boost::gregorian::date(1970,1,1));

	time_duration duration = time - epoch;

	return duration.total_seconds();
}
}

boost::shared_ptr<Data> TimeHandler::getData(const CDMVariable & variable, size_t unLimDimPos) const
{
	boost::shared_ptr<Data> ret;


	const GridData::Time & refTime = getReferenceTime(unLimDimPos);

	std::string varName = variable.getName();
	if ( varName == referenceTimeName )
		ret = createData(CDM_DOUBLE, 1, secondsSinceEpoch(refTime));
	else if ( varName == validTimeName )
	{
		if ( unlimitedDimension() == ReferenceTime )
		{
			const std::set<GridData::Duration> & allTimes = index_.allTimes();
			ret = createData(CDM_DOUBLE, allTimes.size());
			unsigned i = 0;
			BOOST_FOREACH(const GridData::Duration & t, allTimes)
				ret->setValue(i ++, secondsSinceEpoch(refTime + t));
		}
		else
		{
			// fix time functions: TimeUnit.h
			const std::set<GridData::Duration> & allTimes = index_.allTimes();
			std::set<GridData::Duration>::const_iterator thisTime = allTimes.begin();

			if ( unLimDimPos > allTimes.size() )
				throw CDMException("Invalid time index");

			std::advance(thisTime, unLimDimPos);

			ret = createData(CDM_DOUBLE, 1, secondsSinceEpoch(refTime + * thisTime));
		}
	}
	else
		throw CDMException("Unrecognized variable name for time: " + varName);

	return ret;
}

bool TimeHandler::canHandle(const std::string & wdbName) const
{
	return wdbName == validTimeName or wdbName == referenceTimeName;
}

TimeHandler::Dimension TimeHandler::unlimitedDimension() const
{
	if ( index_.referenceTimes().size() > 1 )
		return ReferenceTime;
	else if ( index_.allTimes().size() > 1 )
		return ValidTime;
	else
		return Other;
}

GridData::Time TimeHandler::getReferenceTime(size_t unLimDimPos) const
{
	if ( unlimitedDimension() == ReferenceTime )
	{
		const std::set<GridData::Time> & referenceTimes = index_.referenceTimes();
		std::set<GridData::Time>::const_iterator thisTime = referenceTimes.begin();

		if ( unLimDimPos >= referenceTimes.size() )
			throw CDMException("Invalid time index");

		std::advance(thisTime, unLimDimPos);

		return * thisTime;
	}
	else
		return * index_.referenceTimes().begin();
}

}
}
