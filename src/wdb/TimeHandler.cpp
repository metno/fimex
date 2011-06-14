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

const std::string TimeHandler::referenceTimeName = "runtime";
const std::string TimeHandler::validTimeName = "time";
const std::string TimeHandler::timeOffsetName = "offsetTime";


TimeHandler::TimeHandler(const WdbIndex & index) :
		index_(index)
{
}

TimeHandler::~TimeHandler()
{
}

void TimeHandler::addToCdm(CDM & cdm) const
{
	addReferenceTimeToCdm(cdm);
	addTimeOffsetToCdm(cdm);
	addValidTimeToCdm(cdm);
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
	else if ( varName == timeOffsetName )
	{
		const std::set<GridData::Duration> & allTimes = index_.allTimes();
		ret = createData(CDM_DOUBLE, allTimes.size());
		unsigned i = 0;
		BOOST_FOREACH(const GridData::Duration & t, allTimes)
			ret->setValue(i ++, t.total_seconds());
	}
	else
		throw CDMException("Unrecognized variable name for time: " + varName);

	return ret;
}

bool TimeHandler::canHandle(const std::string & wdbName) const
{
	return wdbName == validTimeName
			or wdbName == referenceTimeName
			or wdbName == timeOffsetName;
}

void TimeHandler::addToCoordinatesAttribute(std::string & coordinates, const std::string & wdbName) const
{
	std::cout << "OK: " << wdbName;
	if ( index_.hasManyReferenceTimes(wdbName) and index_.hasManyValidTimeOffsets(wdbName) )
	{
		if ( not coordinates.empty() )
			coordinates += ' ';
		coordinates += validTimeName;
		std::cout << " = " << coordinates;
	}
	std::cout << std::endl;
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

void TimeHandler::addReferenceTimeToCdm(CDM & cdm) const
{
	const std::set<GridData::Time> & referenceTimes = index_.referenceTimes();

	Dimension unlimited = unlimitedDimension();

	std::vector<std::string> runShape;
	const std::string reftime = referenceTimeName;
	if ( referenceTimes.size() > 1 )
	{
		CDMDimension referenceTimeDimension(referenceTimeName, referenceTimes.size());
		if ( unlimited == ReferenceTime )
			referenceTimeDimension.setUnlimited(true);

		cdm.addDimension(referenceTimeDimension);
		runShape.push_back(referenceTimeName);
	}
	cdm.addVariable(CDMVariable(referenceTimeName, CDM_DOUBLE, runShape));
	cdm.addAttribute(referenceTimeName, CDMAttribute("long_name", "Run time for model"));
	cdm.addAttribute(referenceTimeName, CDMAttribute("standard_name", "forecast_reference_time"));
	cdm.addAttribute(referenceTimeName, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
	cdm.addAttribute(referenceTimeName, CDMAttribute("_CoordinateAxisType", "RunTime"));
}

void TimeHandler::addTimeOffsetToCdm(CDM & cdm) const
{
	const std::set<GridData::Time> & referenceTimes = index_.referenceTimes();
	const std::set<GridData::Duration> & validTimes = index_.allTimes();

	if ( referenceTimes.size() > 1 and validTimes.size() > 1 )
	{
		cdm.addDimension(CDMDimension(timeOffsetName, validTimes.size()));

		cdm.addVariable(CDMVariable(timeOffsetName, CDM_DOUBLE, std::vector<std::string>(1, timeOffsetName)));
		cdm.addAttribute(timeOffsetName, CDMAttribute("long_name", "offset since referenceTime"));
		cdm.addAttribute(timeOffsetName, CDMAttribute("units", "seconds"));
	}
}

void TimeHandler::addValidTimeToCdm(CDM & cdm) const
{
	const std::set<GridData::Time> & referenceTimes = index_.referenceTimes();
	const std::set<GridData::Duration> & validTimes = index_.allTimes();

	Dimension unlimited = unlimitedDimension();

	std::vector<std::string> timeShape;
	if ( referenceTimes.size() > 1)
	{
		timeShape.push_back(timeOffsetName);
		timeShape.push_back(referenceTimeName);
	}
	else
		timeShape.push_back(validTimeName);

	cdm.addVariable(CDMVariable(validTimeName, CDM_DOUBLE, timeShape));
	cdm.addAttribute(validTimeName, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
	cdm.addAttribute(validTimeName, CDMAttribute("long_name", "forecast (valid) time"));
	cdm.addAttribute(validTimeName, CDMAttribute("standard_name", "time"));
	cdm.addAttribute(validTimeName, CDMAttribute("axis", "T"));

	if ( referenceTimes.size() == 1 )
	{
		CDMDimension time(validTimeName, validTimes.size());
		if ( unlimited == ValidTime )
			time.setUnlimited(true);
		cdm.addDimension(time);
	}
}


}
}
