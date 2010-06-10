/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

#include "fimex/TimeUnit.h"
#include "fimex/Utils.h"
#include <boost/date_time/posix_time/posix_time.hpp>

extern "C" {
#include "udunits.h"
}


namespace MetNoFimex
{

static std::string twoDigits(int i) {
	std::string s = type2string(i);
	if (s.length() < 2) {
		return "0" + s;
	} else {
		return s;
	}
}

bool FimexTime::operator==(const FimexTime &rhs) const
{
	return year == rhs.year &&
		month == rhs.month &&
		mday == rhs.mday &&
		hour == rhs.hour &&
		minute == rhs.minute &&
		second == rhs.second &&
		msecond == rhs.msecond;
}

std::ostream& operator<< (std::ostream& out, const FimexTime& fTime)
{
	out << fTime.year << "-" << twoDigits(fTime.month) << "-" << twoDigits(fTime.mday) << " ";
	out << twoDigits(fTime.hour) << ":" << twoDigits(fTime.minute) << ":" << twoDigits(fTime.second);
	if (fTime.msecond > 0) {
		out << ".";
		if (fTime.msecond < 10) {
			out << "00";
		} else if (fTime.msecond < 100) {
			out << "0";
		}
		out << fTime.msecond;
	}
	return out;
}

FimexTime string2FimexTime(const std::string& str) throw(CDMException)
{
	FimexTime ft;
	std::vector<std::string> dateTime = tokenize(str, " ");
	if (dateTime.size() != 2) {
		throw CDMException("string2FimexTime: date and time not found:" + str);
	}
	std::vector<std::string> dateParts = tokenize(dateTime[0], "-");
	if (dateParts.size() != 3) {
		throw CDMException("string2FimexTime: date does not consist of 3 parts:" +str);
	}
	ft.year = string2type<int>(dateParts[0]);
	ft.month = string2type<int>(dateParts[1]);
	ft.mday = string2type<int>(dateParts[2]);

	std::vector<std::string> timeParts = tokenize(dateTime[1], ":");
	if (timeParts.size() != 3) {
		throw CDMException("string2FimexTime: time does not consist of 3 parts");
	}
	ft.hour = string2type<int>(timeParts[0]);
	ft.minute = string2type<int>(timeParts[1]);

	std::vector<std::string> secondParts = tokenize(timeParts[2], ".");
	ft.second = string2type<int>(secondParts[0]);
	if (secondParts.size() > 1) {
		ft.msecond = static_cast<int>(round(string2type<double>("."+secondParts[1]) * 1000));
	} else {
		ft.msecond = 0;
	}
	return ft;
}

void TimeUnit::init(const std::string& timeUnitString) throw(CDMException)
{
	if (!units.isTime(timeUnitString)) {
		throw CDMException("trying to initialize time with wrong unit: "+timeUnitString);
	} else {
		units.convert(timeUnitString, "seconds since 1970-01-01 00:00:00 +00:00", epochSlope, epochOffset);
		pUnit = boost::shared_ptr<utUnit>(new utUnit);
		utScan(timeUnitString.c_str(), pUnit.get());
	}
}

TimeUnit::TimeUnit() throw(CDMException)
{
	init();
}
TimeUnit::TimeUnit(const std::string& timeUnit) throw(CDMException)
{
	init(timeUnit);
}

TimeUnit::~TimeUnit()
{
}

double TimeUnit::unitTime2epochSeconds(double unitTime) const
{
	return unitTime * epochSlope + epochOffset;
}
double TimeUnit::epochSeconds2unitTime(double epochSeconds) const
{
	return (epochSeconds - epochOffset) / epochSlope;
}
FimexTime TimeUnit::unitTime2fimexTime(double unitTime) const throw(CDMException)
{
	FimexTime fiTime;
	float second;
	int year, month, mday, hour, minute;
	handleUdUnitError(utCalendar(unitTime, pUnit.get(), &year, &month, &mday, &hour, &minute, &second), "converting double to calendar");
	fiTime.year = static_cast<unsigned int>(year);
	fiTime.month = static_cast<char>(month);
	fiTime.mday = static_cast<char>(mday);
	fiTime.hour = static_cast<char>(hour);
	fiTime.minute = static_cast<char>(minute);
	fiTime.second = static_cast<char>(second);
	fiTime.msecond = static_cast<unsigned int>((second - fiTime.second)*1000);

	return fiTime;
}
double TimeUnit::fimexTime2unitTime(const FimexTime& fiTime) const throw(CDMException)
{
	float second = fiTime.second + (fiTime.msecond/1000.);
	double retVal;
	handleUdUnitError(utInvCalendar(fiTime.year, fiTime.month, fiTime.mday, fiTime.hour, fiTime.minute, second, pUnit.get(), &retVal), "converting calendar to double");
	return retVal;
}

double TimeUnit::posixTime2unitTime(boost::posix_time::ptime pTime) const throw(CDMException)
{
    boost::gregorian::date pDate = pTime.date();
    boost::posix_time::time_duration pTimeDur = pTime.time_of_day();
    double retVal;
    float seconds = pTimeDur.seconds() + (pTimeDur.fractional_seconds() * 1. / boost::posix_time::time_duration::ticks_per_second());
    handleUdUnitError(utInvCalendar(pDate.year(), pDate.month(), pDate.day(), pTimeDur.hours(), pTimeDur.minutes(), seconds, pUnit.get(), &retVal), "converting calendar to double");
    return retVal;
}


}
