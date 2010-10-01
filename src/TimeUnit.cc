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
	out << fTime.getYear() << "-" << twoDigits(fTime.getMonth()) << "-" << twoDigits(fTime.getMDay()) << " ";
	out << twoDigits(fTime.getHour()) << ":" << twoDigits(fTime.getMinute()) << ":" << twoDigits(fTime.getSecond());
	if (fTime.getMSecond() > 0) {
		out << ".";
		if (fTime.getMSecond() < 10) {
			out << "00";
		} else if (fTime.getMSecond() < 100) {
			out << "0";
		}
		out << fTime.getMSecond();
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
	ft.setYear(string2type<int>(dateParts[0]));
	ft.setMonth(string2type<int>(dateParts[1]));
	ft.setMDay(string2type<int>(dateParts[2]));

	std::vector<std::string> timeParts = tokenize(dateTime[1], ":");
	if (timeParts.size() != 3) {
		throw CDMException("string2FimexTime: time does not consist of 3 parts");
	}
	ft.setHour(string2type<int>(timeParts[0]));
	ft.setMinute(string2type<int>(timeParts[1]));

	std::vector<std::string> secondParts = tokenize(timeParts[2], ".");
	ft.setSecond(string2type<int>(secondParts[0]));
	if (secondParts.size() > 1) {
		ft.setMSecond(static_cast<int>(round(string2type<double>("."+secondParts[1]) * 1000)));
	} else {
		ft.setMSecond(0);
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
	fiTime.setYear(static_cast<unsigned int>(year));
	fiTime.setMonth(static_cast<char>(month));
	fiTime.setMDay(static_cast<char>(mday));
	fiTime.setHour(static_cast<char>(hour));
	fiTime.setMinute(static_cast<char>(minute));
	fiTime.setSecond(static_cast<char>(second));
	fiTime.setMSecond(static_cast<unsigned int>((second - fiTime.getSecond())*1000));

	return fiTime;
}
boost::posix_time::ptime TimeUnit::unitTime2posixTime(double unitTime) const
{
    float second;
    int year, month, mday, hour, minute;
    handleUdUnitError(utCalendar(unitTime, pUnit.get(), &year, &month, &mday, &hour, &minute, &second), "converting double to calendar");
    // TODO nanoseconds?
    return boost::posix_time::ptime(boost::gregorian::date(year,month,mday), boost::posix_time::time_duration(hour, minute, (int) second));
}

double TimeUnit::fimexTime2unitTime(const FimexTime& fiTime) const throw(CDMException)
{
	float second = fiTime.getSecond() + (fiTime.getMSecond()/1000.);
	double retVal;
	handleUdUnitError(utInvCalendar(fiTime.getYear(), fiTime.getMonth(), fiTime.getMDay(), fiTime.getHour(), fiTime.getMinute(), second, pUnit.get(), &retVal), "converting calendar to double");
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
