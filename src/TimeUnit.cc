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
#include <limits>

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

void FimexTime::setTime(unsigned short year, char month, char mday, char hour, char minute, char second, unsigned short msecond)
{
    this->year = year;
    this->month = month;
    this->mday = mday;
    this->hour = hour;
    this->minute = minute;
    this->second = second;
    this->msecond = msecond;
}


FimexTime::FimexTime(unsigned short year, char month, char mday, char hour, char minute, char second, unsigned short msecond)
: year(year), month(month), mday(mday), hour(hour), minute(minute), second(second), msecond(msecond)
{
}

FimexTime::FimexTime(special_values val)
{
    switch (val) {
        case min_date_time: setTime(0, CHAR_MIN, CHAR_MIN, CHAR_MIN, CHAR_MIN, CHAR_MIN, 0); break;
        case max_date_time: setTime(USHRT_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, USHRT_MAX); break;
        default: throw CDMException("unimplemented special_value: "+val);
    }
}

bool FimexTime::parseISO8601(const std::string& isoStr)
{
    using namespace std;
    string trimStr = trim(isoStr);
    if (trimStr.size() == 0) return false;

    // T delimiter for date and time
    vector<string> dateTime = tokenize(trimStr, "T");
    if (dateTime.size() == 1) {
        // try space delimiter for date and time
        dateTime = tokenize(trimStr, " ");
    }

    // convert date and time 'int' vectors
    vector<string> time;
    vector<int> date;
    if (dateTime.size() == 1) {
        date = tokenizeDotted<int>(dateTime[0], "-");
        if (date.size() == 3) {
            // set time to zero
            time.push_back("0");
            time.push_back("0");
            time.push_back("0.0");
        } else {
            return false; // date required
        }
    } else if (dateTime.size() == 2) {
        date = tokenizeDotted<int>(dateTime[0], "-");
        time = tokenize(dateTime[1], ":");
    } else {
        return false;
    }

    if (time.size() == 2) time.push_back(0);
    // check date and time and set
    if ((date.size() == 3) && (time.size() == 3)) {
        vector<string> seconds = tokenize(time[2], ".");
        int milliSecs = 0;
        if (seconds.size() > 1) {
            milliSecs = static_cast<int>(round(string2type<double>("."+seconds[1]) * 1000));
        }
        setTime(date[0], date[1], date[2], string2type<short>(time[0]), string2type<short>(time[1]), string2type<short>(seconds[0]), milliSecs);
        return true;
    } else {
        return false;
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
	if (!ft.parseISO8601(str)) {
	    throw CDMException("string2FimexTime: date and time not found:" + str);
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
