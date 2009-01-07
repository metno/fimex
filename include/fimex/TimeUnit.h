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

#ifndef TIMEUNIT_H_
#define TIMEUNIT_H_

#include "boost/shared_ptr.hpp"
#include "fimex/Units.h"
#include "fimex/CDMException.h"
#include <iostream>

// pre-declaration of utUnit pointer
struct utUnit;
namespace boost
{
	namespace posix_time {
		struct ptime;
		struct time_duration;
	}
}

namespace MetNoFimex
{

/**
 * time representation and some overloaded operators
 *
 * @warning: the implementor needs to make sure, that all values are
 * given correctly, i.e. seconds between 0 and 59
 */
class FimexTime {
public:
	FimexTime() {}
	/// construct by ptime
	FimexTime(const boost::posix_time::ptime& time);
	/// millisecond
	unsigned short msecond;
	/// second (0-59)
	char second;
	/// minute (0-59)
	char minute;
	/// hour (0-23)
	char hour;
	/// day of month (1-31)
	char mday;
	/// month (1-12)
	char month;
	/// year (2008 as of writing)
	unsigned short year;
	/// compare two fimexTimes
	bool operator==(const FimexTime &rhs) const;
	/// compare two fimexTimes
	bool operator!=(const FimexTime &rhs) const { return !(*this == rhs); }
	/// compare two fimexTimes
	bool operator>(const FimexTime &rhs) const;
	/// compare two fimexTimes
	bool operator<(const FimexTime &rhs) const { return (rhs > *this); }
	/// compare two fimexTimes
	bool operator>=(const FimexTime &rhs) const { return !(rhs > *this); }
	/// compare two fimexTimes
	bool operator<=(const FimexTime &rhs) const { return !(*this > rhs); }
	/// add a time_duration to a FimexTime
	FimexTime operator+(const boost::posix_time::time_duration& timeDuration) const;
	/// get the difference of two FimexTimes
	boost::posix_time::time_duration operator-(const FimexTime &rhs) const;

private:
	boost::posix_time::ptime toPTime() const;
};


std::ostream& operator<< (std::ostream& out, const FimexTime& fTime);
FimexTime string2FimexTime(const std::string& str) throw(CDMException);

/**
 * TimeUnit calculates times from a time given in a unit as
 * of CF-1.0 (e.g. 'days since 2000-01-01 00:00:00') to a
 * unix time (i.e. 'seconds since 1970-01-01 00:00:00') or a
 * time struct #FimexTime
 *
 * All times are assumed to be UTC, and we use the Gregorian Calendar
 * (not 100% true for times before 1600AD, depending on implementation)
 *
 */
class TimeUnit
{
	Units units; // unit initialization
	boost::shared_ptr<utUnit> pUnit; // pointer to unit implementation
	double epochOffset;
	double epochSlope;
public:
	/// initialize a timeUnit with a unit string
	TimeUnit() throw(CDMException);
	TimeUnit(const std::string& timeUnitString) throw(CDMException);
	virtual ~TimeUnit();
	/// calculate the epochSeconds for a time in the current unit
	double unitTime2epochSeconds(double unitTime) const;
	/// calculate the time in the current unit from the epoch
	double epochSeconds2unitTime(double epochSeconds) const;
	/// calculate the time in a calendar form
	FimexTime unitTime2fimexTime(double unitTime) const throw(CDMException);
	/// calculate the time in the current unit from the calendar form
	double fimexTime2unitTime(const FimexTime& fiTime) const throw(CDMException);
	/// same as #fimexTime2unitTime but copying fiTime instead of referencing, needed for i.e. bind1st(mem_fun())
	double fimexTime2unitTimeX(FimexTime fiTime) const throw(CDMException) { return fimexTime2unitTime(fiTime); }
private:
	void init(const std::string& timeUnitString = "seconds since 1970-01-01 00:00:00") throw(CDMException);
};

}

#endif /* TIMEUNIT_H_ */
