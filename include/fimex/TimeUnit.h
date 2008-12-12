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

namespace MetNoFimex
{

/**
 * time representation
 */
struct FimexTime {
	/// millisecond
	int msecond;
	/// second (0-59)
	int second;
	/// minute (0-59)
	int minute;
	/// hour (0-23)
	int hour;
	/// day of month (1-31)
	int mday;
	/// month (1-12)
	int month;
	/// year (2008 as of writing)
	int year;
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
private:
	void init(const std::string& timeUnitString = "seconds since 1970-01-01 00:00:00") throw(CDMException);
};

}

#endif /* TIMEUNIT_H_ */
