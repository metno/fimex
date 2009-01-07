/*
 * Fimex, TimeSpec.h
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
 *
 *  Created on: Dec 11, 2008
 *      Author: Heiko Klein
 */

#ifndef TIMESPEC_H_
#define TIMESPEC_H_

#include "fimex/TimeUnit.h"
#include "fimex/CDMException.h"
#include <vector>
#include <string>

namespace MetNoFimex
{

/**
 * This class can be used to describe a list of times in an efficient textual way.
 *
 * Unless otherwise mentioned, i.e. with <em>bounds</em> a value v(time)
 * describes the time at exactly that instance. All times are UTC.
 *
 *
 * - TIMESTAMP format: YYYY-MM-DD HH:MM:SS
 * - TIMESTAMPS: comma-separated list of values with possible ... extension, ... meaning continuation of the difference of the previous two values
 * - UNIT: see <a href="http://www.unidata.ucar.edu/software/udunits/">udunit</a>, default: second
 * - VALUE: float-number
 * - VALUES: comma-separated list of values with possible ... extension, ... meaning continuation of the difference of the previous two values
 *         0 is the first time in the original time-axis, x is the last time-value in the original time-axis
 *
 * A TimeSpec consists of at least of timestamps or values:
 *
 * - timespec := (TIMESTAMPS | VALUES[;relativeUnit=UNIT])[;unit=UNIT]
 *
 * relativeUnit will reset the relative value 0 to the first value larger
 * than t0 (original start time)
 * with t0 =  i * (v1-v0)* unit with i being a integer.
 *
 * @subsubsection secTimeSpecEx1 Example: absolute times, every 4th hour
 *
 * @code
 * timespec = 2000-01-01 00:00:00,2000-01-01 00:04:00,...,2010-01-01 00:00:00
 * @endcode
 *
 * All times outside the original time-axis will be discarded.
 *
 * @subsubsection secTimeSpecEx2 Example: relative time, each 3rd hour starting at 00 o'clock, one extrapolation
 *
 * @code
 * timespec = -3,0,3,...,x,x+3;relativeUnit=hours since 2000-01-01 00:00:00;unit=hours since 2000-01-01 00:00:00;
 * @endcode
 */
class TimeSpec
{
public:
	/**
	 * Define a timeSpec
	 * @param timeSpec string representation as explained above
	 * @param startTime time to start in case of a relativeStart timeSpec
	 * @param endTime time to end in case of a relativeStart timeSpec
	 */
	TimeSpec(const std::string& timeSpec, const FimexTime& startTime, const FimexTime& endTime) throw(CDMException);
	virtual ~TimeSpec() {};
	const std::vector<FimexTime>& getTimeSteps() const {return timeSteps;}
	const std::string& getUnitString() const {return outputUnit;}

private:
	std::string outputUnit;
	std::vector<FimexTime> timeSteps;
};

} /* MetNoFimex */

#endif /* TIMESPEC_H_ */
