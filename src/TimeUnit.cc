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
#include "fimex/config.h"
#if HAVE_UDUNITS
extern "C" {
#include "udunits.h"
}


namespace MetNoFimex
{

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
	handleUdUnitError(utCalendar(unitTime, pUnit.get(), &(fiTime.year), &(fiTime.month), &(fiTime.mday), &(fiTime.hour), &(fiTime.minute), &second), "converting double to calendar");
	fiTime.second = static_cast<int>(second);
	fiTime.msecond = static_cast<int>((second - fiTime.second)*1000);
	return fiTime;
}
double TimeUnit::fimexTime2unitTime(const FimexTime& fiTime) const throw(CDMException)
{
	float second = fiTime.second + (fiTime.msecond/1000.);
	double retVal;
	handleUdUnitError(utInvCalendar(fiTime.year, fiTime.month, fiTime.mday, fiTime.hour, fiTime.minute, second, pUnit.get(), &retVal), "converting calendar to double");
	return retVal;
}

}
#endif // HAVE_UDUNITS
