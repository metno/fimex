/*
 * TimeUnit.cc
 *
 *  Created on: Jul 18, 2008
 *      Author: heikok
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

double TimeUnit::unitTime2epochSeconds(double unitTime)
{
	return unitTime * epochSlope + epochOffset;
}
double TimeUnit::epochSeconds2unitTime(double epochSeconds)
{
	return (epochSeconds - epochOffset) / epochSlope;
}
FimexTime TimeUnit::unitTime2fimexTime(double unitTime) throw(CDMException)
{
	FimexTime fiTime;
	float second;
	handleUdUnitError(utCalendar(unitTime, pUnit.get(), &(fiTime.year), &(fiTime.month), &(fiTime.mday), &(fiTime.hour), &(fiTime.minute), &second), "converting double to calendar");
	fiTime.second = static_cast<int>(second);
	fiTime.msecond = static_cast<int>((second - fiTime.second)*1000);
	return fiTime;
}
double TimeUnit::fimexTime2unitTime(const FimexTime& fiTime) throw(CDMException)
{
	float second = fiTime.second + (fiTime.msecond/1000.);
	double retVal;
	handleUdUnitError(utInvCalendar(fiTime.year, fiTime.month, fiTime.mday, fiTime.hour, fiTime.minute, second, pUnit.get(), &retVal), "converting calendar to double");
	return retVal;
}

}
#endif // HAVE_UDUNITS
