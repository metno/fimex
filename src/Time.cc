#include "Time.h"
#include <cassert>
#include <udunits.h>
extern "C" int utIsInit(); // this has been forgotten in the udunits.h 12.4

namespace MetNoUtplukk {

Time::Time(std::string format, double value) throw(TimeException)
: format(format), value(value)
{
	if (!utIsInit()) {
		if (utInit("") != 0) {
			throw TimeException("unable to initialize udunits");
		}
	}
	if (utScan(format.c_str() , unit.get()) != 0) {
		throw TimeException("unknown time format");
	}
}

Time::Time(boost::shared_ptr<utUnit> unit, double value) throw(TimeException)
: unit(unit), value(0)
{
	if (! utIsTime(unit.get())) {
		throw TimeException("initializaton unit not a time unit");
	}
	char* tempChar;
	if (utPrint(unit.get(), &tempChar) != 0) {
		throw TimeException("utPrint failed");
	}
	format = tempChar;
}

Time::~Time()
{
}

void Time::convert(const Time& newTimeFormat)
{
	double slope, intercept;
	assert(0 == utConvert(unit.get(), newTimeFormat.unit.get(), &slope, &intercept)); // should never happen
	double newVal = slope * value + intercept;
	unit = newTimeFormat.unit;
	value = newVal;
	format = newTimeFormat.getFormatString();
}

} // end namespace MetNoUtplukk
