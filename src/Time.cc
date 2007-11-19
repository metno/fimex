#include "Time.h"
#include <cassert>
extern "C" int utIsInit(); // this has been forgotten in the udunits.h 12.4

namespace MetNoUtplukk {

Time::Time(std::string format, double value) throw(TimeException)
: format(format), unit(new utUnit()), value(value)
{
	int uterror;
	if (!utIsInit()) {
		if ((uterror = utInit("")) != 0) {
			throw TimeException(uterror);
		}
	}
	if ((uterror = utScan(format.c_str() , unit.get())) != 0) {
		throw TimeException(uterror);
	}
}

Time::Time(boost::shared_ptr<utUnit> unit, double value) throw(TimeException)
: unit(unit), value(value)
{
	if (! utIsTime(unit.get())) {
		throw TimeException("initializaton unit not a time unit");
	}
	char* tempChar;
	int uterror;
	if ((uterror = utPrint(unit.get(), &tempChar)) != 0) {
		throw TimeException(uterror);
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
