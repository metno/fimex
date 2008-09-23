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

#include "fimex/Time.h"
#include <cassert>
extern "C" int utIsInit(); // this has been forgotten in the udunits.h 12.4

namespace MetNoFimex {

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

} // end namespace MetNoFimex
