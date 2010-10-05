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

#include "fimex/Units.h"
#include "fimex/Logger.h"

extern "C" {
#include "udunits.h"
}
// add forgotten utIsInit fom udunits
extern "C" int utIsInit();

namespace MetNoFimex
{


static LoggerPtr logger = getLogger("fimex.Units");

void handleUdUnitError(int unitErrCode, const std::string& message) throw(UnitException)
{
	switch (unitErrCode) {
	case 0: break;
	case UT_EOF: throw UnitException("end-of-file encountered : " + message);
    case UT_ENOFILE: throw UnitException("no units-file : " + message);
    case UT_ESYNTAX: throw UnitException("syntax error : " + message);
    case UT_EUNKNOWN: throw UnitException("unknown specification : " + message);
    case UT_EIO: throw UnitException("I/O error : " + message);
    case UT_EINVALID: throw UnitException("invalid unit-structure : " + message);
    case UT_ENOINIT: throw UnitException("package not initialized : " + message);
    case UT_ECONVERT: throw UnitException("two units are not convertable : " + message);
    case UT_EALLOC: throw UnitException("memory allocation failure : " + message);
    case UT_ENOROOM: throw UnitException("insufficient room supplied : " + message);
    case UT_ENOTTIME: throw UnitException("not a unit of time : " + message);
    case UT_DUP: throw UnitException("duplicate unit : " + message);
	default: throw UnitException("unknown error");
	}
}
int Units::counter = 0;
Units::Units()
{
	if (!utIsInit()) {
		handleUdUnitError(utInit(0));
	}
	++counter;
}

Units::Units(const Units& u)
{
	counter++;
}

Units& Units::operator=(const Units& rhs)
{
	return *this; // no state! no increase/decrease to counter required
}

Units::~Units()
{
    --counter;
}

bool Units::unload(bool force) throw(UnitException)
{
    if (force || counter <= 0) {
        utTerm();
        counter = 0;
        return true;
    }
    return false;
}


void Units::convert(const std::string& from, const std::string& to, double& slope, double& offset) throw(UnitException)
{
    LOG4FIMEX(logger, Logger::DEBUG, "convert from " << from << " to " << to);
	if (from == to) {
		slope = 1.;
		offset = 0.;
		return;
	}
	utUnit fromUnit, toUnit;
	handleUdUnitError(utScan(from.c_str(), &fromUnit), from);
	handleUdUnitError(utScan(to.c_str(), &toUnit), to);
	handleUdUnitError(utConvert(&fromUnit, &toUnit, &slope, &offset));
}

bool Units::areConvertible(const std::string& unit1, const std::string& unit2) const throw(UnitException)
{
    LOG4FIMEX(logger, Logger::DEBUG, "test convertibility of " << unit1 << " to " << unit2);
	utUnit fromUnit, toUnit;
	double slope, offset;
	handleUdUnitError(utScan(unit1.c_str(), &fromUnit), unit1);
	handleUdUnitError(utScan(unit2.c_str(), &toUnit), unit2);
	int error = utConvert(&fromUnit, &toUnit, &slope, &offset);
	switch (error) {
	case 0: return true;
	case UT_ECONVERT: return false;
	default: handleUdUnitError(error);
	}
	return false;
}
bool Units::isTime(const std::string& timeUnit) const throw(UnitException)
{
	utUnit unit;
	handleUdUnitError(utScan(timeUnit.c_str(), &unit), timeUnit);
	return utIsTime(&unit) != 0;
}

}
