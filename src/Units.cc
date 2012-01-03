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

#include "boost/shared_ptr.hpp"
#ifdef _OPENMP
#include <omp.h>
#endif
#include <MutexLock.h>
#include "../config.h"
#ifdef HAVE_UDUNITS2_H
#include "udunits2.h"
#include "converter.h"
#else // UDUNITS1
extern "C" {
#include "udunits.h"
}
// add forgotten utIsInit fom udunits
extern "C" int utIsInit();
#endif // UDUNITS2

namespace MetNoFimex
{
#ifdef HAVE_UDUNITS2_H
static ut_system* utSystem;
#endif

static MutexType unitsMutex;
extern MutexType& getUnitsMutex() {
    return unitsMutex;
}

static LoggerPtr logger = getLogger("fimex.Units");

void handleUdUnitError(int unitErrCode, const std::string& message) throw(UnitException)
{
    switch (unitErrCode) {
#ifdef HAVE_UDUNITS2_H
    case UT_SUCCESS: break;
    case UT_BAD_ARG:  throw UnitException("An argument violates the function's contract: " + message);
    case UT_EXISTS: throw UnitException("Unit, prefix, or identifier already exists: " + message);
    case UT_NO_UNIT: throw UnitException("No such unit exists: " + message);
    case UT_OS: throw UnitException("Operating-system error: " + message);
    case UT_NOT_SAME_SYSTEM: throw UnitException("The units belong to different unit-systems: " + message);
    case UT_MEANINGLESS: throw UnitException("The operation on the unit(s) is meaningless: " + message);
    case UT_NO_SECOND: throw UnitException("The unit-system doesn't have a unit named 'second': " + message);
    case UT_VISIT_ERROR: throw UnitException("An error occurred while visiting a unit: " + message);
    case UT_CANT_FORMAT: throw UnitException("A unit can't be formatted in the desired manner: " + message);
    case UT_SYNTAX: throw UnitException("string unit representation contains syntax error: " + message);
    case UT_UNKNOWN: throw UnitException("string unit representation contains unknown word: " + message);
    case UT_OPEN_ARG: throw UnitException("Can't open argument-specified unit database: " + message);
    case UT_OPEN_ENV: throw UnitException("Can't open environment-specified unit database: " + message);
    case UT_OPEN_DEFAULT: throw UnitException("Can't open installed, default, unit database: " + message);
    case UT_PARSE: throw UnitException("Error parsing unit specification: " + message);
#else // udunits1
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
#endif // UDUNITS2
    default: throw UnitException("unknown error");
    }
}

Units::Units()
{
    ScopedCritical lock(unitsMutex);
#ifdef HAVE_UDUNITS2_H
    if (utSystem == 0) {
        ut_set_error_message_handler(&ut_ignore);
        utSystem = ut_read_xml(0);
        handleUdUnitError(ut_get_status());
    }
#else
	if (!utIsInit()) {
		handleUdUnitError(utInit(0));
	}
#endif
}

Units::Units(const Units& u)
{}

Units& Units::operator=(const Units& rhs)
{
	return *this; // no state! no increase/decrease to counter required
}

Units::~Units()
{}

bool Units::unload(bool force) throw(UnitException)
{
    bool retVal = false;
    ScopedCritical lock(unitsMutex);
    if (force) {
#ifdef HAVE_UDUNITS2_H
        ut_free_system(utSystem);
#else
        utTerm();
#endif
        retVal = true;
    }

    return retVal;
}


void Units::convert(const std::string& from, const std::string& to, double& slope, double& offset) throw(UnitException)
{
    LOG4FIMEX(logger, Logger::DEBUG, "convert from " << from << " to " << to);
	if (from == to) {
		slope = 1.;
		offset = 0.;
		return;
	}
	ScopedCritical lock(unitsMutex);
#ifdef HAVE_UDUNITS2_H
	boost::shared_ptr<ut_unit> fromUnit(ut_parse(utSystem, from.c_str(), UT_UTF8), ut_free);
	handleUdUnitError(ut_get_status(), from);
	boost::shared_ptr<ut_unit> toUnit(ut_parse(utSystem, to.c_str(), UT_UTF8), ut_free);
	handleUdUnitError(ut_get_status(), to);
	boost::shared_ptr<cv_converter> conv(ut_get_converter(fromUnit.get(), toUnit.get()), cv_free);
    handleUdUnitError(ut_get_status(), from + " -> " + to);
    offset = cv_convert_double(conv.get(), 0.0);
    slope = cv_convert_double(conv.get(), 1.0) - offset;
#else
	utUnit fromUnit, toUnit;
	handleUdUnitError(utScan(from.c_str(), &fromUnit), from);
	handleUdUnitError(utScan(to.c_str(), &toUnit), to);
	handleUdUnitError(utConvert(&fromUnit, &toUnit, &slope, &offset));
#endif
}

bool Units::areConvertible(const std::string& unit1, const std::string& unit2) const
{
    LOG4FIMEX(logger, Logger::DEBUG, "test convertibility of " << unit1 << " to " << unit2);
    int areConv = 0;
    ScopedCritical lock(unitsMutex);
#ifdef HAVE_UDUNITS2_H
    try {
        boost::shared_ptr<ut_unit> fromUnit(ut_parse(utSystem, unit1.c_str(), UT_UTF8), ut_free);
        handleUdUnitError(ut_get_status(), unit1);
        boost::shared_ptr<ut_unit> toUnit(ut_parse(utSystem, unit2.c_str(), UT_UTF8), ut_free);
        handleUdUnitError(ut_get_status(), unit2);
        areConv = ut_are_convertible(fromUnit.get(), toUnit.get());
    } catch (UnitException& ue) {
        LOG4FIMEX(logger, Logger::WARN, ue.what());
    }
#else
	utUnit fromUnit, toUnit;
	double slope, offset;
	handleUdUnitError(utScan(unit1.c_str(), &fromUnit), unit1);
	handleUdUnitError(utScan(unit2.c_str(), &toUnit), unit2);
	int error = utConvert(&fromUnit, &toUnit, &slope, &offset);
	switch (error) {
	case 0: areConv = 1; break;
	case UT_ECONVERT: areConv = 0; break;
	default: handleUdUnitError(error);
	}
#endif

	return areConv;
}
bool Units::isTime(const std::string& timeUnit) const
{
#ifdef HAVE_UDUNITS2_H
    return areConvertible(timeUnit, "seconds since 1970-01-01 00:00:00");
#else
    bool isTime = false;
    ScopedCritical lock(unitsMutex);
	utUnit unit;
	handleUdUnitError(utScan(timeUnit.c_str(), &unit), timeUnit);
	isTime = (utIsTime(&unit) != 0);
    return isTime;
#endif
}

const void* Units::exposeInternals() const {
#ifdef HAVE_UDUNITS2_H
    return utSystem;
#else
    return 0;
#endif
}

}
