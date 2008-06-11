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

#include "Units.h"
#include "config.h"
#if HAVE_UDUNITS
extern "C" {
#include "udunits.h"
}
// add forgotten utIsInit fom udunits
extern "C" int utIsInit();
#endif

namespace MetNoFimex
{
static void handleUnitError(int unitErrCode, const std::string& message = "") throw(UnitException)
{
	switch (unitErrCode) {
	case 0: break;
#if HAVE_UDUNITS
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
#endif
	default: throw UnitException("unknown error");
	}
}
int Units::counter = 0;
Units::Units()
{
#if HAVE_UDUNITS
	if (!utIsInit()) {
		handleUnitError(utInit(0));
	}
#endif
	++counter;
}

Units::~Units()
{
	if (--counter == 0) {
#if HAVE_UDUNITS
		utTerm();
#endif
	}
}

void Units::convert(const std::string& from, const std::string& to, double* slope, double* offset) throw(UnitException)
{
	if (from == to) {
		*slope = 1.;
		*offset = 0.;
	}
#if HAVE_UDUNITS
	utUnit fromUnit, toUnit;
	handleUnitError(utScan(from.c_str(), &fromUnit), from);
	handleUnitError(utScan(to.c_str(), &toUnit), to);
	handleUnitError(utConvert(&fromUnit, &toUnit, slope, offset));
#else
	throw UnitException("fimex not compiled with udunits support");
#endif
}

}
