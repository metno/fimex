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

#ifndef UNITS_H_
#define UNITS_H_

#include <string>
#include <boost/noncopyable.hpp>
#include "fimex/CDMException.h"
namespace MetNoFimex
{

class UnitException : public CDMException
{
public:
	UnitException() {}
	UnitException(std::string message) : CDMException(message) {}
};

class Units : public boost::noncopyable
{
	static int counter;
public:
	/** initialization of unit handling, i.e. parsing of unit file etc if required */
	Units();
	virtual ~Units();
	/**
	 * calculate the linear unit conversion: newVal (in to unit) = oldVal (in from unit) * slope + offset 
	 * @param from unit
	 * @param to unit
	 * @param slope return value of the slope
	 * @param offset return value of the offset
	 */
	void convert(const std::string& from, const std::string& to, double* slope, double* offset) throw(UnitException);
};

}

#endif /*UNITS_H_*/
