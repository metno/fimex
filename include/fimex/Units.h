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
#include "fimex/CDMException.h"
#include "fimex/UnitsConverter.h"
#include <boost/shared_ptr.hpp>

namespace MetNoFimex
{

/**
 * @headerfile fimex/Units.h
 */
class UnitException : public CDMException
{
public:
    UnitException() {}
    UnitException(std::string message) : CDMException(message) {}
};

/**
 * @headerfile fimex/Units.h
 */

/**
 * The class Units describes a units-system, not a single unit. Different units
 * can be compared and converted if comparable within the system.
 */
class Units
{
    friend class TimeUnit;
public:
    /**
     * initialization of unit handling, i.e. parsing of unit file etc if required
     * the unit file is installation-dependent on the underlying units-package (udunits or udunits2)
     * and can be controlled through UDUNITS_PATH environment
     */
    Units();
    Units(const Units& rhs);
    Units& operator=(const Units& rhs);
    virtual ~Units();
    /**
     * calculate the linear unit conversion: newVal (in to unit) = oldVal (in from unit) * slope + offset
     * @param from unit
     * @param to unit
     * @param slope return value of the slope
     * @param offset return value of the offset
     * @throw UnitException
     * @warning The slope and offset are only useful when the units are linearly convertible.
     */
    void convert(const std::string& from, const std::string& to, double& slope, double& offset);
    /**
     * Get a UnitsConverter which translates from 'from' unit to 'to' unit.
     * @param from
     * @param to
     * @return a UnitsConverter object
     * @throw  UnitException
     */
    boost::shared_ptr<UnitsConverter> getConverter(const std::string& from, const std::string& to);
    /**
     * @brief test if two units are convertible to each others
     * @param unit1 first unit
     * @param unit2 second unit
     */
    bool areConvertible(const std::string& unit1, const std::string& unit2) const;
    /**
     * @brief test if unit is a time
     * @param timeUnit
     */
    bool isTime(const std::string& timeUnit) const;
    /**
     * Units initialize themselve on first using the default unix-file path
     * and keep the internal datastructure until the end of the program, or
     * this function is used.
     *
     * @param force unload units-setup, even if some objects exist, defaults to false
     * @return true if unloaded, false if there are still some objects using the
     * internal data-structure.
     */
    static bool unload(bool force = false) throw(UnitException);
private:
    /**
     * expose the internals of the implementation as a void*
     * you need to be sure that you know the internals!
     *
     * Needed in TimeUnit.
     */
    const void* exposeInternals() const;
};

void handleUdUnitError(int unitErrCode, const std::string& message = "") throw(UnitException);

}

#endif /*UNITS_H_*/
