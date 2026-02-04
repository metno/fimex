/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

#ifndef FIMEX_TIMEUNIT_H
#define FIMEX_TIMEUNIT_H

#include "fimex/FimexTime.h"

#include <memory>
#include <string>

namespace MetNoFimex {

/**
 * @headerfile fimex/TimeUnit.h
 */

/**
 * TimeUnit calculates times from a time given in a unit as
 * of CF-1.0 (e.g. 'days since 2000-01-01 00:00:00') to a
 * unix time (i.e. 'seconds since 1970-01-01 00:00:00') or a
 * time struct #MetNoFimex::FimexTime
 *
 * All times are assumed to be UTC, and we use the Gregorian Calendar
 * (not 100% true for times before 1600AD, depending on implementation)
 *
 */
class TimeUnit
{
    std::shared_ptr<void> pUnit; // pointer to unit implementation
    double epochOffset;
    double epochSlope;
public:
    /// initialize a timeUnit with a unit string
    TimeUnit();
    TimeUnit(const std::string& timeUnitString);
    virtual ~TimeUnit();

    /// calculate the epochSeconds for a time in the current unit
    double unitTime2epochSeconds(double unitTime) const;

    /// calculate the time in the current unit from the epoch
    double epochSeconds2unitTime(double epochSeconds) const;

    /// calculate the time in a calendar form
    FimexTime unitTime2fimexTime(double unitTime) const;

    /*! calculate the time in the current unit from the calendar form
     * \returns NaN for invalid times
     */
    double fimexTime2unitTime(const FimexTime& fiTime) const;

    /*! calculate the time in the current unit from the calendar form
     * \returns invalidValue for invalid times
     */
    double fimexTime2unitTime(const FimexTime& fiTime, double invalidValue) const;

private:
    void init(const std::string& timeUnitString = "seconds since 1970-01-01 00:00:00");
};

} // namespace MetNoFimex

#endif /* FIMEX_TIMEUNIT_H */
