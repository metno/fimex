/*
 * Fimex
 *
 * (C) Copyright 2008-2022, met.no
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

#include "fimex/TimeUnit.h"

#include "fimex/CDMException.h"
#include "fimex/MutexLock.h"
#include "fimex/TimeUtils.h"
#include "fimex/Type2String.h"
#include "fimex/Units.h"

#include <climits>
#include <cmath>
#include <limits>

#include "udunits2.h"
#include "converter.h"

// TODO: make thread-safe with mifi_units lock

namespace MetNoFimex
{
/// only use for internals, exported from Units.cc
extern OmpMutex& getUnitsMutex();

static void void_ut_free(void* ptr)
{
    ut_free(reinterpret_cast<ut_unit*>(ptr));
}

void TimeUnit::init(const std::string& timeUnitString)
{
    Units units; // unit initialization
    if (!units.isTime(timeUnitString)) {
        throw CDMException("trying to initialize time with wrong unit: "+timeUnitString);
    } else {
        units.convert(timeUnitString, "seconds since 1970-01-01 00:00:00 +00:00", epochSlope, epochOffset);
        OmpScopedLock lock(getUnitsMutex());
        pUnit = std::shared_ptr<void>(
            reinterpret_cast<void*>(ut_parse(reinterpret_cast<const ut_system*>(units.exposeInternals()), timeUnitString.c_str(), UT_UTF8)), void_ut_free);
        handleUdUnitError(ut_get_status(), "parsing " + timeUnitString);
    }
}

TimeUnit::TimeUnit()
{
    init();
}
TimeUnit::TimeUnit(const std::string& timeUnit)
{
    init(timeUnit);
}

TimeUnit::~TimeUnit()
{
}

double TimeUnit::unitTime2epochSeconds(double unitTime) const
{
    return unitTime * epochSlope + epochOffset;
}

double TimeUnit::epochSeconds2unitTime(double epochSeconds) const
{
    return (epochSeconds - epochOffset) / epochSlope;
}

FimexTime TimeUnit::unitTime2fimexTime(double unitTime) const
{
    Units units; // unit initialization
    FimexTime fiTime;
    float second;
    int year, month, mday, hour, minute;
    OmpScopedLock lock(getUnitsMutex());
    std::shared_ptr<ut_unit> baseTime(ut_get_unit_by_name(reinterpret_cast<const ut_system*>(units.exposeInternals()), "second"), ut_free);
    handleUdUnitError(ut_get_status(), "parsing unit seconds");
    std::shared_ptr<cv_converter> conv(ut_get_converter(reinterpret_cast<ut_unit*>(pUnit.get()), baseTime.get()), cv_free);
    handleUdUnitError(ut_get_status(), "converting double to calendar");
    double encodedTime = cv_convert_double(conv.get(), unitTime);
    double res, sec;
    ut_decode_time(encodedTime, &year, &month, &mday, &hour, &minute, &sec, &res);
    second = static_cast<float>(sec);
    fiTime.setYear(static_cast<unsigned int>(year));
    fiTime.setMonth(static_cast<char>(month));
    fiTime.setMDay(static_cast<char>(mday));
    fiTime.setHour(static_cast<char>(hour));
    fiTime.setMinute(static_cast<char>(minute));
    fiTime.setSecond(static_cast<char>(second));
    fiTime.setMSecond(static_cast<unsigned int>((second - fiTime.getSecond())*1000));

    return fiTime;
}

double TimeUnit::fimexTime2unitTime(const FimexTime& fiTime, double invalidValue) const
{
    if (fiTime.invalid()) {
        if (fiTime == FimexTime(FimexTime::min_date_time))
            return std::numeric_limits<double>::lowest();
        else if (fiTime == FimexTime(FimexTime::max_date_time))
            return std::numeric_limits<double>::max();
        return invalidValue;
    }

    Units units; // unit initialization
    float second = fiTime.getSecond() + (fiTime.getMSecond()/1000.);
    double unitTime;
    OmpScopedLock lock(getUnitsMutex());
    std::shared_ptr<ut_unit> baseTime(ut_get_unit_by_name(reinterpret_cast<const ut_system*>(units.exposeInternals()), "second"), ut_free);
    double encodedTime = ut_encode_time(fiTime.getYear(), fiTime.getMonth(), fiTime.getMDay(), fiTime.getHour(), fiTime.getMinute(), second);
    handleUdUnitError(ut_get_status(), "encoding fimexTime");
    std::shared_ptr<cv_converter> conv(ut_get_converter(baseTime.get(), reinterpret_cast<ut_unit*>(pUnit.get())), cv_free);
    handleUdUnitError(ut_get_status(), "converter calendar to double");
    unitTime = cv_convert_double(conv.get(), encodedTime);
    handleUdUnitError(ut_get_status(), "converting calendar to double");
    return unitTime;
}

double TimeUnit::fimexTime2unitTime(const FimexTime& fiTime) const
{
    return fimexTime2unitTime(fiTime, std::nan(""));
}

} // namespace MetNoFimex
