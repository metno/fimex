/*
  Fimex, src/TimeUtils.cc

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/


#include "fimex/TimeUtils.h"

#include "fimex/TimeUnit.h"

#include <date/date.h>

#include <cstdio>

namespace MetNoFimex {

time_point make_time_point(int year, int month, int day, int hour, int minute, int second)
{
    date::year_month_day d = date::year(year) / date::month(month) / date::day(day);
    return date::sys_days(d) + std::chrono::hours(hour) + std::chrono::minutes(minute) + std::chrono::seconds(second);
}

time_point make_invalid_time_point()
{
    return time_point::max();
}

bool is_invalid_time_point(const time_point& tp)
{
    return tp == make_invalid_time_point();
}

time_point make_time_from_string(const std::string& iso9601)
{
    FimexTime ft;
    if (ft.parseISO8601(iso9601))
        return asTimePoint(ft);
    else
        return make_invalid_time_point();
}

time_point make_time_from_timet(long timet)
{
    std::chrono::system_clock::time_point t = std::chrono::system_clock::from_time_t(static_cast<std::time_t>(timet));
    return std::chrono::time_point_cast<std::chrono::seconds>(t);
}

static std::string make_time_string(const time_point& tp, const char* fmt)
{
    if (is_invalid_time_point(tp))
        return "invalid-date-time";

    const time_t tt = std::chrono::system_clock::to_time_t(tp);
    const tm utc = *std::gmtime(&tt);
    char buf[64];
    snprintf(buf, sizeof(buf), fmt, 1900 + utc.tm_year, 1 + utc.tm_mon, utc.tm_mday, utc.tm_hour, utc.tm_min, utc.tm_sec);
    return std::string(buf);
}

std::string make_time_string(const time_point& tp)
{
    static const char fmt[] = "%04d%02d%02dT%02d%02d%02d";
    return make_time_string(tp, fmt);
}

std::string make_time_string_extended(const time_point& tp)
{
    static const char fmt[] = "%04d-%02d-%02dT%02d:%02d:%02d";
    return make_time_string(tp, fmt);
}

time_point make_time_utc_now()
{
#if __cplusplus >= 202001L // c++ 20 has utc clock
    std::chrono::utc_clock::time_point tp = std::chrono::utc_clock::now();
    return std::chrono::time_point_cast<std::chrono::seconds>(tp);
#else
    const time_t tt = time(0);
    tm utc = *std::gmtime(&tt);
    return make_time_point(1900 + utc.tm_year, 1 + utc.tm_mon, utc.tm_mday, utc.tm_hour, utc.tm_min, utc.tm_sec);
#endif
}

time_point asTimePoint(const FimexTime& ft)
{
    if (ft.invalid())
        return make_invalid_time_point();

    return make_time_point(ft.getYear(), ft.getMonth(), ft.getMDay(), ft.getHour(), ft.getMinute(), static_cast<int>(ft.getSecond()))
        // + std::chrono::milliseconds(ft.getMSecond())
        ;
}

FimexTime fromTimePoint(const time_point& tp)
{
    if (is_invalid_time_point(tp))
        return FimexTime();

    const time_t tt = std::chrono::system_clock::to_time_t(tp);
    const tm utc = *std::gmtime(&tt);
    return FimexTime(1900 + utc.tm_year, 1 + utc.tm_mon, utc.tm_mday, utc.tm_hour, utc.tm_min, utc.tm_sec);
}

epoch_seconds timePoint2epochTime(const time_point& time)
{
    return (time - date::sys_days(date::January / 1 / 1970)).count();
}

epoch_seconds fimexTime2epochTime(const FimexTime& time)
{
    return timePoint2epochTime(asTimePoint(time));
}

} // namespace MetNoFimex
