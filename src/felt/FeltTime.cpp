/*
 wdb

 Copyright (C) 2007-2019 met.no

 Contact information:
 Norwegian Meteorological Institute
 Box 43 Blindern
 0313 OSLO
 NORWAY
 E-mail: wdb@met.no
 
 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, 
 MA  02110-1301, USA
 */

#include "felt/FeltTime.h"

#include <date/date.h>

#include <stdexcept>

namespace felt {

using time_duration = std::chrono::seconds;
using time_point = std::chrono::time_point<std::chrono::system_clock, time_duration>;

FeltTime& FeltTime::addHours(int add_hours)
{
    date::year_month_day d = date::year(year) / date::month(month) / date::day(day);
    time_point tp = date::sys_days(d) + std::chrono::hours(hour) + std::chrono::minutes(minute) + std::chrono::seconds(second);

    tp += std::chrono::hours(add_hours);

    const time_t tt = std::chrono::system_clock::to_time_t(tp);
    const tm utc = *std::gmtime(&tt);
    year = 1900 + utc.tm_year;
    month = 1 + utc.tm_mon;
    day = utc.tm_mday;
    hour = utc.tm_hour;
    minute = utc.tm_min;
    second = utc.tm_sec;

    return *this;
}

FeltTime parseTime(const word data[3])
{
    return FeltTime { data[0], data[1] / 100, data[1] % 100, data[2] / 100, data[2] % 100, 0 };
}

FeltTime parseTimeNoThrow(const word* data)
{
    try {
        return parseTime(data);
    } catch (std::exception&) {
        return FeltTime { 0, -1, -1, -1, -1, -1 };
    }
}

} // namespace felt
