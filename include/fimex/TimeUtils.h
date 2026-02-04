/*
  Fimex, include/fimex/TimeUtils.h

  Copyright (C) 2019-2026 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://github.com/metno/fimex/wiki

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


#ifndef FIMEX_TIMEUTILS_H
#define FIMEX_TIMEUTILS_H

#include "fimex/TimeUnit.h"

#include <chrono>
#include <string>

namespace MetNoFimex {

using time_duration = std::chrono::seconds;
using time_point = std::chrono::time_point<std::chrono::system_clock, time_duration>;

time_point make_time_point(int year, int month, int day, int hour, int minute, int second);

time_point make_time_from_string(const std::string& iso9601);
time_point make_time_from_timet(long timet);
time_point make_time_utc_now();
time_point make_invalid_time_point();

std::string make_time_string(const time_point& tp);
std::string make_time_string_extended(const time_point& tp);

bool is_invalid_time_point(const time_point& tp);

time_point asTimePoint(const FimexTime& ft);
FimexTime fromTimePoint(const time_point& tp);

typedef long epoch_seconds;

/**
 * convert a c++ time point to seconds sinc 1970-01-01
 * @param time time to convert
 */
epoch_seconds timePoint2epochTime(const time_point& time);
epoch_seconds fimexTime2epochTime(const FimexTime& time);

} // namespace MetNoFimex

#endif // FIMEX_TIMEUTILS_H
