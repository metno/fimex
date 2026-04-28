/*
 * Fimex, LogTimer.cc
 *
 * (C) Copyright 2024-2026, met.no
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

#include "LogTimer.h"

#include <sys/time.h>

namespace MetNoFimex {

namespace { // anonymous

double seconds(const struct timeval& t)
{
    return t.tv_sec + t.tv_usec / 1e6;
}

double seconds_now()
{
    struct timeval now;
    gettimeofday(&now, 0);
    return seconds(now);
}

} // namespace

LogTimer::LogTimer(Logger_p logger, const char* scopename)
    : logger_(logger)
    , scopename_(scopename)
    , time_(seconds_now())
{
}

LogTimer::~LogTimer()
{
    const double duration = seconds_now() - time_;
    LOG4FIMEX(logger_, Logger::DEBUG, "spent " << duration << "s in " << scopename_);
}

} // namespace MetNoFimex
