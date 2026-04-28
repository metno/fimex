/*
 * Fimex, LogTimer.h
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

#ifndef FIMEX_LOGTIMER_H_
#define FIMEX_LOGTIMER_H_

#include "fimex/Logger.h"

namespace MetNoFimex {

class LogTimer
{
public:
    LogTimer(Logger_p logger, const char* scopename);
    ~LogTimer();

private:
    Logger_p logger_;
    const char* scopename_;
    double time_;
};

} // namespace MetNoFimex

#define LOGTIMER LogTimer lt__(logger, __PRETTY_FUNCTION__)
#define LOGTIMER_MSG(x) LogTimer lt__(logger, x)

#endif /* FIMEX_LOGTIMER_H_ */
