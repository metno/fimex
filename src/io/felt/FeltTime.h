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

#ifndef FELT_FELTTIME_H
#define FELT_FELTTIME_H

#include "FeltConstants.h"

namespace felt {

struct FeltTime
{
    int year;
    int month;
    int day;
    int hour;
    int minute;
    int second;
    bool invalid() const { return month <= 0; }
    FeltTime& addHours(int hours);
};

FeltTime parseTime(const word* data);

FeltTime parseTimeNoThrow(const word* data);

} // namespace felt

#endif /*FELT_FELTTIME_H*/
