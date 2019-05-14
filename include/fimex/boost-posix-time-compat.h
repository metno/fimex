/*
  Fimex, include/fimex/boost-posix-time-compat.h

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

#ifndef FIMEX_BOOST_POSIX_TIME_COMPAT_H
#define FIMEX_BOOST_POSIX_TIME_COMPAT_H

#include <fimex/FimexTime.h>
#include <boost/date_time/posix_time/posix_time_types.hpp>

namespace MetNoFimex {

//! convert FimexTime  to boost::posix_time::ptime with 1s resolution
inline boost::posix_time::ptime fromFimexTime(const FimexTime& ft)
{
    namespace bp = boost::posix_time;
    namespace bg = boost::gregorian;
    if (ft.invalid())
        return bp::ptime();
    return bp::ptime(bg::date((int)ft.getYear(), (int)ft.getMonth(), (int)ft.getMDay()),
            bp::time_duration((int)ft.getHour(), (int)ft.getMinute(), (int)ft.getSecond()));
}

//! convert boost::posix_time::ptime to FimexTime with 1s resolution
inline FimexTime toFimexTime(const boost::posix_time::ptime& bpt)
{
    namespace bp = boost::posix_time;
    namespace bg = boost::gregorian;
    if (bpt.is_not_a_date_time())
        return FimexTime();
    const bg::date& d = bpt.date();
    const bp::time_duration& t = bpt.time_of_day();
    return FimexTime(d.year(), d.month(), d.day(), t.hours(), t.minutes(), t.seconds());
}

} // namespace MetNoFimex

#endif //FIMEX_BOOST_POSIX_TIME_COMPAT_H
