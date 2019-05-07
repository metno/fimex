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

#include "fimex/FimexTime.h"

#include "fimex/TimeUtils.h"
#include "fimex/Utils.h"

#include <climits>
#include <cmath>
#include <limits>

namespace MetNoFimex {

static std::string twoDigits(int i)
{
    char digits[2] = {'0', '?'};
    if (i < 0 || i >= 100) {
        digits[0] = '?';
    } else if (i < 10) {
        digits[1] = '0' + (char)i;
    } else {
        digits[0] = '0' + (char)(i / 10);
        digits[1] = '0' + (char)(i % 10);
    }
    return std::string(digits, digits + 2);
}

void FimexTime::setTime(unsigned short year, char month, char mday, char hour, char minute, char second, unsigned short msecond)
{
    this->year = year;
    this->month = month;
    this->mday = mday;
    this->hour = hour;
    this->minute = minute;
    this->second = second;
    this->msecond = msecond;
}

FimexTime::FimexTime(unsigned short year, char month, char mday, char hour, char minute, char second, unsigned short msecond)
    : year(year)
    , month(month)
    , mday(mday)
    , hour(hour)
    , minute(minute)
    , second(second)
    , msecond(msecond)
{
}

FimexTime::FimexTime(special_values val)
{
    switch (val) {
    case min_date_time:
        setTime(0, CHAR_MIN, CHAR_MIN, CHAR_MIN, CHAR_MIN, CHAR_MIN, 0);
        break;
    case max_date_time:
        setTime(USHRT_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, USHRT_MAX);
        break;
    default:
        throw CDMException("unimplemented special_value: " + type2string(val));
    }
}

bool FimexTime::invalid() const
{
    return month <= 0 || month > 12;
}

bool FimexTime::parseISO8601(const std::string& isoStr)
{
    using namespace std;
    string trimStr = trim(isoStr);
    if (trimStr.size() == 0)
        return false;

    // T delimiter for date and time
    vector<string> dateTime = tokenize(trimStr, "T");
    if (dateTime.size() == 1) {
        // try space delimiter for date and time
        dateTime = tokenize(trimStr, " ");
    }

    // convert date and time 'int' vectors
    vector<string> time;
    vector<int> date;
    if (dateTime.size() == 1) {
        date = tokenizeDotted<int>(dateTime[0], "-");
        if (date.size() == 3) {
            // set time to zero
            time.push_back("0");
            time.push_back("0");
            time.push_back("0.0");
        } else {
            return false; // date required
        }
    } else if (dateTime.size() == 2) {
        date = tokenizeDotted<int>(dateTime[0], "-");
        time = tokenize(dateTime[1], ":");
    } else {
        return false;
    }

    if (time.size() == 2)
        time.push_back("0");
    // check date and time and set
    if ((date.size() == 3) && (time.size() == 3)) {
        vector<string> seconds = tokenize(time[2], ".");
        int milliSecs = 0;
        if (seconds.size() > 1) {
            milliSecs = std::round(string2type<double>("." + seconds[1]) * 1000);
        }
        setTime(date[0], date[1], date[2], string2type<short>(time[0]), string2type<short>(time[1]), string2type<short>(seconds[0]), milliSecs);
        return true;
    } else {
        return false;
    }
}

bool FimexTime::operator==(const FimexTime& rhs) const
{
    return year == rhs.year && month == rhs.month && mday == rhs.mday && hour == rhs.hour && minute == rhs.minute && second == rhs.second &&
           msecond == rhs.msecond;
}

std::ostream& operator<<(std::ostream& out, const FimexTime& fTime)
{
    out << fTime.getYear() << "-" << twoDigits(fTime.getMonth()) << "-" << twoDigits(fTime.getMDay());
    out << "T";
    out << twoDigits(fTime.getHour()) << ":" << twoDigits(fTime.getMinute()) << ":" << twoDigits(fTime.getSecond());
    if (fTime.getMSecond() > 0) {
        out << ".";
        if (fTime.getMSecond() < 10) {
            out << "00";
        } else if (fTime.getMSecond() < 100) {
            out << "0";
        }
        out << fTime.getMSecond();
    }
    return out;
}

FimexTime string2FimexTime(const std::string& str)
{
    FimexTime ft;
    if (!ft.parseISO8601(str)) {
        throw CDMException("string2FimexTime: date and time not found:" + str);
    }
    return ft;
}

FimexTime make_fimextime_utc_now()
{
    return fromTimePoint(make_time_utc_now());
}

std::string make_time_string_extended(const FimexTime& ft)
{
    std::ostringstream out;
    out << ft;
    return out.str();
}

std::string make_time_string(const FimexTime& fTime)
{
    std::ostringstream out;
    out << fTime.getYear() << twoDigits(fTime.getMonth()) << twoDigits(fTime.getMDay()) << "T" << twoDigits(fTime.getHour()) << twoDigits(fTime.getMinute())
        << twoDigits(fTime.getSecond());
    return out.str();
}

} // namespace MetNoFimex
