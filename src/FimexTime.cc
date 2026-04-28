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

#include "fimex/FimexTime.h"

#include "fimex/StringUtils.h"
#include "fimex/TimeUtils.h"
#include "fimex/TokenizeDotted.h"

#include <climits>
#include <cmath>
#include <limits>
#include <vector>

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

void FimexTime::setTime(unsigned short year, unsigned char month, unsigned char mday, unsigned char hour, unsigned char minute, unsigned char second,
                        unsigned short msecond)
{
    this->year = year;
    this->month = month;
    this->mday = mday;
    this->hour = hour;
    this->minute = minute;
    this->second = second;
    this->msecond = msecond;
}

FimexTime::FimexTime(unsigned short year, unsigned char month, unsigned char mday, unsigned char hour, unsigned char minute, unsigned char second,
                     unsigned short msecond)
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
        setTime(0, 0, 0, 0, 0, 0, 0);
        break;
    case max_date_time:
        setTime(USHRT_MAX, UCHAR_MAX, UCHAR_MAX, UCHAR_MAX, UCHAR_MAX, UCHAR_MAX, USHRT_MAX);
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

namespace {

// try to use multiples of 4 to make it possible (if hard) to read in hex
static const size_t WIDTH_MSECOND = 16;
static const size_t WIDTH_SECOND = 8;
static const size_t WIDTH_MINUTE = 8;
static const size_t WIDTH_HOUR = 8;
static const size_t WIDTH_MDAY = 8;
static const size_t WIDTH_MONTH = 4;
static const size_t WIDTH_YEAR = 12;

static const size_t SHIFT_SECOND = WIDTH_MSECOND;
static const size_t SHIFT_MINUTE = SHIFT_SECOND + WIDTH_SECOND;
static const size_t SHIFT_HOUR = SHIFT_MINUTE + WIDTH_MINUTE;
static const size_t SHIFT_MDAY = SHIFT_HOUR + WIDTH_HOUR;
static const size_t SHIFT_MONTH = SHIFT_MDAY + WIDTH_MDAY;
static const size_t SHIFT_YEAR = SHIFT_MONTH + WIDTH_MONTH;
static_assert(SHIFT_YEAR + WIDTH_YEAR <= 64, "oversized FimexTime encoding");

static const size_t MASK_MSECOND = ((1 << WIDTH_MSECOND) - 1);
static const size_t MASK_SECOND = ((1 << WIDTH_SECOND) - 1);
static const size_t MASK_MINUTE = ((1 << WIDTH_MINUTE) - 1);
static const size_t MASK_HOUR = ((1 << WIDTH_HOUR) - 1);
static const size_t MASK_MDAY = ((1 << WIDTH_MDAY) - 1);
static const size_t MASK_MONTH = ((1 << WIDTH_MONTH) - 1);
static const size_t MASK_YEAR = ((1 << WIDTH_YEAR) - 1);

} // namespace

unsigned long long FimexTime::toEncoded() const
{
    // clang-format off
    return (uint64_t(year)  << SHIFT_YEAR)
         | (uint64_t(month) << SHIFT_MONTH)
         | (uint64_t(mday) << SHIFT_MDAY)
         | (uint64_t(hour) << SHIFT_HOUR)
         | (minute << SHIFT_MINUTE)
         | (second << SHIFT_SECOND)
         | msecond;
    // clang-format on
}

/// this representation can be used for comparison (==, <, >) not for calculation
FimexTime FimexTime::fromEncoded(unsigned long long enc)
{
    const unsigned short year = (enc >> SHIFT_YEAR) & MASK_YEAR;
    const unsigned char month = (enc >> SHIFT_MONTH) & MASK_MONTH;
    const unsigned char mday = (enc >> SHIFT_MDAY) & MASK_MDAY;
    const unsigned char hour = (enc >> SHIFT_HOUR) & MASK_HOUR;
    const unsigned char minute = (enc >> SHIFT_MINUTE) & MASK_MINUTE;
    const unsigned char second = (enc >> SHIFT_SECOND) & MASK_SECOND;
    const unsigned short msecond = enc & MASK_MSECOND;
    return FimexTime(year, month, mday, hour, minute, second, msecond);
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
