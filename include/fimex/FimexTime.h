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

#ifndef FIMEX_FIMEXTIME_H
#define FIMEX_FIMEXTIME_H

#include <iosfwd>
#include <memory>

namespace MetNoFimex {

/**
 * @headerfile fimex/TimeUnit.h
 */
/**
 * time representation and some overloaded operators
 *
 * @warning: the implementor needs to make sure, that all values are
 * given correctly, i.e. seconds between 0 and 59
 */
class FimexTime
{
private:
    /// year (2008 as of writing)
    unsigned short year;
    /// month (1-12)
    char month;
    /// day of month (1-31)
    char mday;
    /// hour (0-23)
    char hour;
    /// minute (0-59)
    char minute;
    /// second (0-59)
    char second;
    /// millisecond
    unsigned short msecond;

public:
    enum special_values { min_date_time, max_date_time };
    FimexTime(unsigned short year, char month, char mday, char hour = 0, char minute = 0, char second = 0, unsigned short msecond = 0);
    FimexTime(special_values val = min_date_time);
    /**
     *  parse and set the time in ISO8601 formats (not all), e.g. YYYY-MM-DD, HH:MM:SS, YYYY-MM-DD HH:MM:SS, YYYY-MM-DDTHH:MM:SS
     *  (and without seconds)
     *
     *  @return true, if time/date has been set, false otherwise
     */
    bool parseISO8601(const std::string& isoString);
    bool invalid() const;

    /// set all the time-parameters at once
    void setTime(unsigned short year, char month, char mday, char hour = 0, char minute = 0, char second = 0, unsigned short msecond = 0);
    /// year (2008 as of writing)
    unsigned short getYear() const { return year; }
    void setYear(unsigned short year) { this->year = year; }
    /// month (1-12)
    char getMonth() const { return month; }
    void setMonth(char month) { this->month = month; }
    /// day of month (1-31)
    char getMDay() const { return mday; }
    void setMDay(char mday) { this->mday = mday; }
    /// hour (0-23)
    char getHour() const { return hour; }
    void setHour(char hour) { this->hour = hour; }
    /// minute (0-59)
    char getMinute() const { return minute; }
    void setMinute(char minute) { this->minute = minute; }
    /// second (0-59)
    char getSecond() const { return second; }
    void setSecond(char second) { this->second = second; }
    /// millisecond
    unsigned short getMSecond() const { return msecond; }
    void setMSecond(unsigned short msecond) { this->msecond = msecond; }

    /// compare two fimexTimes
    bool operator==(const FimexTime& rhs) const;
    /// compare two fimexTimes
    bool operator!=(const FimexTime& rhs) const { return !(*this == rhs); }
    /// compare two fimexTimes
    bool operator>(const FimexTime& rhs) const { return (toLong() > rhs.toLong()); }
    /// compare two fimexTimes
    bool operator<(const FimexTime& rhs) const { return (rhs > *this); }
    /// compare two fimexTimes
    bool operator>=(const FimexTime& rhs) const { return !(rhs > *this); }
    /// compare two fimexTimes
    bool operator<=(const FimexTime& rhs) const { return !(*this > rhs); }

private:
    /// this representation can be used for comparison (==, <, >) not for calculation
    long long toLong() const
    {
        return year * 10000000000000LL + month * 100000000000LL + mday * 1000000000LL + hour * 10000000LL + minute * 100000 + second * 1000 + msecond;
    }
};

std::ostream& operator<<(std::ostream& out, const FimexTime& fTime);

FimexTime string2FimexTime(const std::string& str);
FimexTime make_fimextime_utc_now();

std::string make_time_string_extended(const FimexTime& ft);
std::string make_time_string(const FimexTime& ft);
inline bool is_invalid_time_point(const FimexTime& ft)
{
    return ft.invalid();
}

} // namespace MetNoFimex

#endif /* FIMEX_FIMEXTIME_H */
