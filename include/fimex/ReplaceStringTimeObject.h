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

#ifndef REPLACESTRINGTIMEOBJECT_H_
#define REPLACESTRINGTIMEOBJECT_H_

#include "fimex/ReplaceStringObject.h"
#include <ctime>

namespace MetNoFimex
{

/**
 * @headerfile fimex/ReplaceStringTimeObject.h
 */
class ReplaceStringTimeObject : public ReplaceStringObject
{
    std::time_t myTime;
    std::string myFormat;
    std::time_t offset;
public:
    /**
     * initialize a ReplaceStringTimeObject with time and string set
     */
    explicit ReplaceStringTimeObject(std::time_t time, const std::string& format = "%Y-%m-%d %H:%M:%S%F%Q")
        : myTime(time)
        , myFormat(format)
        , offset(0)
    {
    }

    ~ReplaceStringTimeObject();

    friend std::ostream& operator<<(std::ostream& s, const ReplaceStringTimeObject& rsto);
    std::ostream& put(std::ostream& s) const override
    {
        s << *this;
        return s;
    }

    /**
     *  set the formatting String for this object
     *
     * @param format: format string of strftime http://www.cplusplus.com/reference/clibrary/ctime/strftime.html
     */
    void setFormatString(const std::string& format) override { myFormat = format; }

    /**
     * set the formatting string and additional options for this object
     * options are: 0: offset as in seconds, i.e. +5000, -6000
     */
    void setFormatStringAndOptions(const std::string& format, const std::vector<std::string>& options) override;
};

} // namespace MetNoFimex

#endif /*REPLACESTRINGTIMEOBJECT_H_*/
