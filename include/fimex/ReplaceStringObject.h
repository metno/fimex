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

#ifndef REPLACESTRINGOBJECT_H_
#define REPLACESTRINGOBJECT_H_

#include <iosfwd>
#include <string>
#include <vector>

namespace MetNoFimex {

/**
 * @headerfile fimex/ReplaceStringObject.h
 */
/**
 * Interface for objects which might be converted to
 * different strings
 */
class ReplaceStringObject
{
public:
    virtual ~ReplaceStringObject() = 0;

    /**
     *  put the formatted string to the stream
     *
     * implementors are asked to implement operator<<
     */
    virtual std::ostream& put(std::ostream& s) const = 0;

    /// set the formatting string for this object
    virtual void setFormatString(const std::string& format) = 0;

    /// set the formatting string and additional options for this object
    virtual void setFormatStringAndOptions(const std::string& format, const std::vector<std::string>& options) = 0;
};

} // namespace MetNoFimex

#endif /*REPLACESTRINGOBJECT_H_*/
