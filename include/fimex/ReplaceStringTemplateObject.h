/*
 * Fimex, ReplaceStringTemplateObject.h
 *
 * (C) Copyright 2013, met.no
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
 *
 *  Created on: Jun 6, 2013
 *      Author: heikok
 */

#ifndef REPLACESTRINGTEMPLATEOBJECT_H_
#define REPLACESTRINGTEMPLATEOBJECT_H_

#include "ReplaceStringObject.h"

namespace MetNoFimex {

/**
 * @headerfile fimex/ReplaceStringTemplateObject.h
 */
/**
 * Template implementation or ReplaceStringObject, for all Objects having
 * a default serialization.
 */
template<typename T>
class ReplaceStringTemplateObject: public MetNoFimex::ReplaceStringObject
{
public:
    ReplaceStringTemplateObject(T obj) : obj_(obj) {}

    ~ReplaceStringTemplateObject() {}
    std::ostream& put(std::ostream& s) const override
    {
        s << obj_;
        return s;
    }

    /// set the formatting string for this object, does nothing
    void setFormatString(const std::string& format) override {}

    /// set the formatting string and additional options for this object, does nothing
    void setFormatStringAndOptions(const std::string& format, const std::vector<std::string>& options) override {}

private:
    T obj_;
};

} // namespace MetNoFimex

#endif /* REPLACESTRINGTEMPLATEOBJECT_H_ */
