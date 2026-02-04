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

#ifndef CDMDIMENSION_H_
#define CDMDIMENSION_H_

#include "fimex/CDMNamedEntity.h"
#include "fimex/deprecated.h"

#include <iosfwd>
#include <string>

namespace MetNoFimex {

/**
 * @headerfile fimex/CDMDimension.h
 */
class CDMDimension : public CDMNamedEntity
{
public:
    CDMDimension(); // default null constructor for maps
    CDMDimension(std::string name, long length);
    ~CDMDimension();
    const std::string& getName() const override { return name; }
    void setName(std::string newName) {name = newName;}
    size_t getLength() const {return length;}
    void setLength(size_t length) {this->length = length;}
    void setUnlimited(int unlimited) {this->unlimited = unlimited;}
    int isUnlimited() const {return unlimited;}

    /**
     * @brief print xml representation to stream
     * @param out stream to write xml to
     */
    MIFI_DEPRECATED(void toXMLStream(std::ostream& out) const);

private:
    std::string name;
    size_t length;
    int unlimited;
};

} // namespace MetNoFimex

#endif /*CDMDIMENSION_H_*/
