/*
 * Fimex
 *
 * (C) Copyright 2019, met.no
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

#include "fimex/XMLUtils.h"

#include <cstdlib>

namespace MetNoFimex {

std::string XmlCharPtr::to_string() const
{
    return std::string(to_cc());
}

float XmlCharPtr::to_float() const
{
    return atof(to_cc());
}

long XmlCharPtr::to_long() const
{
    return atol(to_cc());
}

long long XmlCharPtr::to_longlong() const
{
    return atoll(to_cc());
}

int XmlCharPtr::cmp(const char* text) const
{
    return xmlStrcmp(p_, reinterpret_cast<const xmlChar*>(text));
}

size_t XmlCharPtr::len() const
{
    return xmlStrlen(p_);
}

} // namespace MetNoFimex
