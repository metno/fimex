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

#ifndef FIMEX_STRING2TYPE_H_
#define FIMEX_STRING2TYPE_H_

#include <sstream>
#include <stdexcept>
#include <vector>

namespace MetNoFimex {

template <typename T>
T string2type(const std::string& s)
{
    T retVal;
    bool ok = !s.empty();

    if (std::is_arithmetic<T>() && ok) {
        const char c = s[0];
        ok = (c == '-') || (std::is_floating_point<T>() && c == '.') || std::isdigit(c);
    }
    if (ok) {
        std::istringstream buffer(s);
        buffer >> retVal;
        ok = buffer.eof() && !buffer.fail();
    }
    if (!ok)
        throw std::runtime_error("could not convert '" + s + "'");
    return retVal;
}

//! convert char to digits, not bytes
template <>
inline char string2type(const std::string& s)
{
    return static_cast<char>(string2type<int>(s));
}

//! convert unsigned char to digits, not bytes
template <>
inline unsigned char string2type(const std::string& s)
{
    return static_cast<unsigned char>(string2type<unsigned int>(s));
}

//! no conversion for std::string
template <>
inline std::string string2type<std::string>(const std::string& s)
{
    return s;
}

//! recognize on/true/1 as true, off,0,false as false
template <>
bool string2type<bool>(const std::string& s);

template <typename T>
std::vector<T> strings2types(const std::vector<std::string>& values)
{
    std::vector<T> t;
    t.reserve(values.size());
    for (const std::string& v : values)
        t.push_back(string2type<T>(v));
    return t;
}

} // namespace MetNoFimex

#endif // FIMEX_STRING2TYPE_H_
