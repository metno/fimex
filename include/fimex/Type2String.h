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

#ifndef FIMEX_TYPE2STRING_H_
#define FIMEX_TYPE2STRING_H_

#include <sstream>

namespace MetNoFimex {

/**
 * convert a type (i.e. int, float) to string representation
 */
template <typename T>
std::ostream& type2stream(std::ostream& out, T in)
{
    out << in;
    return out;
}

//! specialization for high prececision
template <>
std::ostream& type2stream<double>(std::ostream& out, double in);

//! convert char from digits
template <>
inline std::ostream& type2stream<char>(std::ostream& out, char in)
{
    return type2stream(out, static_cast<int>(in));
}

//! convert unsigned char from digits
template <>
inline std::ostream& type2stream<unsigned char>(std::ostream& out, unsigned char in)
{
    return type2stream(out, static_cast<unsigned int>(in));
}

/**
 * convert a type (i.e. int, float) to string representation
 */
template <typename T>
std::string type2string(T in)
{
    std::ostringstream buffer;
    type2stream(buffer, in);
    return buffer.str();
}

//! no conversion for std::string
template <>
inline std::string type2string<std::string>(std::string in)
{
    return in;
}

} // namespace MetNoFimex

#endif // FIMEX_TYPE2STRING_H_
