/*
 * Fimex
 *
 * (C) Copyright 2008-2023, met.no
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

#include "fimex/MathUtils.h"

#include <numeric>

namespace MetNoFimex {

size_t RoundAndClamp::operator()(double d) const
{
    return clamped(round(d));
}

size_t RoundAndClamp::operator()(float f) const
{
    return clamped(round(f));
}

size_t RoundAndClamp::clamped(size_t r) const
{
    if (r >= mini && r <= maxi)
        return r;
    else
        return invalid;
}

size_t product(const size_t* begin, const size_t* end)
{
    // it is important to initialize with "size_t(1)" -- just using "1"
    // will cause overflow at 32 bits
    return std::accumulate(begin, end, size_t(1), std::multiplies<size_t>());
}

} // namespace MetNoFimex
