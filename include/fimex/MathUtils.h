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

#ifndef FIMEX_MATHUTILS_H_
#define FIMEX_MATHUTILS_H_

#include "fimex/String2Type.h"
#include "fimex/Type2String.h"

#include <cmath>
#include <limits>
#include <string>

namespace MetNoFimex {

/**
 * Round a double to integer.
 */
inline int round(double num)
{
    return ::lround(num);
}

/**
 * Round a float to integer.
 */
inline int round(float num)
{
    return ::lroundf(num);
}

template <typename T>
inline T clamp(T low, T value, T high)
{
    if (value < low)
        return low;
    if (value < high)
        return value;
    return high;
}

/** Cast with rounding as functor.
 *
 * Rounding is used if destination type (OUT) is integer and original type (IN) is not.
 */
template <typename OUT, typename IN, bool R>
struct data_rounder;

template <typename OUT, typename IN>
struct data_rounder<OUT, IN, true>
{
    inline OUT operator()(const IN& in) const { return static_cast<OUT>(round(in)); }
};

template <typename OUT, typename IN>
struct data_rounder<OUT, IN, false>
{
    inline OUT operator()(const IN& in) const { return static_cast<OUT>(in); }
};

/** Type cast as a functor.
 *
 * Uses data_rounder for type conversion.
 */
template <typename OUT, typename IN>
struct data_caster
{
    inline OUT operator()(const IN& in) const;
};

template <typename OUT, typename IN>
OUT data_caster<OUT, IN>::operator()(const IN& in) const
{
    return data_rounder < OUT, IN, std::numeric_limits<OUT>::is_integer && !std::numeric_limits<IN>::is_integer > ()(in);
}

template <typename INOUT>
struct data_caster<INOUT, INOUT>
{
    INOUT operator()(const INOUT& in) { return in; }
};

template <typename IN>
struct data_caster<std::string, IN>
{
    std::string operator()(const IN& in) { return type2string(in); }
};

template <typename OUT>
struct data_caster<OUT, std::string>
{
    OUT operator()(const std::string& in) { return string2type<OUT>(in); }
};

/** Round a double orr float to integer, and if the value is outside a range, replace with an "invalid" value.
 */
struct RoundAndClamp
{
    size_t mini, maxi, invalid;

    RoundAndClamp(size_t vb, size_t ve, size_t inv)
        : mini(vb)
        , maxi(ve)
        , invalid(inv)
    {
    }

    size_t operator()(double d) const;
    size_t operator()(float f) const;

    size_t clamped(size_t r) const;
};

/**
 * normalize Longitude to be within [-180:180]
 * @param in longitude in degree
 * @return longitude in degree within [-180:180]
 */
template <typename T>
T normalizeLongitude180(T in)
{
    while (in < -180) {
        in += 360;
    }
    while (in > 180) {
        in -= 360;
    }
    return in;
}

/**
 * template to declare isnan function in c++
 * @param x
 * @return same as C99 isnan pragma
 */
template <typename C>
int mifi_isnan(C x)
{
    return std::isnan(x);
}

} // namespace MetNoFimex

#endif // FIMEX_MATHUTILS_H_
