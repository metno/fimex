/*
  Fimex, include/fimex/min_max.h

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/


#ifndef FIMEX_MIN_MAX_H
#define FIMEX_MIN_MAX_H

#include <functional>
#include <iterator>
#include <utility>

template <typename Iter, class Compare = std::less<typename std::iterator_traits<Iter>::value_type>>
std::pair<Iter, Iter> min_max_element(Iter begin, Iter end, Compare comp = Compare())
{
    if (begin == end)
        return std::make_pair(end, end);

    Iter mini = begin, maxi = begin;

    Iter next1 = begin;
    while (++next1 != end) {
        Iter next2 = next1;
        ++next2;
        if (next2 == end)
            break;
        if (comp(*next1, *next2)) {
            // e1 < e2 => e1 cannot be maxi, e2 cannot be mini
            if (comp(*next1, *mini))
                mini = next1;
            if (comp(*maxi, *next2))
                maxi = next2;
        } else {
            if (comp(*next2, *mini))
                mini = next2;
            if (comp(*maxi, *next1))
                maxi = next1;
        }
        next1 = next2;
    }
    if (next1 != end) {
        if (comp(*next1, *mini))
            mini = next1;
        if (comp(*maxi, *next1))
            maxi = next1;
    }
    return std::make_pair(mini, maxi);
}

template <typename T>
bool minimize(T& a, const T& b)
{
    if (b < a) {
        a = b;
        return true;
    } else {
        return false;
    }
}

template <typename T>
bool maximize(T& a, const T& b)
{
    if (b > a) {
        a = b;
        return true;
    } else {
        return false;
    }
}

template <typename T>
void minimaximize(T& mi, T& ma, const T& b)
{
    if (b < mi)
        mi = b;
    else if (ma < b)
        ma = b;
}

#endif // FIMEX_MIN_MAX_H
