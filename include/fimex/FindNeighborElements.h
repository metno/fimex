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

#ifndef FIMEX_FINDNEIGHBORELEMENTS_H_
#define FIMEX_FINDNEIGHBORELEMENTS_H_

#include "min_max.h"

#include <iterator>
#include <limits>
#include <utility>

namespace MetNoFimex {

/**
 * Find closest distinct elements in an unordered list. The order of elements is not defined.
 *
 * Except for the case where all elements are equal, it is always ensured that the neighbors
 * are distinct.

 * @param start
 * @param end
 * @param x
 * @return pair of the positions of a and b, with a closer than b
 */
template <typename InputIterator>
std::pair<typename std::iterator_traits<InputIterator>::difference_type, typename std::iterator_traits<InputIterator>::difference_type>
find_closest_distinct_elements(InputIterator start, InputIterator end, typename std::iterator_traits<InputIterator>::value_type x)
{
    using namespace std;
    typedef typename std::iterator_traits<InputIterator>::value_type value_t;
    typename iterator_traits<InputIterator>::difference_type retVal1 = 0;
    typename iterator_traits<InputIterator>::difference_type retVal2 = 0;
    value_t v1, v1Diff, v2Diff;
    if (start != end) {
        v1 = *start;
        v1Diff = abs(x - *start);
        v2Diff = v1Diff;
    }
    for (InputIterator cur = start; cur != end; ++cur) {
        const value_t vDiff = abs(x - *cur);
        if (vDiff <= v2Diff) {
            if (vDiff < v1Diff) {
                retVal2 = retVal1;
                v2Diff = v1Diff;
                v1 = *cur;
                retVal1 = distance(start, cur);
                v1Diff = vDiff;
            } else if (*cur != v1) {
                retVal2 = distance(start, cur);
                v2Diff = vDiff;
            }
        } // else nothing to be done
    }
    return make_pair(retVal1, retVal2);
}

/**
 * Find closest distinct neighbor elements in an unordered list, with a <= x < b
 * It might extrapolate if x is smaller than all elements (or x > all elements) and
 * fall back to find_closest_distinct_elements()
 *
 * Except for the case where all elements are equal, it is always ensured that the neighbors
 * are distinct.

 * @param start
 * @param end
 * @param x
 * @return pair of the positions of a and b, with a closer than b
 */
template <typename InputIterator>
std::pair<typename std::iterator_traits<InputIterator>::difference_type, typename std::iterator_traits<InputIterator>::difference_type>
find_closest_neighbor_distinct_elements(InputIterator start, InputIterator end, typename std::iterator_traits<InputIterator>::value_type x)
{
    if (start == end)
        return std::make_pair(0, 0);

    typedef typename std::iterator_traits<InputIterator>::value_type value_t;
    InputIterator lowest = start;
    InputIterator highest = start;
    InputIterator cur = start;
    value_t lowDiff = x - *cur;
    value_t highDiff = -lowDiff;
    const double maxDiff = std::numeric_limits<value_t>::max();
    if (lowDiff < 0)
        lowDiff = maxDiff;
    if (highDiff < 0)
        highDiff = maxDiff;
    while (++cur != end) {
        const value_t diff = x - *cur;
        if (diff >= 0) {
            if (minimize(lowDiff, diff))
                lowest = cur;
        } else {
            if (minimize(highDiff, -diff))
                highest = cur;
        }
    }
    if (lowDiff == maxDiff || highDiff == maxDiff) {
        // extrapolating
        return find_closest_distinct_elements(start, end, x);
    }

    return std::make_pair(std::distance(start, lowest), std::distance(start, highest));
}

} // namespace MetNoFimex

#endif /*FIMEX_FINDNEIGHBORELEMENTS_H_*/
