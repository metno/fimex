/*
  Fimex, src/leap_iterator.h

  Copyright (C) 2019-2026 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://github.com/metno/fimex/wiki

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

#ifndef LEAP_ITERATOR_HH
#define LEAP_ITERATOR_HH 1

#include <iterator>

/**
 * Iterator adaptor that leaps forward/backward:
 * one step in the leap_iterator are n steps in the adapted iterator.
 */
template <class I>
class leap_iterator : public std::iterator<std::bidirectional_iterator_tag, typename std::iterator_traits<I>::value_type>
{
public:
    typedef typename std::iterator_traits<I>::value_type value_type;
    typedef typename std::iterator_traits<I>::difference_type difference_type;
    typedef typename std::iterator_traits<I>::reference reference;

    leap_iterator() {}

    explicit leap_iterator(I x, difference_type step)
        : base_(x)
        , step_(step)
    {
    }

    leap_iterator& operator++() { return this->operator+=(1); }
    leap_iterator& operator--() { return this->operator-=(1); }
    leap_iterator operator++(int)
    {
        leap_iterator r = *this;
        *this += 1;
        return r;
    }
    leap_iterator operator--(int)
    {
        leap_iterator r = *this;
        *this -= 1;
        return r;
    }

    leap_iterator& operator+=(difference_type n)
    {
        base_ += n * step_;
        return *this;
    }
    leap_iterator& operator-=(difference_type n) { return this->operator+=(-n); }

    leap_iterator operator+(difference_type n) const
    {
        leap_iterator r = *this;
        r += n;
        return r;
    }
    leap_iterator operator-(difference_type n) const { return this->operator+(-n); }

    bool operator==(leap_iterator other) const { return base_ == other.base_; }
    bool operator!=(leap_iterator other) const { return !(*this == other); }

    reference operator*() const { return *base_; }

    difference_type step() const { return step_; }

private:
    I base_;
    difference_type step_;
};

#endif // LEAP_ITERATOR_HH
