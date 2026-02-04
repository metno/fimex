/*
  Fimex, modules/python/pyfimex0_helpers.h

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

#ifndef PYFIMEX0_HELPERS_H
#define PYFIMEX0_HELPERS_H 1

#include <pybind11/pybind11.h>

// include here, see https://github.com/pybind/pybind11/issues/1055
#include <pybind11/stl.h>

namespace MetNoFimex {

template <typename C>
inline C to_std_container(const pybind11::iterable& pyc)
{
    typedef typename C::value_type T;
    C c;
    for (pybind11::iterator it = pybind11::iter(pyc); it && it != pybind11::iterator::sentinel(); ++it)
        c.insert(c.end(), (*it).cast<T>());
    return std::move(c);
}

} // namespace MetNoFimex

#endif // PYFIMEX0_HELPERS_H
