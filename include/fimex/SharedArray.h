/*
  Fimex, include/fimex/SharedArray.h

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


#ifndef FIMEX_SHARED_ARRAY_H
#define FIMEX_SHARED_ARRAY_H

#include <memory>

namespace MetNoFimex {

template <typename T>
class shared_array
{
public:
    shared_array() {}

    shared_array(T* content)
        : holder_(content, std::default_delete<T[]>())
    {
    }

    template <class Deleter>
    shared_array(T* content, Deleter d)
        : holder_(content, d)
    {
    }

    shared_array(const shared_array& other)
        : holder_(other.holder_)
    {
    }

    shared_array& operator=(const shared_array& other)
    {
        holder_ = other.holder_;
        return *this;
    }

    operator bool() const { return static_cast<bool>(holder_); }

    T* get() { return holder_.get(); }
    const T* get() const { return holder_.get(); }

    T& operator[](int i) { return get()[i]; }
    const T& operator[](int i) const { return get()[i]; }

private:
    std::shared_ptr<T> holder_;
};

template <typename T>
inline shared_array<T> make_shared_array(size_t size)
{
    return shared_array<T>(new T[size]);
}

} // namespace MetNoFimex

#endif // FIMEX_SHARED_ARRAY_H
