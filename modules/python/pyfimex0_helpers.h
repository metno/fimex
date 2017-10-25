#ifndef PYFIMEX0_HELPERS_H
#define PYFIMEX0_HELPERS_H 1

#include <boost/python/object.hpp>
#include <boost/python/stl_iterator.hpp>

namespace MetNoFimex {

template<typename C>
inline C to_std_container(const boost::python::object& iterable)
{
    typedef typename C::value_type T;
    typedef boost::python::stl_input_iterator<T> I;
    return C(I(iterable), I());
}

} // namespace MetNoFimex

#endif // PYFIMEX0_HELPERS_H
