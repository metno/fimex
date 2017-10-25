/*
 * Fimex, pyfimex0_Data.cc
 *
 * (C) Copyright 2017, met.no
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
 *
 *  Created on: Aug 1, 2017
 *      Author: Alexander Bürger
 */

#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif // HAVE_CONFIG_H

#include "fimex/Data.h"

#include <boost/python/class.hpp>
#include <boost/python/def.hpp>
#include <boost/python/handle.hpp>
#include <boost/python/list.hpp>
#include <boost/python/object.hpp>
#include <boost/python/register_ptr_to_python.hpp>

#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <numpy/ndarrayobject.h> // ensure you include this header

using namespace MetNoFimex;
namespace bp = boost::python;

namespace {

template<typename T>
struct numpy_type;

template<>
struct numpy_type<double> {
    static int typenum() { return NPY_DOUBLE; }
};

template<>
struct numpy_type<float> {
    static int typenum() { return NPY_FLOAT; }
};

template<>
struct numpy_type<short> {
    static int typenum() { return NPY_SHORT; }
};

template<>
struct numpy_type<int> {
    static int typenum() { return NPY_INT; }
};

template<>
struct numpy_type<std::string> {
    static int typenum() { return NPY_STRING; }
};

template<typename T>
bp::object wrap(boost::shared_array<T> data, npy_intp size)
{
    if (!data)
        return bp::object();

    T* datacopy = new T[size];
    std::copy(data.get(), data.get() + size, datacopy);

    // based on https://stackoverflow.com/a/34023333/8337632
    npy_intp shape[1] = { size }; // array size
    PyObject* obj = PyArray_New(&PyArray_Type,
            1, shape,
            numpy_type<T>::typenum(), // data type
            NULL, datacopy,
            0, NPY_ARRAY_CARRAY_RO, // NPY_ARRAY_CARRAY_RO for readonly
            NULL);
    bp::handle<> array(obj);
    return bp::object(array);
}

template<>
bp::object wrap(boost::shared_array<std::string> data, npy_intp size)
{
    if (!data)
        return bp::object();

    bp::list strings;
    for (npy_intp i=0; i<size; ++i)
        strings.append(data[i]);
    return strings;
}

bp::object Data_values(DataPtr data)
{
    switch (data->getDataType()) {
    case CDM_SHORT:
        return wrap(data->asShort(), data->size());
    case CDM_INT:
        return wrap(data->asInt(), data->size());
    case CDM_FLOAT:
        return wrap(data->asFloat(), data->size());
    case CDM_DOUBLE:
        return wrap(data->asDouble(), data->size());
    case CDM_STRINGS:
        return wrap(data->asStrings(), data->size());
    default:
        return bp::object();
    }
}

#if PY_MAJOR_VERSION >= 3
void* init_numpy()
{
    import_array();
    return 0;
}
#else // PY_MAJOR_VERSION < 3
void init_numpy()
{
    import_array();
}
#endif

} // namespace

void pyfimex0_numpy()
{
    init_numpy();
}

void pyfimex0_Data()
{
    bp::class_<Data, boost::noncopyable>("_Data", bp::no_init)
            .def("size", &Data::size)
            .def("values", Data_values)
            ;
    bp::register_ptr_to_python<DataPtr>();
}
