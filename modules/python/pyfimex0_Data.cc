/*
 * Fimex, pyfimex0_Data.cc
 *
 * (C) Copyright 2017-2026, met.no
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
 *
 *  Created on: Aug 1, 2017
 *      Author: Alexander Bürger
 */

#include "fimex/CDMDataType.h"
#include "fimex/CDMException.h"
#include "fimex/Data.h"

#include "pyfimex0_helpers.h"
#include <pybind11/numpy.h>

using namespace MetNoFimex;
namespace py = pybind11;

namespace {

// also see https://github.com/pybind/pybind11/issues/1389
struct py_object_deleter
{
    py_object_deleter(py::object po)
        : po_(new py::object(po))
    {
    }
    void operator()(void*)
    {
        py::gil_scoped_acquire acquire;
        po_->release();
    }

    std::unique_ptr<py::object> po_;
};

template <class T>
py::array make_shared_nparray(MetNoFimex::shared_array<T> values, size_t count)
{
    MetNoFimex::shared_array<T>* array_ref(new MetNoFimex::shared_array<T>(values));
    py::capsule array_capsule(array_ref, [](void* ptr) { delete (MetNoFimex::shared_array<T>*)(ptr); });
    return py::array_t<T>(count, array_ref->get(), array_capsule); // pass 'base' py::object to avoid copy
}

py::object Data_values(DataPtr data)
{
    switch (data->getDataType()) {
    case CDM_SHORT:
        return make_shared_nparray(data->asShort(), data->size());
    case CDM_USHORT:
        return make_shared_nparray(data->asUShort(), data->size());
    case CDM_CHAR:
        return make_shared_nparray(data->asChar(), data->size());
    case CDM_UCHAR:
        return make_shared_nparray(data->asUChar(), data->size());
    case CDM_INT:
        return make_shared_nparray(data->asInt(), data->size());
    case CDM_UINT:
        return make_shared_nparray(data->asUInt(), data->size());
    case CDM_INT64:
        return make_shared_nparray(data->asInt64(), data->size());
    case CDM_UINT64:
        return make_shared_nparray(data->asUInt64(), data->size());
    case CDM_FLOAT:
        return make_shared_nparray(data->asFloat(), data->size());
    case CDM_DOUBLE:
        return make_shared_nparray(data->asDouble(), data->size());
    case CDM_STRING:
        return py::str(data->asString());
    case CDM_STRINGS: {
        auto strings = data->asStrings();
        py::list pystrings;
        const size_t count = data->size();
        for (size_t i = 0; i < count; ++i)
            pystrings.append(strings[i]);
        return std::move(pystrings);
    }
    default:
        throw CDMException("datatype not supported in pyfimex0");
    }
}

template <class T>
DataPtr create_NpArrayData(py::array_t<T> nparray)
{
    py::buffer_info req = nparray.request();
    shared_array<T> arr((T*)req.ptr, py_object_deleter(nparray));
    return createData(req.size, arr);
}

DataPtr Data_createS(const std::string& text)
{
    return createData(text);
}

DataPtr Data_createTV(CDMDataType dataType, py::object values)
{
    switch (dataType) {
    case CDM_FLOAT:
        return create_NpArrayData<float>(values);
    case CDM_DOUBLE:
        return create_NpArrayData<double>(values);
    case CDM_INT:
        return create_NpArrayData<int>(values);
    case CDM_UINT:
        return create_NpArrayData<unsigned int>(values);
    case CDM_SHORT:
        return create_NpArrayData<short>(values);
    case CDM_USHORT:
        return create_NpArrayData<unsigned short>(values);
    case CDM_CHAR:
        return create_NpArrayData<char>(values);
    case CDM_UCHAR:
        return create_NpArrayData<unsigned char>(values);
    case CDM_INT64:
        return create_NpArrayData<long long>(values);
    case CDM_UINT64:
        return create_NpArrayData<unsigned long long>(values);
    case CDM_STRING:
        return createData(values.cast<std::string>());
    default:
        throw CDMException("datatype not supported in pyfimex0");
    }
}

CDMDataType cdmDataTypeFromNP(py::object dtype)
{
    // see https://github.com/pybind/pybind11/issues/1424
    const std::string req_name = dtype.attr("name").cast<std::string>();

#define CONVERT_TYPE(T, C)                                                                                                                                     \
    do {                                                                                                                                                       \
        static const std::string T_name = py::dtype::of<T>().attr("name").cast<std::string>();                                                                 \
        if (req_name == T_name)                                                                                                                                \
            return C;                                                                                                                                          \
    } while (0)
    CONVERT_TYPE(float, CDM_FLOAT);
    CONVERT_TYPE(double, CDM_DOUBLE);
    CONVERT_TYPE(int, CDM_INT);
    CONVERT_TYPE(unsigned int, CDM_UINT);
    CONVERT_TYPE(short, CDM_SHORT);
    CONVERT_TYPE(unsigned short, CDM_USHORT);
    CONVERT_TYPE(char, CDM_CHAR);
    CONVERT_TYPE(unsigned char, CDM_UCHAR);
    CONVERT_TYPE(long long, CDM_INT64);
    CONVERT_TYPE(unsigned long long, CDM_UINT64);
#undef CONVERT_TYPE

    return CDM_NAT;
}

DataPtr Data_createNP(py::array nparray)
{
    return Data_createTV(cdmDataTypeFromNP(nparray.dtype()), nparray);
}

} // namespace

void pyfimex0_Data(py::module m)
{
    // clang-format off
    py::enum_<CDMDataType>(m, "CDMDataType")
        .value("NAT", CDM_NAT)
        .value("CHAR", CDM_CHAR)
        .value("SHORT", CDM_SHORT)
        .value("INT", CDM_INT)
        .value("FLOAT", CDM_FLOAT)
        .value("DOUBLE", CDM_DOUBLE)
        .value("STRING", CDM_STRING)
        .value("UCHAR", CDM_UCHAR)
        .value("USHORT", CDM_USHORT)
        .value("UINT", CDM_UINT)
        .value("INT64", CDM_INT64)
        .value("UINT64", CDM_UINT64)
        .value("STRINGS", CDM_STRINGS)
        ;

    py::class_<Data, DataPtr>(m, "_Data")
        .def("size", &Data::size)
        .def("values", Data_values)
        .def("getDataType", &Data::getDataType);
    // clang-format on

    m.def("createData", Data_createNP, "create Data with datatype based on the numpy array type");
    m.def("createData", Data_createS, "create Data with datatype CDM_STRING");
    m.def("createData", Data_createTV, "create Data with given datatype and values (not CDM_STRING)");

    m.def("cdmDataTypeFromNP", cdmDataTypeFromNP);
}
