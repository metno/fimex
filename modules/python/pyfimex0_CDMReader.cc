/*
 * Fimex, pyfimex0_CDMReader.cc
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

#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMReader.h"
#include "fimex/Data.h"
#include "fimex/SliceBuilder.h"

#include "pyfimex0_helpers.h"

#define PY_GIL_ACQUIRE py::gil_scoped_acquire acquire
#define PY_GIL_RELEASE py::gil_scoped_release release

using namespace MetNoFimex;
namespace py = pybind11;

namespace {

std::vector<size_t> getSliceBuilderStartPositionsGilReleased(const SliceBuilder& sb)
{
    PY_GIL_RELEASE;
    return sb.getDimensionStartPositions();
}

std::vector<size_t> getSliceBuilderDimensionSizesGilReleased(const SliceBuilder& sb)
{
    PY_GIL_RELEASE;
    return sb.getDimensionSizes();
}

py::object convertToPyList(const std::vector<size_t>& v)
{
    py::list pylist;
    for (size_t s : v)
        pylist.append(s);
    return std::move(pylist);
}


void SliceBuilder__setStartAndSizeDN(SliceBuilder& sb, const std::string& dimName, size_t start, size_t size)
{
    PY_GIL_RELEASE;
    sb.setStartAndSize(dimName, start, size);
}

py::object SliceBuilder__getDimensionStartPositions(const SliceBuilder& sb)
{
    return convertToPyList(getSliceBuilderStartPositionsGilReleased(sb));
}

py::object SliceBuilder__getDimensionSizes(const SliceBuilder& sb)
{
    return convertToPyList(getSliceBuilderDimensionSizesGilReleased(sb));
}

DataPtr CDMReader__getData(CDMReader_p reader, const std::string& varName)
{
    PY_GIL_RELEASE;
    return reader->getData(varName);
}

DataPtr CDMReader__getScaledData(CDMReader_p reader, const std::string& varName)
{
    PY_GIL_RELEASE;
    return reader->getScaledData(varName);
}

DataPtr CDMReader__getScaledDataInUnit(CDMReader_p reader, const std::string& varName, const std::string& unit)
{
    PY_GIL_RELEASE;
    return reader->getScaledDataInUnit(varName, unit);
}

// wrapper for overload
DataPtr CDMReader__getDataSliceUL(CDMReader_p reader, const std::string& varName, int unLimDimPos)
{
    PY_GIL_RELEASE;
    return reader->getDataSlice(varName, unLimDimPos);
}
DataPtr CDMReader__getDataSliceSB(CDMReader_p reader, const std::string& varName, const SliceBuilder& sb)
{
    PY_GIL_RELEASE;
    return reader->getDataSlice(varName, sb);
}

// wrapper for overload
DataPtr CDMReader__getScaledDataSliceUL(CDMReader_p reader, const std::string& varName, int unLimDimPos)
{
    PY_GIL_RELEASE;
    return reader->getScaledDataSlice(varName, unLimDimPos);
}
DataPtr CDMReader__getScaledDataSliceSB(CDMReader_p reader, const std::string& varName, const SliceBuilder& sb)
{
    PY_GIL_RELEASE;
    return reader->getScaledDataSlice(varName, sb);
}

// wrapper for overload
DataPtr CDMReader__getScaledDataSliceInUnitUL(CDMReader_p reader, const std::string& varName, const std::string& unit, int unLimDimPos)
{
    PY_GIL_RELEASE;
    return reader->getScaledDataSliceInUnit(varName, unit, unLimDimPos);
}
DataPtr CDMReader__getScaledDataSliceInUnitSB(CDMReader_p reader, const std::string& varName, const std::string& unit, const SliceBuilder& sb)
{
    PY_GIL_RELEASE;
    return reader->getScaledDataSliceInUnit(varName, unit, sb);
}

// wrappers for default arguments
CDMReader_p createFileReader4(const std::string& fileType, const std::string& fileName,
        const std::string& configFile, const std::vector<std::string>& args)
{
    PY_GIL_RELEASE;
    return CDMFileReaderFactory::create(fileType, fileName, configFile, args);
}
CDMReader_p createFileReader3(const std::string& fileType, const std::string& fileName,
        const std::string& configFile)
{
    PY_GIL_RELEASE;
    return CDMFileReaderFactory::create(fileType, fileName, configFile);
}
CDMReader_p createFileReader2(const std::string& fileType, const std::string& fileName)
{
    PY_GIL_RELEASE;
    return CDMFileReaderFactory::create(fileType, fileName);
}

// see https://pybind11.readthedocs.io/en/stable/advanced/classes.html
class PyCDMReader : public CDMReader
{
public:
    PyCDMReader()
        : CDMReader()
    {
    }

    DataPtr getData(const std::string& varName) override
    {
        PY_GIL_ACQUIRE;
        PYBIND11_OVERLOAD(DataPtr, CDMReader, getData, varName);
    }
    DataPtr getScaledData(const std::string& varName) override
    {
        PY_GIL_ACQUIRE;
        PYBIND11_OVERLOAD(DataPtr, CDMReader, getScaledData, varName);
    }
    DataPtr getScaledDataInUnit(const std::string& varName, const std::string& unit) override
    {
        PY_GIL_ACQUIRE;
        PYBIND11_OVERLOAD(DataPtr, CDMReader, getScaledDataInUnit, varName, unit);
    }

    DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos) override
    {
        PY_GIL_ACQUIRE;
        PYBIND11_OVERLOAD_PURE(DataPtr, CDMReader, getDataSlice, varName, unLimDimPos);
    }
    DataPtr getScaledDataSlice(const std::string& varName, size_t unLimDimPos) override
    {
        PY_GIL_ACQUIRE;
        PYBIND11_OVERLOAD(DataPtr, CDMReader, getScaledDataSlice, varName, unLimDimPos);
    }
    DataPtr getScaledDataSliceInUnit(const std::string& varName, const std::string& unit, size_t unLimDimPos) override
    {
        PY_GIL_ACQUIRE;
        PYBIND11_OVERLOAD(DataPtr, CDMReader, getScaledDataSliceInUnit, varName, unit, unLimDimPos);
    }

    DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb) override
    {
        PY_GIL_ACQUIRE;
        PYBIND11_OVERLOAD_NAME(DataPtr, CDMReader, "getDataSliceSB", getDataSlice, varName, sb);
    }
    DataPtr getScaledDataSlice(const std::string& varName, const SliceBuilder& sb) override
    {
        PY_GIL_ACQUIRE;
        PYBIND11_OVERLOAD_NAME(DataPtr, CDMReader, "getScaledDataSliceSB", getScaledDataSlice, varName, sb);
    }
    DataPtr getScaledDataSliceInUnit(const std::string& varName, const std::string& unit, const SliceBuilder& sb) override
    {
        PY_GIL_ACQUIRE;
        PYBIND11_OVERLOAD_NAME(DataPtr, CDMReader, "getScaledDataSliceInUnitSB", getScaledDataSliceInUnit, varName, unit, sb);
    }
};

} // namespace

void pyfimex0_CDMReader(py::module m)
{
    py::class_<SliceBuilder>(m, "SliceBuilder")
        .def(py::init<const CDM&, std::string, bool>())
        .def("getStartAndSize",
             [](const SliceBuilder& sb, const std::string& dim) {
                 size_t st, n;
                 sb.getStartAndSize(dim, st, n);
                 return py::make_tuple(st, n);
             })
        .def("setStartAndSize", SliceBuilder__setStartAndSizeDN)
        .def("getDimensionStartPositions", SliceBuilder__getDimensionStartPositions)
        .def("getDimensionSizes", SliceBuilder__getDimensionSizes);

    py::class_<CDMReader, PyCDMReader, CDMReader_p>(m, "CDMReader")
        .def(py::init<>())
        .def("getData", CDMReader__getData)
        .def("getScaledData", CDMReader__getScaledData)
        .def("getScaledDataInUnit", CDMReader__getScaledDataInUnit)
        .def("getDataSlice", CDMReader__getDataSliceUL)
        .def("getScaledDataSlice", CDMReader__getScaledDataSliceUL)
        .def("getScaledDataSliceInUnit", CDMReader__getScaledDataSliceInUnitUL)
        .def("getDataSliceSB", CDMReader__getDataSliceSB)
        .def("getScaledDataSliceSB", CDMReader__getScaledDataSliceSB)
        .def("getScaledDataSliceInUnitSB", CDMReader__getScaledDataSliceInUnitSB)
        .def("getCDM", &CDMReader::getCDM, py::return_value_policy::reference_internal)
        .def("getInternalCDM", &CDMReader::getInternalCDM, py::return_value_policy::reference_internal)
        .def("setInternalCDM", &CDMReader::setInternalCDM);

    m.def("createFileReader", createFileReader4);
    m.def("createFileReader", createFileReader3);
    m.def("createFileReader", createFileReader2);
}
