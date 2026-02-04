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
#include "fimex/CDMReaderWriter.h"
#include "fimex/Data.h"

#include "pyfimex0_helpers.h"

#define PY_GIL_RELEASE py::gil_scoped_release release

using namespace MetNoFimex;
namespace py = pybind11;

namespace {

// wrapper for overload
void CDMReaderWriter__putDataSlice2(CDMReaderWriter_p reader, const std::string& varName, size_t unLimDimPos, const DataPtr data)
{
    PY_GIL_RELEASE;
    reader->putDataSlice(varName, unLimDimPos, data);
}

// wrapper for overload
void CDMReaderWriter__putScaledDataSlice2(CDMReaderWriter_p reader, const std::string& varName, size_t unLimDimPos, const DataPtr data)
{
    PY_GIL_RELEASE;
    reader->putScaledDataSlice(varName, unLimDimPos, data);
}

// wrapper for overload
void CDMReaderWriter__putScaledDataSliceInUnit2(CDMReaderWriter_p rw, const std::string& varName, const std::string& unit, size_t unLimDimPos,
                                                const DataPtr data)
{
    PY_GIL_RELEASE;
    rw->putScaledDataSliceInUnit(varName, unit, unLimDimPos, data);
}

// wrappers for default arguments
CDMReaderWriter_p createFileReaderWriter4(const std::string& fileType, const std::string& fileName, const std::string& configFile,
                                          const std::vector<std::string>& args)
{
    PY_GIL_RELEASE;
    return CDMFileReaderFactory::createReaderWriter(fileType, fileName, configFile, args);
}

CDMReaderWriter_p createFileReaderWriter3(const std::string& fileType, const std::string& fileName, const std::string& configFile)
{
    PY_GIL_RELEASE;
    return CDMFileReaderFactory::createReaderWriter(fileType, fileName, configFile);
}

CDMReaderWriter_p createFileReaderWriter2(const std::string& fileType, const std::string& fileName)
{
    PY_GIL_RELEASE;
    return CDMFileReaderFactory::createReaderWriter(fileType, fileName);
}

} // namespace

void pyfimex0_CDMReaderWriter(py::module m)
{
    py::class_<CDMReaderWriter, CDMReaderWriter_p, CDMReader>(m, "_CDMReaderWriter")
        .def("sync", &CDMReaderWriter::sync)
        .def("putDataSlice", CDMReaderWriter__putDataSlice2)
        .def("putScaledDataSlice", CDMReaderWriter__putScaledDataSlice2)
        .def("putScaledDataSliceInUnit", CDMReaderWriter__putScaledDataSliceInUnit2);

    m.def("createFileReaderWriter", createFileReaderWriter4);
    m.def("createFileReaderWriter", createFileReaderWriter3);
    m.def("createFileReaderWriter", createFileReaderWriter2);
}
