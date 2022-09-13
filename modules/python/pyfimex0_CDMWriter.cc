/*
 * Fimex, pyfimex0_CDMWriter.cc
 *
 * (C) Copyright 2017-2022, met.no
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

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMWriter.h"

#include "pyfimex0_helpers.h"

using namespace MetNoFimex;
namespace py = pybind11;

namespace {

// wrappers for createNetCDFWriter
void createNetCDFWriter_4(CDMReader_p reader, const std::string& filename, const std::string& configfile, int version)
{
    if (version != 3 && version != 4)
        throw CDMException("pyfimex createNetCDFWriter only supports version 3 or 4");

    const std::string filetype = (version == 4) ? "nc4" : "netcdf";
    createWriter(reader, filetype, filename, configfile);
}

void createNetCDFWriter_3(CDMReader_p reader, const std::string& filename, const std::string& configfile)
{
    createNetCDFWriter_4(reader, filename, configfile, 3);
}

void createNetCDFWriter_2(CDMReader_p reader, const std::string& filename)
{
    createNetCDFWriter_3(reader, filename, "");
}

// wrappers for default arguments
void createFileWriter4(CDMReader_p reader, const std::string& fileType, const std::string& fileName, const std::string& configFile)
{
    py::gil_scoped_release release;
    createWriter(reader, fileType, fileName, configFile);
}

void createFileWriter3(CDMReader_p reader, const std::string& fileType, const std::string& fileName)
{
    createFileWriter4(reader, fileType, fileName, std::string());
}

} // namespace

void pyfimex0_CDMWriter(py::module m)
{
    py::class_<CDMWriter>(m, "_CDMWriter");

    m.def("createFileWriter", createFileWriter4);
    m.def("createFileWriter", createFileWriter3);

    m.def("createNetCDFWriter", createNetCDFWriter_4);
    m.def("createNetCDFWriter", createNetCDFWriter_3);
    m.def("createNetCDFWriter", createNetCDFWriter_2);
}
