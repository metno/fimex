/*
 * Fimex, pyfimex0_CDMReader.cc
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

#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMReaderWriter.h"
#include "fimex/Data.h"

#include <boost/python.hpp>

using namespace MetNoFimex;
namespace bp = boost::python;

namespace {

// wrapper for overload
void CDMReaderWriter__putDataSlice2(CDMReaderWriter_p reader, const std::string& varName, size_t unLimDimPos, const DataPtr data)
{
    reader->putDataSlice(varName, unLimDimPos, data);
}

// wrapper for overload
void CDMReaderWriter__putScaledDataSlice2(CDMReaderWriter_p reader, const std::string& varName, size_t unLimDimPos, const DataPtr data)
{
    reader->putScaledDataSlice(varName, unLimDimPos, data);
}

// wrapper for overload
void CDMReaderWriter__putScaledDataSliceInUnit2(CDMReaderWriter_p rw, const std::string& varName, const std::string& unit, size_t unLimDimPos,
                                                const DataPtr data)
{
    rw->putScaledDataSliceInUnit(varName, unit, unLimDimPos, data);
}

// wrappers for default arguments
CDMReaderWriter_p createFileReaderWriter4(const std::string& fileType, const std::string& fileName, const std::string& configFile,
                                          const std::vector<std::string>& args)
{
    return CDMFileReaderFactory::createReaderWriter(fileType, fileName, configFile, args);
}
CDMReaderWriter_p createFileReaderWriter3(const std::string& fileType, const std::string& fileName, const std::string& configFile)
{
    return CDMFileReaderFactory::createReaderWriter(fileType, fileName, configFile);
}
CDMReaderWriter_p createFileReaderWriter2(const std::string& fileType, const std::string& fileName)
{
    return CDMFileReaderFactory::createReaderWriter(fileType, fileName);
}

} // namespace

void pyfimex0_CDMReaderWriter()
{
    bp::class_<CDMReaderWriter, bp::bases<CDMReader>, boost::noncopyable>("_CDMReaderWriter", bp::no_init)
        .def("putDataSlice", CDMReaderWriter__putDataSlice2)
        .def("putScaledDataSlice", CDMReaderWriter__putScaledDataSlice2)
        .def("putScaledDataSliceInUnit", CDMReaderWriter__putScaledDataSliceInUnit2);
    bp::register_ptr_to_python<CDMReaderWriter_p>();

    bp::def("createFileReaderWriter", createFileReaderWriter4);
    bp::def("createFileReaderWriter", createFileReaderWriter3);
    bp::def("createFileReaderWriter", createFileReaderWriter2);
}
