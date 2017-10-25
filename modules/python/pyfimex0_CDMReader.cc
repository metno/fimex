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
#include "fimex/CDMReader.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/Data.h"

#include <boost/python.hpp>

using namespace MetNoFimex;
namespace bp = boost::python;

namespace {

// wrapper for overload
DataPtr getDataSlice2(CDMReader_p reader, const std::string& varName, size_t unLimDimPos)
{
    return reader->getDataSlice(varName, unLimDimPos);
}


// wrappers for default arguments
CDMReader_p createFileReader4(const std::string& fileType, const std::string& fileName,
        const std::string& configFile, const std::vector<std::string>& args)
{
    return CDMFileReaderFactory::create(fileType, fileName, configFile, args);
}
CDMReader_p createFileReader3(const std::string& fileType, const std::string& fileName,
        const std::string& configFile)
{
    return CDMFileReaderFactory::create(fileType, fileName, configFile);
}
CDMReader_p createFileReader2(const std::string& fileType, const std::string& fileName)
{
    return CDMFileReaderFactory::create(fileType, fileName);
}

} // namespace

void pyfimex0_CDMReader()
{
    bp::class_<CDMReader, boost::noncopyable>("_CDMReader", bp::no_init)
            .def("getDataSlice", getDataSlice2)
            .def("getCDM", &CDMReader::getCDM, bp::return_internal_reference<1>())
            ;
    bp::register_ptr_to_python<CDMReader_p>();

    bp::def("createFileReader", createFileReader4);
    bp::def("createFileReader", createFileReader3);
    bp::def("createFileReader", createFileReader2);
}
