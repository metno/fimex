/*
 * Fimex, pyfimex0_CDMWriter.cc
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
#include "fimex/CDMWriter.h"

#include <boost/python.hpp>

using namespace MetNoFimex;
namespace bp = boost::python;

namespace {

// wrappers for default arguments
void createFileWriter4(CDMReader_p reader, const std::string& fileType, const std::string& fileName, const std::string& configFile)
{
    createWriter(reader, fileType, fileName, configFile);
}

void createFileWriter3(CDMReader_p reader, const std::string& fileType, const std::string& fileName)
{
    createFileWriter4(reader, fileType, fileName, std::string());
}

} // namespace

void pyfimex0_CDMWriter()
{
    bp::class_<CDMWriter, boost::noncopyable>("_CDMWriter", bp::no_init)
            ;
    bp::register_ptr_to_python<CDMWriter_p>();

    bp::def("createFileWriter", createFileWriter4);
    bp::def("createFileWriter", createFileWriter3);
}
