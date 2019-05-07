/*
 * Fimex, pyfimex0_NetCDF_CDMWriter.cc
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
 *  Created on: Aug 17, 2017
 *      Author: Alexander Bürger
 */

#include "fimex/NetCDF_CDMWriter.h"

#include "pyfimex0_helpers.h"

#include <boost/python/class.hpp>
#include <boost/python/def.hpp>
#include <boost/python/register_ptr_to_python.hpp>
#include <memory>

namespace MetNoFimex {
typedef std::shared_ptr<NetCDF_CDMWriter> NetCDF_CDMWriter_p;
} // namespace MetNoFimex

using namespace MetNoFimex;
namespace bp = boost::python;

namespace {

NetCDF_CDMWriter_p createNetCDFWriter_4(CDMReader_p reader, const std::string& filename, const std::string& configfile, int version)
{
    return std::make_shared<NetCDF_CDMWriter>(reader, filename, configfile, version);
}

NetCDF_CDMWriter_p createNetCDFWriter_3(CDMReader_p reader, const std::string& filename, const std::string& configfile)
{
    return createNetCDFWriter_4(reader, filename, configfile, 3);
}

NetCDF_CDMWriter_p createNetCDFWriter_2(CDMReader_p reader, const std::string& filename)
{
    return createNetCDFWriter_3(reader, filename, "");
}

} // namespace

void pyfimex0_NetCDF_CDMWriter()
{
    bp::class_<NetCDF_CDMWriter, bp::bases<CDMReader>, boost::noncopyable>("_NetCDF_CDMWriter", bp::no_init)
            ;
    bp::register_ptr_to_python<NetCDF_CDMWriter_p>();

    bp::def("createNetCDFWriter", createNetCDFWriter_4);
    bp::def("createNetCDFWriter", createNetCDFWriter_3);
    bp::def("createNetCDFWriter", createNetCDFWriter_2);
}
