/*
 * Fimex, pyfimex0.cc
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

#include "fimex/CDMconstants.h"

#include <boost/python.hpp>

namespace bp = boost::python;

void pyfimex0_logging();
void pyfimex0_numpy(); // in pyfimex0_Data.cc
void pyfimex0_Data();
void pyfimex0_CDM();
void pyfimex0_CDMInterpolator();
void pyfimex0_CDMExtractor();
void pyfimex0_CDMReader();
void pyfimex0_CDMWriter();
void pyfimex0_CoordinateSystem();
void pyfimex0_NetCDF_CDMWriter();

namespace {
bp::object mifi_version()
{
    return bp::make_tuple(mifi_version_major(),
                          mifi_version_minor(),
                          mifi_version_patch(),
                          mifi_version_status());
}
} // namespace

BOOST_PYTHON_MODULE(pyfimex0)
{
    pyfimex0_numpy();
    pyfimex0_logging();
    pyfimex0_Data();
    pyfimex0_CDM();
    pyfimex0_CDMReader();
    pyfimex0_CDMWriter();
    pyfimex0_CDMInterpolator();
    pyfimex0_CDMExtractor();
    pyfimex0_CoordinateSystem();
    pyfimex0_NetCDF_CDMWriter();

    bp::def("mifi_version", mifi_version, "Returns a 4-tuple with (major, minor, patch, status) version numbers.");
}
