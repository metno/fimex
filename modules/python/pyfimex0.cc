/*
 * Fimex, pyfimex0.cc
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

#include "fimex/CDMconstants.h"

#include <pybind11/pybind11.h>

namespace py = pybind11;

void pyfimex0_logging(py::module m);
void pyfimex0_Data(py::module m);
void pyfimex0_CDM(py::module m);
void pyfimex0_CDMInterpolator(py::module m);
void pyfimex0_CDMExtractor(py::module m);
void pyfimex0_CDMMerger(py::module m);
void pyfimex0_CDMReader(py::module m);
void pyfimex0_CDMReaderWriter(py::module m);
void pyfimex0_CDMTimeInterpolator(py::module m);
void pyfimex0_CDMWriter(py::module m);
void pyfimex0_AggregationReader(py::module m);
void pyfimex0_CoordinateSystem(py::module m);
void pyfimex0_NcmlCDMReader(py::module m);

namespace {
py::object mifi_version()
{
    return py::make_tuple(mifi_version_major(), mifi_version_minor(), mifi_version_patch(), mifi_version_status());
}
} // namespace

PYBIND11_MODULE(pyfimex0, m)
{
    pyfimex0_logging(m);
    pyfimex0_Data(m);
    pyfimex0_CDM(m);
    pyfimex0_CDMReader(m);
    pyfimex0_CDMReaderWriter(m);
    pyfimex0_CDMWriter(m);
    pyfimex0_CDMInterpolator(m);
    pyfimex0_CDMTimeInterpolator(m);
    pyfimex0_CDMExtractor(m);
    pyfimex0_CDMMerger(m);
    pyfimex0_CoordinateSystem(m);
    pyfimex0_AggregationReader(m);
    pyfimex0_NcmlCDMReader(m);

    m.def("mifi_version", mifi_version, "Returns a 4-tuple with (major, minor, patch, status) version numbers.");
}
