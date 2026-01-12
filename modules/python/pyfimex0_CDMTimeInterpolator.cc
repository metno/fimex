/*
 * Fimex, pyfimex0_CDMTimeInterpolator.cc
 *
 * (C) Copyright 2019, met.no
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
 */

#include "fimex/CDMTimeInterpolator.h"

#include "pyfimex0_helpers.h"

#define PY_GIL_RELEASE py::gil_scoped_release release

using namespace MetNoFimex;
namespace py = pybind11;

namespace MetNoFimex {
typedef std::shared_ptr<CDMTimeInterpolator> CDMTimeInterpolator_p;
} // namespace MetNoFimex

namespace {

CDMTimeInterpolator_p createTimeInterpolator(CDMReader_p reader)
{
    PY_GIL_RELEASE;
    return std::make_shared<CDMTimeInterpolator>(reader);
}

} // namespace

void pyfimex0_CDMTimeInterpolator(py::module m)
{
    py::class_<CDMTimeInterpolator, CDMTimeInterpolator_p, CDMReader>(m, "_CDMTimeInterpolator")
        .def("changeTimeAxis", &CDMTimeInterpolator::changeTimeAxis,
             "Change the time-axis from from the one given to a new specification.\n\n"
             ":param timeSpec: string of time-specification\n");

    m.def("createTimeInterpolator", createTimeInterpolator);
}
