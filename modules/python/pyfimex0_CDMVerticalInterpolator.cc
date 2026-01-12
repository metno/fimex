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

#include "fimex/CDMVerticalInterpolator.h"

#include "pyfimex0_helpers.h"

#define PY_GIL_RELEASE py::gil_scoped_release release

using namespace MetNoFimex;
namespace py = pybind11;

namespace MetNoFimex {
typedef std::shared_ptr<CDMVerticalInterpolator> CDMVerticalInterpolator_p;
} // namespace MetNoFimex

namespace {

CDMVerticalInterpolator_p createVerticalInterpolator(CDMReader_p reader, const std::string& vtype, const std::string& vmethod)
{
    PY_GIL_RELEASE;
    return std::make_shared<CDMVerticalInterpolator>(reader, vtype, vmethod);
}

} // namespace

void pyfimex0_CDMVerticalInterpolator(py::module m)
{
    py::class_<CDMVerticalInterpolator, CDMVerticalInterpolator_p, CDMReader>(m, "_CDMVerticalInterpolator")
        .def("interpolateToFixed", &CDMVerticalInterpolator::interpolateToFixed,
             "Change the vertical axis from from the one given to a new specification.\n\n"
             ":param fixed: list of vertical levels\n")
        .def("ignoreValidityMin", &CDMVerticalInterpolator::ignoreValidityMin,
            "Ignore validity min when performing vertical interpolation.\n\n"
            ":param igore: if true, ignore validity min, else do not ignore\n")
        .def("ignoreValidityMax", &CDMVerticalInterpolator::ignoreValidityMax,
            "Ignore validity max when performing vertical interpolation.\n\n"
            ":param igore: if true, ignore validity max, else do not ignore\n")
        ;

    m.def("createVerticalInterpolator", createVerticalInterpolator);
}
