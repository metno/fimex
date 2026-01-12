/*
 * Fimex, pyfimex0_CDMExtractor.cc
 *
 * (C) Copyright 2017-2019, met.no
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

#include "fimex/CDMException.h"
#include "fimex/CDMExtractor.h"
#include "fimex/TimeUnit.h"

#include "pyfimex0_helpers.h"

#define PY_GIL_RELEASE py::gil_scoped_release release

namespace MetNoFimex {
typedef std::shared_ptr<CDMExtractor> CDMExtractor_p;
} // namespace MetNoFimex

using namespace MetNoFimex;
namespace py = pybind11;

namespace {

CDMExtractor_p createExtractor(CDMReader_p reader)
{
    PY_GIL_RELEASE;
    return std::make_shared<CDMExtractor>(reader);
}

void reduceTimeStartEnd(MetNoFimex::CDMExtractor_p e, const std::string& tStart, const std::string& tEnd)
{
    PY_GIL_RELEASE;
    FimexTime start(FimexTime::min_date_time);
    if (!start.parseISO8601(tStart))
        throw CDMException("cannot parse time '" + tStart + "'");

    FimexTime end(FimexTime::max_date_time);
    if (!end.parseISO8601(tEnd))
        throw CDMException("cannot parse time '" + tEnd + "'");

    e->reduceTime(start, end);
}

void reduceDimensionStartEnd(MetNoFimex::CDMExtractor_p e, const std::string& dimName, size_t start, long long end)
{
    PY_GIL_RELEASE;
    e->reduceDimensionStartEnd(dimName, start, end);
}

// wrapper for overload
void reduceDimension_1(MetNoFimex::CDMExtractor_p e, const std::string& dimName, py::iterable slices)
{
    const auto slcs = to_std_container< std::set<std::size_t> >(slices);
    PY_GIL_RELEASE;
    e->reduceDimension(dimName, slcs);
}

void reduceDimension_2(MetNoFimex::CDMExtractor_p e, const std::string& dimName, size_t start, size_t length)
{
    PY_GIL_RELEASE;
    e->reduceDimension(dimName, start, length);
}

void selectVariables_2(MetNoFimex::CDMExtractor_p e, const py::iterable& variables, bool addAuxiliary)
{
  const auto vars = to_std_container< std::set<std::string> >(variables);
  PY_GIL_RELEASE;
  e->selectVariables(vars, addAuxiliary);
}

void selectVariables_1(MetNoFimex::CDMExtractor_p e, const py::iterable& variables)
{
  selectVariables_2(e, variables, true);
}

} // namespace

void pyfimex0_CDMExtractor(py::module m)
{
    py::class_<CDMExtractor, CDMExtractor_p, CDMReader>(m, "_CDMExtractor")
        .def("reduceDimension", reduceDimension_1,
             "Reduce a dimension\n\n"
             ":param dimName: dimension to change\n"
             ":param slices: iterable specifying slices to pick\n")
        .def("reduceDimension", reduceDimension_2,
             "Reduce a dimension\n\n"
             ":param dimName: dimension to change\n"
             ":param start: start-position\n"
             ":param length: size of the reduced dimension\n")
        .def("reduceDimensionStartEnd", reduceDimensionStartEnd,
             "Reduce a dimension\n\n"
             ":param dimName: dimension to change\n"
             ":param start: start-position\n"
             ":param end: end-position, 0 means full size, negative values start from end\n")
        .def("reduceTimeStartEnd", reduceTimeStartEnd,
             "Reduce time dimension\n\n"
             ":param tStart: start time as ISO8601 string\n"
             ":param tEnd: end time as ISO8601 string\n")
        .def("reduceLatLonBoundingBox", &CDMExtractor::reduceLatLonBoundingBox,
             "Tries to reduce the horizontal layer to the latitude-longitude bounding box given in degrees -90..90 and -180..180.\n\n"
             ":param south: southern border\n"
             ":param north: northern border\n"
             ":param west: western border\n"
             ":param east: eastern border")
        .def("removeVariable", &CDMExtractor::removeVariable,
             "Remove selected variable\n\n"
             ":param variable: name of variable to remove\n")
        .def("selectVariables", selectVariables_1,
             "Remove all variables except the ones selected plus some"
             " auxiliary variables needed by the selected variables\n\n"
             ":param variables: iterable specifying variables to select\n");

    m.def("createExtractor", createExtractor);
}
