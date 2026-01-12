/*
 * Fimex, pyfimex0_AggregationReader.cc
 *
 * (C) Copyright 2019-2024, met.no
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

#include "fimex/AggregationReader.h"

#include "pyfimex0_helpers.h"

#define PY_GIL_RELEASE py::gil_scoped_release release

namespace MetNoFimex {
typedef std::shared_ptr<AggregationReader> AggregationReader_p;
} // namespace MetNoFimex

using namespace MetNoFimex;
namespace py = pybind11;

namespace {

AggregationReader_p createAggregationReader(const std::string& aggregationType)
{
    PY_GIL_RELEASE;
    return std::make_shared<AggregationReader>(aggregationType);
}

// handle default args
void addReader1(AggregationReader_p agg, CDMReader_p rd)
{
    PY_GIL_RELEASE;
    agg->addReader(rd, std::string(), std::string());
}

void addReader2(AggregationReader_p agg, CDMReader_p rd, const std::string& id)
{
    PY_GIL_RELEASE;
    agg->addReader(rd, id, std::string());
}

void addReader3(AggregationReader_p agg, CDMReader_p rd, const std::string& id, const std::string& coordValue)
{
    PY_GIL_RELEASE;
    agg->addReader(rd, id, coordValue);
}

} // namespace

void pyfimex0_AggregationReader(py::module m)
{
    py::class_<AggregationReader, AggregationReader_p, CDMReader>(m, "_AggregationReader")
        .def("addReader", addReader1,
             "Add a reader.\n\n"
             ":param reader: a CDMReader\n")
        .def("addReader", addReader2,
             "Add a reader.\n\n"
             ":param reader: a CDMReader\n"
             ":param id: optional reader id (for debug messages)\n")
        .def("addReader", addReader3,
             "Add a reader.\n\n"
             ":param reader: a CDMReader\n"
             ":param id: optional reader id (for debug messages)\n"
             ":param coordValue: coordinate value(s) along join axis)\n")
        .def("initAggregation", &AggregationReader::initAggregation, "Prepare for aggregation reading after adding all readers with addReader.\n");

    m.def("createAggregationReader", createAggregationReader);
}
