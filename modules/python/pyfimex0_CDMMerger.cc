/*
 * Fimex, pyfimex0_CDMMerger.cc
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
 *
 *  Created on: Apr 16, 2019
 *      Author: Alexander Bürger
 */

#include "fimex/CDMMerger.h"
#include "fimex/CDMBorderSmoothing.h"

#include "pyfimex0_helpers.h"

#define PY_GIL_RELEASE py::gil_scoped_release release

using namespace MetNoFimex;
namespace py = pybind11;

namespace {

CDMMerger_p createMerger(CDMReader_p inner, CDMReader_p outer)
{
    PY_GIL_RELEASE;
    return std::make_shared<CDMMerger>(inner, outer);
}

// wrapper for overload
void setTargetGrid1(CDMMerger_p m, const std::string& proj, const std::string& tx_axis, const std::string& ty_axis, const std::string& tx_unit,
                    const std::string& ty_unit, const std::string& tx_type, const std::string& ty_type)
{
    PY_GIL_RELEASE;
    m->setTargetGrid(proj, tx_axis, ty_axis, tx_unit, ty_unit, tx_type, ty_type);
}

void setTargetGrid2(CDMMerger_p m, const std::string& proj, const py::iterable& tx_axis, const py::iterable& ty_axis, const std::string& tx_unit,
                    const std::string& ty_unit, CDMDataType tx_type, CDMDataType ty_type)
{
    const std::vector<double> tx = to_std_container<std::vector<double>>(tx_axis);
    const std::vector<double> ty = to_std_container<std::vector<double>>(ty_axis);
    PY_GIL_RELEASE;
    m->setTargetGrid(proj, tx, ty, tx_unit, ty_unit, tx_type, ty_type);
}

void setSmoothingFromSpec(CDMMerger_p m, const std::string& specification)
{
    PY_GIL_RELEASE;
    m->setSmoothing(createSmoothingFactory(specification));
}

} // namespace

void pyfimex0_CDMMerger(py::module m)
{
    // clang-format off
    py::class_<CDMMerger, CDMMerger_p, CDMReader>(m, "_CDMMerger")
        .def("setSmoothing", setSmoothingFromSpec, "Set the smooting function factory to be used.")
        .def("setUseOuterIfInnerUndefined", &CDMMerger::setUseOuterIfInnerUndefined,
             "If true, the outer value shall be used if the inner value is undefined.")
        .def("setKeepOuterVariables", &CDMMerger::setKeepOuterVariables,
             "Keep outer variables, even if no inner variable with the same name exists.")
        .def("setGridInterpolationMethod", &CDMMerger::setGridInterpolationMethod,
             "Set grid interpolation method, one of MIFI_INTERPOL_*")
        .def("setTargetGrid", setTargetGrid1, "Set target grid from string values, same as in CDMInterpolator.")
        .def("setTargetGrid", setTargetGrid2, "Set target grid from values, same as in CDMInterpolator.")
        .def("setTargetGridFromInner", &CDMMerger::setTargetGridFromInner,
             "Set target grid as inner grid expanded to cover outer grid.");
    ;
    // clang-format on

    m.def("createMerger", createMerger);
}
