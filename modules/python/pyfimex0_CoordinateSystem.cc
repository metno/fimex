/*
 * Fimex, pyfimex0_CoordinateSystem.cc
 *
 * (C) Copyright 2018, met.no
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

#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

using namespace MetNoFimex;
namespace py = pybind11;

namespace {

py::list listCoordinateSystems1(CDMReader_p reader)
{
    const CoordinateSystem_cp_v cs = listCoordinateSystems(reader);
    py::list py_cs;
    for (size_t i=0; i<cs.size(); ++i)
        py_cs.append(cs[i]);
    return py_cs;
}

CoordinateSystem_cp findCompleteCoordinateSystemFor1(const std::vector<CoordinateSystem_cp>& cs, const std::string& varName)
{
    return findCompleteCoordinateSystemFor(cs, varName);
}

CoordinateAxis_cp CoordinateSyste__findAxisOfType1(CoordinateSystem_cp cs, CoordinateAxis::AxisType type)
{
    return cs->findAxisOfType(type);
}

CoordinateAxis_cp CoordinateSyste__findAxisOfType2(CoordinateSystem_cp cs, const std::vector<CoordinateAxis::AxisType>& types)
{
    return cs->findAxisOfType(types);
}

} // namespace

void pyfimex0_CoordinateSystem(py::module m)
{
    py::enum_<CoordinateAxis::AxisType>(m, "CoordinateAxisType")
        .value("Undefined", CoordinateAxis::Undefined)
        .value("GeoX", CoordinateAxis::GeoX)
        .value("GeoY", CoordinateAxis::GeoY)
        .value("GeoZ", CoordinateAxis::GeoZ)
        .value("Time", CoordinateAxis::Time)
        .value("Lon", CoordinateAxis::Lon)
        .value("Lat", CoordinateAxis::Lat)
        .value("Pressure", CoordinateAxis::Pressure)
        .value("Height", CoordinateAxis::Height)
        .value("Depth", CoordinateAxis::Depth)
        .value("ReferenceTime", CoordinateAxis::ReferenceTime)
        .value("Realization", CoordinateAxis::Realization);

    py::class_<CoordinateAxis, CoordinateAxis_p>(m, "_CoordinateAxis").def("getName", &CoordinateAxis::getName);

    py::class_<CoordinateSystem, CoordinateSystem_p>(m, "_CoordinateSystem")
        .def("id", &CoordinateSystem::id)
        .def("findAxisOfType", &CoordinateSyste__findAxisOfType1)
        .def("findAxisOfType", &CoordinateSyste__findAxisOfType2);

    m.def("listCoordinateSystems", listCoordinateSystems1);
    m.def("findCompleteCoordinateSystemFor", findCompleteCoordinateSystemFor1);
}
