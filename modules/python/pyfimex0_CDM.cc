/*
 * Fimex, pyfimex0_CDM.cc
 *
 * (C) Copyright 2017-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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
#include "fimex/Data.h"

#include "pyfimex0_helpers.h"

using namespace MetNoFimex;
namespace py = pybind11;

namespace {

py::list CDM__getAttributeNames(const CDM& cdm, const std::string& varName)
{
    const std::vector<CDMAttribute>& atts = cdm.getAttributes(varName);
    py::list names;
    for (size_t i=0; i<atts.size(); ++i)
        names.append(atts[i].getName());
    return names;
}
py::list CDM__getGlobalAttributeNames(const CDM& cdm)
{
    return CDM__getAttributeNames(cdm, CDM::globalAttributeNS());
}
const CDMAttribute& CDM__getAttribute(const CDM& cdm, const std::string& varName, const std::string& attrName)
{
    return cdm.getAttribute(varName, attrName);
}
const CDMAttribute& CDM__getGlobalAttribute(const CDM& cdm, const std::string& attrName)
{
    return cdm.getAttribute(CDM::globalAttributeNS(), attrName);
}

py::list CDM__getVariableNames(const CDM& cdm)
{
    const CDM::VarVec& vars = cdm.getVariables();
    py::list names;
    for (size_t i=0; i<vars.size(); ++i)
        names.append(vars[i].getName());
    return names;
}
const CDMVariable& CDM__getVariable(const CDM& cdm, const std::string& varName)
{
    return cdm.getVariable(varName);
}

py::list CDM__getDimensionNames(const CDM& cdm)
{
    const CDM::DimVec& dims = cdm.getDimensions();
    py::list names;
    for (size_t i=0; i<dims.size(); ++i)
        names.append(dims[i].getName());
    return names;
}
const CDMDimension& CDM__getDimension(const CDM& cdm, const std::string& dimName)
{
    return cdm.getDimension(dimName);
}
bool CDM__removeDimension(CDM& cdm, const std::string& dimName)
{
    return cdm.removeDimension(dimName);
}
bool CDM__removeDimensionIIU(CDM& cdm, const std::string& dimName, bool iiu)
{
    return cdm.removeDimension(dimName, iiu);
}

} // namespace

void pyfimex0_CDM(py::module m)
{
    py::class_<CDMAttribute>(m, "CDMAttribute")
        .def(py::init<std::string, std::string>())
        .def(py::init<std::string, std::string, std::string>())
        .def(py::init<std::string, DataPtr>())
        .def("getName", &CDMAttribute::getName)
        .def("setName", &CDMAttribute::setName)
        .def("getStringValue", &CDMAttribute::getStringValue)
        .def("getStringValueWithSeparator", &CDMAttribute::getStringValueWithSeparator)
        .def("getData", &CDMAttribute::getData)
        .def("setData", &CDMAttribute::setData)
        .def("getDataType", &CDMAttribute::getDataType);

    py::class_<CDMDimension>(m, "CDMDimension")
        .def(py::init<std::string, long>())
        .def("getName", &CDMDimension::getName)
        .def("setName", &CDMDimension::setName)
        .def("getLength", &CDMDimension::getLength)
        .def("setLength", &CDMDimension::setLength)
        .def("isUnlimited", &CDMDimension::isUnlimited)
        .def("setUnlimited", &CDMDimension::setUnlimited);

    py::class_<CDMVariable>(m, "CDMVariable")
        .def(py::init<std::string, CDMDataType, std::vector<std::string>>())
        .def("getName", &CDMVariable::getName)
        .def("setName", &CDMVariable::setName)
        .def("getShape", &CDMVariable::getShape)
        .def("setShape", &CDMVariable::setShape)
        .def("checkDimension", &CDMVariable::checkDimension)
        .def("getData", &CDMVariable::getData)
        .def("setData", &CDMVariable::setData)
        .def("getDataType", &CDMVariable::getDataType);

    py::class_<CDM> pyCDM(m, "CDM");
    pyCDM.def(py::init<>()).def(py::init<const CDM&>());
    pyCDM.def("getVariableNames", CDM__getVariableNames)
        .def("getVariable", CDM__getVariable, py::return_value_policy::reference_internal)
        .def("hasVariable", &CDM::hasVariable)
        .def("addVariable", &CDM::addVariable)
        .def("removeVariable", &CDM::removeVariable);
    pyCDM.def("getAttributeNames", CDM__getAttributeNames)
        .def("hasAttribute", &CDM::hasAttribute)
        .def("getAttribute", CDM__getAttribute, py::return_value_policy::reference_internal)
        .def("addAttribute", &CDM::addAttribute)
        .def("removeAttribute", &CDM::removeAttribute)
        .def("getGlobalAttributeNames", CDM__getGlobalAttributeNames)
        .def("getGlobalAttribute", CDM__getGlobalAttribute);
    pyCDM.def("getDimension", CDM__getDimension, py::return_value_policy::reference_internal)
        .def("hasDimension", &CDM::hasDimension)
        .def("addDimension", &CDM::addDimension)
        .def("removeDimension", CDM__removeDimension)
        .def("removeDimension", CDM__removeDimensionIIU)
        .def("getDimensionNames", CDM__getDimensionNames);
}
