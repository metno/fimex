/*
 * Fimex, pyfimex0_CDM.cc
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

#include "fimex/CDM.h"
#include "fimex/Data.h"

#include <boost/python/class.hpp>
#include <boost/python/copy_const_reference.hpp>
#include <boost/python/list.hpp>
#include <boost/python/return_value_policy.hpp>

using namespace MetNoFimex;
namespace bp = boost::python;

namespace {

bp::list CDM__getAttributeNames(const CDM& cdm, const std::string& varName)
{
    const std::vector<CDMAttribute>& atts = cdm.getAttributes(varName);
    bp::list names;
    for (size_t i=0; i<atts.size(); ++i)
        names.append(atts[i].getName());
    return names;
}
bp::list CDM__getGlobalAttributeNames(const CDM& cdm)
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

bp::list CDM__getVariableNames(const CDM& cdm)
{
    const CDM::VarVec& vars = cdm.getVariables();
    bp::list names;
    for (size_t i=0; i<vars.size(); ++i)
        names.append(vars[i].getName());
    return names;
}
const CDMVariable& CDM__getVariable(const CDM& cdm, const std::string& varName)
{
    return cdm.getVariable(varName);
}

bp::list CDM__getDimensionNames(const CDM& cdm)
{
    const CDM::DimVec& dims = cdm.getDimensions();
    bp::list names;
    for (size_t i=0; i<dims.size(); ++i)
        names.append(dims[i].getName());
    return names;
}
const CDMDimension& CDM__getDimension(const CDM& cdm, const std::string& dimName)
{
    return cdm.getDimension(dimName);
}

bp::list CDMVariable__getShape(const CDMVariable& var)
{
    const std::vector<std::string>& shape = var.getShape();
    bp::list py_shape;
    for (size_t i=0; i<shape.size(); ++i)
        py_shape.append(shape[i]);
    return py_shape;
}
} // namespace

void pyfimex0_CDM()
{
    bp::class_<CDMAttribute, boost::noncopyable>("_CDMAttribute", bp::no_init)
            .def("getName", &CDMDimension::getName, bp::return_value_policy<bp::copy_const_reference>())
            .def("getData", &CDMAttribute::getData)
            .def("getStringValue", &CDMAttribute::getStringValue);
            ;

    bp::class_<CDMDimension, boost::noncopyable>("_CDMDimension", bp::no_init)
            .def("getName", &CDMDimension::getName, bp::return_value_policy<bp::copy_const_reference>())
            .def("getLength", &CDMDimension::getLength)
            ;

    bp::class_<CDMVariable, boost::noncopyable>("_CDMVariable", bp::no_init)
            .def("getName", &CDMVariable::getName, bp::return_value_policy<bp::copy_const_reference>())
            .def("getShape", CDMVariable__getShape)
            ;

    bp::class_<CDM, boost::noncopyable>("_CDM", bp::no_init)
            .def("getVariableNames", CDM__getVariableNames)
            .def("getVariable", CDM__getVariable, bp::return_internal_reference<1>())
            .def("getAttributeNames", CDM__getAttributeNames)
            .def("getAttribute", CDM__getAttribute, bp::return_internal_reference<1>())
            .def("getGlobalAttributeNames", CDM__getGlobalAttributeNames)
            .def("getGlobalAttribute", CDM__getGlobalAttribute, bp::return_internal_reference<1>())
            .def("getDimension", CDM__getDimension, bp::return_internal_reference<1>())
            .def("getDimensionNames", CDM__getDimensionNames)
            .def("hasDimension", &CDM::hasDimension)
            ;
}
