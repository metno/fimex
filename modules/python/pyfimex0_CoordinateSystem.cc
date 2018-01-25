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

#include "fimex/coordSys/CoordinateSystem.h"

#include "pyfimex0_helpers.h"

#include <boost/python/class.hpp>
#include <boost/python/def.hpp>
#include <boost/python/enum.hpp>
#include <boost/python/list.hpp>
#include <boost/python/object.hpp>
#include <boost/python/register_ptr_to_python.hpp>
#include <boost/version.hpp>

using namespace MetNoFimex;
namespace bp = boost::python;

namespace {

template<class T>
void register_ptr_from_python()
{
#if BOOST_VERSION >= 106300
    // boost 1.63 has an extra template parameter specifying the
    // shared_ptr type (std::shared_ptr or boost::shared_ptr) and
    // this template parameter has no default value
    bp::converter::shared_ptr_from_python<T, boost::shared_ptr>();
#else
    bp::converter::shared_ptr_from_python<T>();
#endif
}

typedef boost::shared_ptr<const CoordinateSystem> CoordinateSystem_cp;
typedef boost::shared_ptr<CoordinateSystem> CoordinateSystem_p;

typedef boost::shared_ptr<CoordinateAxis> CoordinateAxis_p;
typedef boost::shared_ptr<const CoordinateAxis> CoordinateAxis_cp;

bp::list listCoordinateSystems1(CDMReader_p reader)
{
    const std::vector<CoordinateSystem_cp> cs = listCoordinateSystems(reader);
    bp::list py_cs;
    for (size_t i=0; i<cs.size(); ++i)
        py_cs.append(cs[i]);
    return py_cs;
}

CoordinateSystem_cp findCompleteCoordinateSystemFor1(const bp::list& py_cs, const std::string& varName)
{
    return findCompleteCoordinateSystemFor(to_std_container< std::vector<CoordinateSystem_cp> >(py_cs), varName);
}

CoordinateAxis_cp CoordinateSyste__findAxisOfType1(CoordinateSystem_cp cs, CoordinateAxis::AxisType type)
{
    return cs->findAxisOfType(type);
}

CoordinateAxis_cp CoordinateSyste__findAxisOfType2(CoordinateSystem_cp cs, const bp::list& types)
{
    return cs->findAxisOfType(to_std_container< std::vector<CoordinateAxis::AxisType> >(types));
}

} // namespace

void pyfimex0_CoordinateSystem()
{

    bp::enum_<CoordinateAxis::AxisType>("CoordinateAxisType")
            .value("Undefined", CoordinateAxis::Undefined)
            .value("GeoX", CoordinateAxis::GeoX)
            .value("GeoY", CoordinateAxis::GeoY)
            .value("GeoZ", CoordinateAxis::GeoZ)
            .value("Time", CoordinateAxis::Time)
            .value("Lon", CoordinateAxis::Lon)
            .value("Lat", CoordinateAxis::Lat)
            .value("Pressure", CoordinateAxis::Pressure)
            .value("Height", CoordinateAxis::Height)
            .value("ReferenceTime", CoordinateAxis::ReferenceTime)
            .value("Realization", CoordinateAxis::Realization)
            ;

    bp::class_<CoordinateAxis, bp::bases<CDMVariable>, CoordinateAxis_p, boost::noncopyable>("_CoordinateAxis", bp::no_init)
            ;
    bp::register_ptr_to_python<CoordinateAxis_cp>();
    register_ptr_from_python<const CoordinateAxis>();

    bp::class_<CoordinateSystem, CoordinateSystem_p, boost::noncopyable>("_CoordinateSystem", bp::no_init)
            .def("id", &CoordinateSystem::id)
            .def("findAxisOfType", &CoordinateSyste__findAxisOfType1)
            .def("findAxisOfType", &CoordinateSyste__findAxisOfType2)
            ;
    bp::register_ptr_to_python<CoordinateSystem_cp>();
    register_ptr_from_python<const CoordinateSystem>();

    bp::def("listCoordinateSystems", listCoordinateSystems1);
    bp::def("findCompleteCoordinateSystemFor", findCompleteCoordinateSystemFor1);
}
