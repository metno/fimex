/*
 * Fimex, pyfimex0_CDMInterpolator.cc
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
 *  Created on: Aug 17, 2017
 *      Author: Alexander Bürger
 */

#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif // HAVE_CONFIG_H

#include "fimex/CDMInterpolator.h"

#include "pyfimex0_helpers.h"

#include <boost/make_shared.hpp>

#include <boost/python/class.hpp>
#include <boost/python/def.hpp>
#include <boost/python/register_ptr_to_python.hpp>

namespace MetNoFimex {
typedef boost::shared_ptr<CDMInterpolator> CDMInterpolator_p;
} // namespace MetNoFimex

using namespace MetNoFimex;
namespace bp = boost::python;

namespace {

CDMInterpolator_p createInterpolator(CDMReader_p reader)
{
    return boost::make_shared<CDMInterpolator>(reader);
}

// wrapper for overload
void changeProjection1(CDMInterpolator_p i, int method,
                       const std::string& proj_input,
                       const std::string& out_x_axis, const std::string& out_y_axis,
                       const std::string& out_x_axis_unit, const std::string& out_y_axis_unit,
                       const std::string& out_x_axis_type, const std::string& out_y_axis_type)
{
    i->changeProjection(method, proj_input, out_x_axis, out_y_axis,
                        out_x_axis_unit, out_y_axis_unit, out_x_axis_type, out_y_axis_type);
}

void changeProjection2(CDMInterpolatorPtr i, int method,
    const bp::object& lonVals, const bp::object& latVals)
{
    const std::vector<double> lons = to_std_container< std::vector<double> >(lonVals);
    const std::vector<double> lats = to_std_container< std::vector<double> >(latVals);
    i->changeProjection(method, lons, lats);
}

// wrappers for default arguments
void changeProjection1_1(MetNoFimex::CDMInterpolator_p i, int method,
                       const std::string& proj_input,
                       const std::string& out_x_axis, const std::string& out_y_axis,
                       const std::string& out_x_axis_unit, const std::string& out_y_axis_unit)
{
    changeProjection1(i, method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit, "double", "double");
}

} // namespace

void pyfimex0_CDMInterpolator()
{
    bp::class_<CDMInterpolator, bp::bases<CDMReader>, boost::noncopyable>("_CDMInterpolator", bp::no_init)
            .def("changeProjection", changeProjection1)
            .def("changeProjection", changeProjection1_1)
            .def("changeProjection", changeProjection2)
            ;
    bp::register_ptr_to_python<CDMInterpolator_p>();

    bp::def("createInterpolator", createInterpolator);
}
