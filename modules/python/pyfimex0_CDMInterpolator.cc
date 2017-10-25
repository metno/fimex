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

#include "fimex/CDMInterpolator.h"

#include "pyfimex0_helpers.h"

#include <boost/make_shared.hpp>

#include <boost/python/class.hpp>
#include <boost/python/def.hpp>
#include <boost/python/enum.hpp>
#include <boost/python/register_ptr_to_python.hpp>

namespace MetNoFimex {
typedef boost::shared_ptr<CDMInterpolator> CDMInterpolator_p;
} // namespace MetNoFimex

using namespace MetNoFimex;
namespace bp = boost::python;

namespace {

enum InterpolationMethod {
  NearestNeighbor = MIFI_INTERPOL_NEAREST_NEIGHBOR,
  Bilinear = MIFI_INTERPOL_BILINEAR,
  Bicubic = MIFI_INTERPOL_BICUBIC,
  CoordNn = MIFI_INTERPOL_COORD_NN,
  CoordNnKd = MIFI_INTERPOL_COORD_NN_KD,
  ForwardSum = MIFI_INTERPOL_FORWARD_SUM,
  ForwardMean = MIFI_INTERPOL_FORWARD_MEAN,
  ForwardMedian = MIFI_INTERPOL_FORWARD_MEDIAN,
  ForwardMax = MIFI_INTERPOL_FORWARD_MAX,
  ForwardMin = MIFI_INTERPOL_FORWARD_MIN,
  ForwardUndefSum = MIFI_INTERPOL_FORWARD_UNDEF_SUM,
  ForwardUndefMean = MIFI_INTERPOL_FORWARD_UNDEF_MEAN,
  ForwardUndefMedian = MIFI_INTERPOL_FORWARD_UNDEF_MEDIAN,
  ForwardUndefMax = MIFI_INTERPOL_FORWARD_UNDEF_MAX,
  ForwardUndefMin = MIFI_INTERPOL_FORWARD_UNDEF_MIN
};

CDMInterpolator_p createInterpolator(CDMReader_p reader)
{
    return boost::make_shared<CDMInterpolator>(reader);
}

// wrapper for overload
void changeProjection1(CDMInterpolator_p i, InterpolationMethod method,
                       const std::string& proj_input,
                       const bp::object& out_x_axis, const bp::object& out_y_axis,
                       const std::string& out_x_axis_unit, const std::string& out_y_axis_unit,
                       CDMDataType out_x_axis_type, CDMDataType out_y_axis_type)
{
  const std::vector<double> x_axis = to_std_container< std::vector<double> >(out_x_axis);
  const std::vector<double> y_axis = to_std_container< std::vector<double> >(out_y_axis);
  i->changeProjection(method, proj_input, x_axis, y_axis,
                      out_x_axis_unit, out_y_axis_unit,
                      out_x_axis_type, out_y_axis_type);
}

void changeProjection2(CDMInterpolator_p i, InterpolationMethod method,
    const bp::object& lonVals, const bp::object& latVals)
{
    const std::vector<double> lons = to_std_container< std::vector<double> >(lonVals);
    const std::vector<double> lats = to_std_container< std::vector<double> >(latVals);
    i->changeProjection(method, lons, lats);
}

// wrappers for default arguments
void changeProjection1_1(CDMInterpolator_p i, InterpolationMethod method,
                       const std::string& proj_input,
                       const bp::object& out_x_axis, const bp::object& out_y_axis,
                       const std::string& out_x_axis_unit, const std::string& out_y_axis_unit)
{
    changeProjection1(i, method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit, CDM_DOUBLE, CDM_DOUBLE);
}

} // namespace

void pyfimex0_CDMInterpolator()
{
  bp::enum_<InterpolationMethod>("InterpolationMethod")
      .value("NEAREST_NEIGHBOR",     NearestNeighbor)
      .value("BILINEAR",             Bilinear)
      .value("BICUBIC",              Bicubic)
      .value("COORD_NN",             CoordNn)
      .value("COORD_NN_KD",          CoordNnKd)
      .value("FORWARD_SUM",          ForwardSum)
      .value("FORWARD_MEAN",         ForwardMean)
      .value("FORWARD_MEDIAN",       ForwardMedian)
      .value("FORWARD_MAX",          ForwardMax)
      .value("FORWARD_MIN",          ForwardMin)
      .value("FORWARD_UNDEF_SUM",    ForwardUndefSum)
      .value("FORWARD_UNDEF_MEAN",   ForwardUndefMean)
      .value("FORWARD_UNDEF_MEDIAN", ForwardUndefMedian)
      .value("FORWARD_UNDEF_MAX",    ForwardUndefMax)
      .value("FORWARD_UNDEF_MIN",    ForwardUndefMin)
      ;

    bp::class_<CDMInterpolator, bp::bases<CDMReader>, boost::noncopyable>("_CDMInterpolator", bp::no_init)
            .def("changeProjection", changeProjection1,
                 "Change the (main) projection the specified new projection and grid.\n\n"
                 ":param method: interpolation method, must be a pyfimex0.InterpolationMethod value\n"
                 ":param proj_input: input-string for proj4, used as output projection\n"
                 ":param out_x_axis: iterable with values of the output x-axis\n"
                 ":param out_y_axis: iterable with values of the output y-axis\n"
                 ":param out_x_axis_unit: unit of the output x-axis\n"
                 ":param out_y_axis_unit unit of the output y-axis\n"
                 ":param out_x_axis_type: type of x-axis values, must be a pyfimex0.CDMDataType\n"
                 ":param out_y_axis_type: type of y-axis values, must be a pyfimex0.CDMDataType\n")
            .def("changeProjection", changeProjection1_1,
                 "Change the (main) projection the specified new projection and grid.\n"
                 "Uses pyfimex0.CDMDataType.DOUBLE as data type for x and y axes.\n")
            .def("changeProjection", changeProjection2,
                 "Extract/interpolate a list of lat/lon points.\n\n"
                 ":param method: interpolation method, must be a pyfimex0.InterpolationMethod value; "
                 " only nearestneighbor, bilinear and bicubic supported\n"
                 ":param lonVals: longitude values in degree\n"
                 ":param latVals: latitude values in degree (same size as lonvals)\n")
            ;
    bp::register_ptr_to_python<CDMInterpolator_p>();

    bp::def("createInterpolator", createInterpolator);
}
