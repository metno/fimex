/*
 * Fimex, pyfimex0_CDMExtractor.cc
 *
 * (C) Copyright 2017-2018, met.no
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

#include "pyfimex0_helpers.h"

#include <boost/make_shared.hpp>
#include <boost/python/class.hpp>
#include <boost/python/def.hpp>
#include <boost/python/register_ptr_to_python.hpp>

namespace MetNoFimex {
typedef boost::shared_ptr<CDMExtractor> CDMExtractor_p;
} // namespace MetNoFimex

using namespace MetNoFimex;
namespace bp = boost::python;

namespace {

CDMExtractor_p createExtractor(CDMReader_p reader)
{
    return boost::make_shared<CDMExtractor>(reader);
}

void reduceTimeStartEnd(MetNoFimex::CDMExtractor_p e, const std::string& tStart, const std::string& tEnd)
{
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
    e->reduceDimensionStartEnd(dimName, start, end);
}

// wrapper for overload
void reduceDimension_1(MetNoFimex::CDMExtractor_p e, const std::string& dimName, const bp::object& slices)
{
    e->reduceDimension(dimName, to_std_container< std::set<std::size_t> >(slices));
}

void reduceDimension_2(MetNoFimex::CDMExtractor_p e, const std::string& dimName, size_t start, size_t length)
{
    e->reduceDimension(dimName, start, length);
}

void selectVariables_2(MetNoFimex::CDMExtractor_p e, const bp::object variables, bool addAuxiliary)
{
  const std::set<std::string> vars = to_std_container< std::set<std::string> >(variables);
  e->selectVariables(vars, addAuxiliary);
}

void selectVariables_1(MetNoFimex::CDMExtractor_p e, const bp::object variables)
{
  selectVariables_2(e, variables, true);
}

} // namespace

void pyfimex0_CDMExtractor()
{
    bp::class_<CDMExtractor, bp::bases<CDMReader>, boost::noncopyable>("_CDMExtractor", bp::no_init)
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
        .def("selectVariables", selectVariables_1,
             "Remove all variables except the ones selected plus some"
             " auxiliary variables needed by the selected variables\n\n"
             ":param variables: iterable specifying variables to select\n")
            ;
    bp::register_ptr_to_python<CDMExtractor_p>();

    bp::def("createExtractor", createExtractor);
}
