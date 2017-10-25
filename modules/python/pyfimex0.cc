/*
 * Fimex, pyfimex0.cc
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

#include <boost/python.hpp>

namespace bp = boost::python;

void pyfimex0_logging();
void pyfimex0_numpy(); // in pyfimex0_Data.cc
void pyfimex0_Data();
void pyfimex0_CDM();
void pyfimex0_CDMReader();
void pyfimex0_CDMInterpolator();
void pyfimex0_CDMExtractor();

BOOST_PYTHON_MODULE(pyfimex0)
{
    pyfimex0_numpy();
    pyfimex0_logging();
    pyfimex0_Data();
    pyfimex0_CDM();
    pyfimex0_CDMReader();
    pyfimex0_CDMInterpolator();
    pyfimex0_CDMExtractor();
}
