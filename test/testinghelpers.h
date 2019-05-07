/*
  Fimex, test/testinghelpers.h

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/


#ifndef FIMEX_TESTINGHELPERS_H
#define FIMEX_TESTINGHELPERS_H 1

#include <string>

#include "fimex_config.h"

namespace MetNoFimex {

const std::string& topSrcDir();

bool exists(const std::string& path);
void remove(const std::string& path);
std::string require(const std::string& path);
size_t file_size(const std::string& path);

std::string pathShareEtc(const std::string& filename);
std::string pathTest(const std::string& filename);

bool hasTestExtra();
std::string pathTestExtra(const std::string& filename);

void copyFile(const std::string& from, const std::string& to);

} // namespace MetNoFimex

#ifndef FIMEX_TESTINGHELPERS_NO_BOOST
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK
#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;
#endif // HAVE_BOOST_UNIT_TEST_FRAMEWORK
#endif // FIMEX_TESTINGHELPERS_NO_BOOST

#endif // FIMEX_TESTINGHELPERS_H
