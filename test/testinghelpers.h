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

#include "fimex/CDMReaderDecl.h"

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
CDMReader_p getFLTH00Reader();

void copyFile(const std::string& from, const std::string& to);

/*! Write to netcdf file, if compiledwith netcdf support, else "write" to null file. */
bool writeToFile(CDMReader_p input, const std::string& fileName, bool removeFile=true);

} // namespace MetNoFimex

#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK
#include <boost/test/unit_test.hpp>

#define TEST4FIMEX_TEST_SUITE(x) BOOST_AUTO_TEST_SUITE(x)
#define TEST4FIMEX_TEST_SUITE_END() BOOST_AUTO_TEST_SUITE_END()
#define TEST4FIMEX_FIXTURE_TEST_CASE(x, fixture) BOOST_FIXTURE_TEST_CASE(x, fixture)

#define TEST4FIMEX_TEST_CASE(x) BOOST_AUTO_TEST_CASE(x)

#define TEST4FIMEX_REQUIRE(x) BOOST_REQUIRE(x)
#define TEST4FIMEX_CHECK(x) BOOST_CHECK(x)

#define TEST4FIMEX_FAIL(m) BOOST_FAIL(m)

#define TEST4FIMEX_REQUIRE_MESSAGE(x, m) BOOST_REQUIRE_MESSAGE(x, m)
#define TEST4FIMEX_CHECK_MESSAGE(x, m) BOOST_CHECK_MESSAGE(x, m)

#define TEST4FIMEX_REQUIRE_EQ(x, y) BOOST_REQUIRE_EQUAL(x, y)
#define TEST4FIMEX_CHECK_EQ(x, y) BOOST_CHECK_EQUAL(x, y)

#define TEST4FIMEX_CHECK_NE(x, y) BOOST_REQUIRE_NE(x, y)
#define TEST4FIMEX_REQUIRE_GT(a, b) BOOST_REQUIRE_GT(a, b)
#define TEST4FIMEX_REQUIRE_GE(a, b) BOOST_REQUIRE_GE(a, b)
#define TEST4FIMEX_REQUIRE_LE(a, b) BOOST_REQUIRE_LE(a, b)

#define TEST4FIMEX_REQUIRE_CLOSE(x, y, z) BOOST_REQUIRE_CLOSE(x, y, z)
#define TEST4FIMEX_CHECK_CLOSE(x, y, z) BOOST_CHECK_CLOSE(x, y, z)

#define TEST4FIMEX_CHECK_THROW(x, ex) BOOST_CHECK_THROW(x, ex)
#define TEST4FIMEX_CHECK_NO_THROW(x) BOOST_CHECK_NO_THROW(x)

#else // !HAVE_BOOST_UNIT_TEST_FRAMEWORK
#include <mi_cpptest.h>

#define TEST4FIMEX_TEST_SUITE(x) MI_CPPTEST_TEST_SUITE(x)
#define TEST4FIMEX_TEST_SUITE_END() MI_CPPTEST_TEST_SUITE_END()
#define TEST4FIMEX_FIXTURE_TEST_CASE(x, fixture) MI_CPPTEST_FIXTURE_TEST_CASE(x, fixture)

#define TEST4FIMEX_TEST_CASE(x) MI_CPPTEST_TEST_CASE(x)

#define TEST4FIMEX_REQUIRE(x) MI_CPPTEST_REQUIRE(x)
#define TEST4FIMEX_CHECK(x) MI_CPPTEST_CHECK(x)

#define TEST4FIMEX_FAIL(m) MI_CPPTEST_FAIL(m)

#define TEST4FIMEX_REQUIRE_MESSAGE(x, m) MI_CPPTEST_REQUIRE_MESSAGE(x, m)
#define TEST4FIMEX_CHECK_MESSAGE(x, m) MI_CPPTEST_CHECK_MESSAGE(x, m)

#define TEST4FIMEX_REQUIRE_EQ(x, y) MI_CPPTEST_REQUIRE_EQ(x, y)
#define TEST4FIMEX_CHECK_EQ(x, y) MI_CPPTEST_CHECK_EQ(x, y)

#define TEST4FIMEX_CHECK_NE(x, y) MI_CPPTEST_CHECK_NE(x, y)
#define TEST4FIMEX_REQUIRE_GT(a, b) MI_CPPTEST_REQUIRE_GT(a, b)
#define TEST4FIMEX_REQUIRE_GE(a, b) MI_CPPTEST_REQUIRE_GE(a, b)
#define TEST4FIMEX_REQUIRE_LE(a, b) MI_CPPTEST_REQUIRE_LE(a, b)

#define TEST4FIMEX_REQUIRE_CLOSE(x, y, z) MI_CPPTEST_REQUIRE_CLOSE(x, y, z)
#define TEST4FIMEX_CHECK_CLOSE(x, y, z) MI_CPPTEST_CHECK_CLOSE(x, y, z)

#define TEST4FIMEX_CHECK_THROW(x, ex) MI_CPPTEST_CHECK_THROW(x, ex)
#define TEST4FIMEX_CHECK_NO_THROW(x) MI_CPPTEST_CHECK_NO_THROW(x)
#endif // !HAVE_BOOST_UNIT_TEST_FRAMEWORK

extern "C" {
// implemented in interpolation.c
extern int mifi_3d_array_position(int x, int y, int z, int ix, int iy, int iz);
}

#endif // FIMEX_TESTINGHELPERS_H
