/*
 * Fimex, testBinaryConstants.cc
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Sep 10, 2009
 *      Author: Heiko Klein
 */

#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include "fimex/binaryConstants.h"

BOOST_AUTO_TEST_CASE(test_binaryConstants) {
    BOOST_CHECK(0 == binary<0>::value);
    BOOST_CHECK(1 == binary<01>::value);
    BOOST_CHECK(64 == binary<01000000>::value);
    BOOST_CHECK(64 != binary< 1000000>::value); // remember to take octal values
    // std::cerr << binary<01000000000000000000000ULL>::value;
    BOOST_CHECK(2097152 == binary<01000000000000000000000ULL>::value); // large value
}


#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
