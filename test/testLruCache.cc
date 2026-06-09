/*
  Fimex, test/testLruCache.cc

  Copyright (C) 2026 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: fimx@met.no

  Project Info:  https://github.com/metno/fimex/wiki

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

#include "testinghelpers.h"

#include "LruCache.h"

#include <string>

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_lru_capacity_zero)
{
    int counter = 0;
    LruCache<string, int> c(0);

    auto make = [&]() { return ++counter; };

    int a1 = c.getOrInsert("a", make);
    int a2 = c.getOrInsert("a", make);
    TEST4FIMEX_CHECK(a1 != 0);
    // capacity 0 means no caching: subsequent calls produce new values
    TEST4FIMEX_CHECK(a2 != a1);
}

TEST4FIMEX_TEST_CASE(test_lru_eviction_and_order)
{
    int counter = 0;
    LruCache<string, int> c(2);

    auto make = [&]() { return ++counter; };

    int a = c.getOrInsert("a", make); // a=1
    int b = c.getOrInsert("b", make); // b=2
    // re-access a to make it recently used
    int a_again = c.getOrInsert("a", make);
    TEST4FIMEX_CHECK_EQ(a, a_again);

    int cval = c.getOrInsert("c", make); // should evict b

    // requesting b should create a new value (not equal to original b)
    int b_new = c.getOrInsert("b", make);
    TEST4FIMEX_CHECK(b_new != b);
}

TEST4FIMEX_TEST_CASE(test_lru_erase_clear)
{
    int counter = 0;
    LruCache<string, int> c(2);
    auto make = [&]() { return ++counter; };

    int a = c.getOrInsert("a", make);
    int b = c.getOrInsert("b", make);
    TEST4FIMEX_CHECK_EQ(c.size(), (size_t)2);

    c.erase("a");
    TEST4FIMEX_CHECK_EQ(c.size(), (size_t)1);

    int a_new = c.getOrInsert("a", make);
    TEST4FIMEX_CHECK(a_new != a);

    c.clear();
    TEST4FIMEX_CHECK_EQ(c.size(), (size_t)0);
}
