/*
 * Fimex, testUtils.cc
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
 *  Created on: Oct 5, 2009
 *      Author: Heiko Klein
 */


#include "testinghelpers.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#include "fimex/Utils.h"

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE(test_scaleValue)
{
    ScaleValue<int, double> sv(-32686, 2, 0, -2.7e11, 0.5, 1);
    double delta = 1e-5;
    BOOST_CHECK_CLOSE(-2., sv(0), delta);
    BOOST_CHECK_CLOSE(-2.7e11, sv(-32686), delta);
    BOOST_CHECK_CLOSE(2., sv(1), delta);
}

BOOST_AUTO_TEST_CASE(test_tokenizeDotted)
{
    vector<int> pos = tokenizeDotted<int>("12");
    BOOST_REQUIRE_EQUAL(1, pos.size());
    BOOST_CHECK_EQUAL(pos[0], 12);

    string dotted = "1.2,2.4,...,6";
    vector<float> tokens = tokenizeDotted<float>(dotted);
    BOOST_CHECK_EQUAL(5, tokens.size());
    BOOST_CHECK_CLOSE(tokens[4], 6.f, .1);

    string dotted2 = "1.2,2.4,...,4.8";
    vector<float> tokens2 = tokenizeDotted<float>(dotted2);
    BOOST_CHECK_EQUAL(4, tokens2.size());
    BOOST_CHECK_CLOSE(tokens2[2], 3.6f, .1);

    // backwards
    string dotted3 = "6,4.8,...,-1.2";
    vector<double> tokens3 = tokenizeDotted<double>(dotted3);
    BOOST_CHECK_EQUAL(7, tokens3.size());
    BOOST_CHECK_CLOSE(tokens3[2], 3.6, .1);

    // old, now fixed error, due to using abs instead of fabs
    string dotted4 = "90,89.96,...,50";
    vector<double> tokens4 = tokenizeDotted<double>(dotted4);
    BOOST_CHECK_EQUAL(40*25 + 1, tokens4.size());
    BOOST_CHECK_CLOSE(tokens4.at(tokens4.size()-2), 50.04, .1);

    // check in case end doesn't match forward
    string dotted5 = "10,20,...,35";
    vector<double> tokens5 = tokenizeDotted<double>(dotted5);
    BOOST_CHECK_EQUAL(4, tokens5.size());

    // check in case end doesn't match backward
    string dotted6 = "20,10,...,-35";
    vector<double> tokens6 = tokenizeDotted<double>(dotted6);
    BOOST_CHECK_EQUAL(7, tokens6.size());
}

BOOST_AUTO_TEST_CASE(test_find_closest_distinct_elements)
{
    int ary[8] = {1, 2, 3, 4, 4, -1, -2, 5};
    pair<size_t, size_t> p = find_closest_distinct_elements(&ary[0], &ary[0]+8, 1.5);
    BOOST_CHECK_EQUAL(p.first, 0);
    BOOST_CHECK_EQUAL(p.second, 1);

    p = find_closest_distinct_elements(&ary[0], &ary[0]+8, 4);
    BOOST_CHECK_EQUAL(p.first, 3);
    BOOST_CHECK_EQUAL(p.second, 7);

    p = find_closest_distinct_elements(&ary[0], &ary[0]+8, -1);
    BOOST_CHECK_EQUAL(p.first, 5);
    BOOST_CHECK_EQUAL(p.second, 6);

    p = find_closest_distinct_elements(&ary[0], &ary[0]+8, -3);
    BOOST_CHECK_EQUAL(p.first, 6);
    BOOST_CHECK_EQUAL(p.second, 5);

    p = find_closest_distinct_elements(&ary[0], &ary[0]+8, 5);
    BOOST_CHECK_EQUAL(p.first, 7);
    BOOST_CHECK_EQUAL(p.second, 3);

    // below follow test for find_closest_neighbor_distinct_elements
    p = find_closest_neighbor_distinct_elements(&ary[0], &ary[0]+8, 1.5);
    BOOST_CHECK_EQUAL(p.first, 0);
    BOOST_CHECK_EQUAL(p.second, 1);

    p = find_closest_neighbor_distinct_elements(&ary[0], &ary[0]+8, 4);
    BOOST_CHECK_EQUAL(p.first, 3);
    BOOST_CHECK_EQUAL(p.second, 7);

    p = find_closest_neighbor_distinct_elements(&ary[0], &ary[0]+8, -1);
    BOOST_CHECK_EQUAL(p.first, 5);
    BOOST_CHECK_EQUAL(p.second, 0);

    p = find_closest_neighbor_distinct_elements(&ary[0], &ary[0]+8, -3);
    BOOST_CHECK_EQUAL(p.first, 6);
    BOOST_CHECK_EQUAL(p.second, 5);

    p = find_closest_neighbor_distinct_elements(&ary[0], &ary[0]+8, 5);
    BOOST_CHECK_EQUAL(p.first, 7);
    BOOST_CHECK_EQUAL(p.second, 3);
}

BOOST_AUTO_TEST_CASE(test_scanFiles)
{
    vector<string> files;
    scanFiles(files, topSrcDir(), -1, boost::regex(".*stUti.?.?\\.cc"), true);
    BOOST_CHECK_EQUAL(files.size(), 1);
    BOOST_CHECK(files.at(0).find("testUtils.cc") != string::npos);
    files.clear();
    scanFiles(files, topSrcDir(), -1, boost::regex(".*stUti.?.?\\.cc"), false);
    BOOST_CHECK_EQUAL(files.size(), 1);
    BOOST_CHECK(files.at(0).find("testUtils.cc") != string::npos);
}

BOOST_AUTO_TEST_CASE(test_globFiles)
{
    vector<string> files;
    string glob = topSrcDir() + "/**stUti??.cc";
    globFiles(files, glob);
    BOOST_CHECK_EQUAL(files.size(), 1);
    BOOST_CHECK(files.at(0).find("testUtils.cc") != string::npos);
    files.clear();

    string glob2 = topSrcDir() + "/test/*stUti??.cc";
    globFiles(files, glob2);
    BOOST_CHECK_EQUAL(files.size(), 1);
    BOOST_CHECK(files.at(0).find("testUtils.cc") != string::npos);
}

#endif // HAVE_BOOST_UNIT_TEST_FRAMEWORK
