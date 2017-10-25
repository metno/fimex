
#ifndef FIMEX_TESTINGHELPERS_H
#define FIMEX_TESTINGHELPERS_H 1

#include <string>

#include "fimex_config.h"

namespace MetNoFimex {

const std::string& topSrcDir();

bool exists(const std::string& path);
std::string require(const std::string& path);

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
