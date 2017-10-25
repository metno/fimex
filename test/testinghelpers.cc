
// prevent #include <boost/test/unit_test.hpp> from testinghelpers.h here
#define FIMEX_TESTINGHELPERS_NO_BOOST
#include "testinghelpers.h"

#include <stdexcept>
#include <fstream>

using std::string;

namespace {
const string top_src(TOP_SRCDIR);

const string src_share_etc(TOP_SRCDIR "/share/etc/");
const string src_test(TOP_SRCDIR "/test/");

const string extra_data_dir(TEST_EXTRADATA_DIR "/");
} // namespace

namespace MetNoFimex {

const std::string& topSrcDir()
{
    return top_src;
}

bool exists(const std::string& path)
{
    return std::ifstream(path.c_str()).is_open();
}

string require(const std::string& path)
{
    if (!exists(path))
        throw std::runtime_error("no such file: '" + path + "'");
    return path;
}

string pathShareEtc(const std::string& filename)
{
    return require(src_share_etc + filename);
}

string pathTest(const std::string& filename)
{
    return require(src_test + filename);
}

string pathTestExtra(const std::string& filename)
{
    return require(extra_data_dir + filename);
}

bool hasTestExtra()
{
    return exists(extra_data_dir + "flth00.dat");
}

void copyFile(const std::string& from, const std::string& to)
{
    std::ifstream  src(from.c_str());
    std::ofstream  dst(to.c_str());
    dst << src.rdbuf();
}

} // namespace MetNoFimex

#ifndef HAVE_BOOST_UNIT_TEST_FRAMEWORK
// no / old boost testframework
int main(int argc, char* args[]) {
}
#endif // !HAVE_BOOST_UNIT_TEST_FRAMEWORK
