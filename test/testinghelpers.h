
#ifndef FIMEX_TESTINGHELPERS_H
#define FIMEX_TESTINGHELPERS_H 1

#include <string>

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

#endif // FIMEX_TESTINGHELPERS_H
