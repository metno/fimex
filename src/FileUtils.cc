/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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
 */

#include "fimex/FileUtils.h"

#include "fimex/CDMException.h"
#include "fimex/Logger.h"
#include "fimex/StringUtils.h"

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <iomanip>
#include <iostream>

#if __cplusplus >= 201703L
#define HAVE_STD_FILESYSTEM 1
#else
//#define HAVE_BOOST_FILESYSTEM 1
#endif

#if defined(HAVE_STD_FILESYSTEM)
#warning "using std::filesystem"
#include <filesystem>
#elif defined(HAVE_BOOST_FILESYSTEM)
#warning "using boost::filesystem"
#include <boost/filesystem.hpp>
#else
#warning "using stat/dirent"
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>
#endif

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.FileUtils");

namespace {
#if defined(HAVE_STD_FILESYSTEM) || defined(HAVE_BOOST_FILESYSTEM)
#define HAVE_STD_OR_BOOST_FILESYSTEM 1
#if defined(HAVE_STD_FILESYSTEM)
namespace fs = std::filesystem;
#else
namespace fs = boost::filesystem;
#endif
typedef fs::path path_t;
#else // no ...::filesystem
typedef std::string path_t;
#endif

enum filetype_t { REGULAR_FILE, DIRECTORY, OTHER, ERROR };

std::string path_filename_string(const path_t& path)
{
#if defined(HAVE_STD_OR_BOOST_FILESYSTEM)
    return path.filename().string();
#else
    const size_t last_slash = path.find_last_of("/");
    if (last_slash == std::string::npos)
        return path;
    else
        return path.substr(last_slash + 1);
#endif
}

std::string path_string(const path_t& path)
{
#if defined(HAVE_STD_OR_BOOST_FILESYSTEM)
    return path.string();
#else
    return path;
#endif
}

std::vector<path_t> directory_contents(const path_t& dir)
{
    std::vector<path_t> entries;
#if defined(HAVE_STD_OR_BOOST_FILESYSTEM)
    copy(fs::directory_iterator(dir), fs::directory_iterator(), back_inserter(entries));
#else
    std::string prefix = dir;
    if (!prefix.empty() && prefix[prefix.size() - 1] != '/')
        prefix += '/';
    if (DIR* d = opendir(dir.c_str())) {
        /* print all the files and directories within directory */
        while (struct dirent* dent = readdir(d)) {
            const std::string name = dent->d_name;
            if (name != "." && name != "..")
                entries.push_back(prefix + name);
        }
        closedir(d);
    } else {
        // error
    }
#endif
    sort(entries.begin(), entries.end());
    return entries;
}

filetype_t file_type(const path_t& path)
{
#if defined(HAVE_STD_OR_BOOST_FILESYSTEM)
    try {
        fs::file_status stat = fs::file_status(fs::status(path));
        if (fs::is_directory(stat)) {
            return DIRECTORY;
        } else if (fs::is_regular_file(stat)) {
            return REGULAR_FILE;
        } else {
            return OTHER;
        }
    } catch (std::exception& ex) {
        return ERROR;
    }
#else
    struct stat sb;
    if (stat(path.c_str(), &sb) != 0)
        return ERROR;
    else if (S_ISDIR(sb.st_mode))
        return DIRECTORY;
    else if (S_ISREG(sb.st_mode))
        return REGULAR_FILE;
    else
        return OTHER;
#endif
}

} // namespace

// internal implementation of scanFiles
static void scanFiles_(std::vector<std::string>& files, const path_t& dir, int depth, const std::regex& regexp, bool matchFileOnly,
                       std::string currentRelDir = "", int depthCount = 0)
{
    LOG4FIMEX(logger, Logger::DEBUG, "scanning directory " + path_string(dir));
    if (depthCount > 1000) {
        throw CDMException("possible circular reference: more than 1000 subdirectories found at " + path_string(dir));
    }
    std::vector<path_t> entries = directory_contents(dir);
    for (const path_t& e : entries) {
        const filetype_t ft = file_type(e);
        if (ft == DIRECTORY) {
            if (depth != 0) {
                if (!matchFileOnly) {
                    // remember the directory behind start-directory
                    currentRelDir += path_filename_string(e) + "/";
                }
                scanFiles_(files, e, depth - 1, regexp, matchFileOnly, currentRelDir, depthCount + 1);
            }
        } else if (ft == REGULAR_FILE) {
            const std::string filename = (matchFileOnly ? "" : currentRelDir) + path_filename_string(e);
            if (std::regex_match(filename, regexp)) {
                files.push_back(path_string(e));
            }
        }
    }
}
void scanFiles(std::vector<std::string>& files, const std::string& dir, int depth, const std::regex& regexp, bool matchFileOnly)
{
    scanFiles_(files, path_t(dir), depth, regexp, matchFileOnly);
}

void globFiles(std::vector<std::string>& files, const std::string& glob)
{
    std::string reg = glob;

    // get the initial directory = everything without (*,?) until /
    std::regex initialPathRegexp("^([^*?]*/)(.*)");
    std::string dir = ".";
    std::smatch what;
    if (std::regex_match(glob, what, initialPathRegexp)) {
        dir = std::string(what[1].first, what[1].second);
        reg = std::string(what[2].first, what[2].second);
    }

    // ** might expand to multi-directories
    int depth = -1;
    if (reg.find("**") == std::string::npos) {
        depth = std::count(reg.begin(), reg.end(), '/');
    }

    // replace ** to .*
    // replace * to [^/]*
    // replace ? to [^/]?
    // and escaping everything else
    std::ostringstream output;
    for (size_t i = 0; i < reg.size(); i++) {
        char charI = reg[i];
        if (charI == '?') {
            output << "[^/]?";
        } else if (charI == '*') {
            if (i + 1 < reg.size() && reg[i + 1] == '*') {
                i++;
                // two ** match also new directories
                output << ".*";
            } else {
                output << "[^/]*";
            }
        } else {
            regex_escape(output, charI);
        }
    }
    std::regex globReg(output.str());
    scanFiles(files, dir, depth, globReg, false);
}

} // namespace MetNoFimex
