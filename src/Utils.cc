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

#include "fimex/Utils.h"
#include "fimex/Logger.h"

#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <exception>
#include <cmath>
#include <iomanip>

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

namespace MetNoFimex
{

static Logger_p logger = getLogger("fimex.Utils");

int RoundAndClamp::operator()(double d) const
{
  return clamped(round(d));
}

int RoundAndClamp::operator()(float f) const
{
  return clamped(round(f));
}

int RoundAndClamp::clamped(int r) const
{
  if (r>=mini && r<=maxi)
    return r;
  else
    return invalid;
}

std::string trim(const std::string& str, const std::string& ws)
{
    const size_t begin = str.find_first_not_of(ws);
    if (begin == std::string::npos)
        return std::string();
    const size_t end = str.find_last_not_of(ws);
    if (end == std::string::npos)
        return str.substr(begin);
    else
        return str.substr(begin, end + 1 - begin);
}

std::string string2lowerCase(const std::string& str)
{
    std::string s(str);
    for (unsigned int i = 0; i < s.length(); i++) {
        s[i] = std::tolower(s[i]);
    }
    return s;
}

static bool startsOrEndsWith(const std::string& txt, const std::string& sub, int startcompare)
{
    if (sub.empty())
        return true;
    if (txt.size() < sub.size())
        return false;
    return txt.compare(startcompare, sub.size(), sub) == 0;
}

bool starts_with(const std::string& txt, const std::string& start)
{
    return startsOrEndsWith(txt, start, 0);
}

bool ends_with(const std::string& txt, const std::string& end)
{
    return startsOrEndsWith(txt, end, ((int)txt.size()) - ((int)end.size()));
}

std::string replace_all_copy(const std::string& in, char thys, char that)
{
    std::string out = in;
    std::replace(out.begin(), out.end(), thys, that);
    return out;
}

template <>
std::ostream& type2stream<double>(std::ostream& out, double in)
{
    std::ostringstream buffer;
    buffer << std::setprecision(std::numeric_limits<double>::digits10 + 1) << in;
    out << buffer.str();
    return out;
}

static const std::string ESCAPE_THESE = "*?\\+.()[]{}|^$";

void regex_escape(std::ostream& out, char ch)
{
    if (ESCAPE_THESE.find(ch) != std::string::npos)
        out << '\\';
    out << ch;
}

void regex_escape(std::ostream& out, const std::string& s)
{
    for (char ch : s)
        regex_escape(out, ch);
}

std::string regex_escape(const std::string& s)
{
    std::ostringstream out;
    regex_escape(out, s);
    return out.str();
}

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
            if (ESCAPE_THESE.find(charI) != std::string::npos)
                output << '\\';
            output << charI;
        }
    }
    std::regex globReg(output.str());
    scanFiles(files, dir, depth, globReg, false);
}


std::vector<std::string> tokenize(const std::string& str, const std::string& delimiters)
{
    std::vector<std::string> tokens;
    // skip delimiters at beginning.
    std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // find first "non-delimiter".
    std::string::size_type pos     = str.find_first_of(delimiters, lastPos);

    while (std::string::npos != pos || std::string::npos != lastPos) {
        // found a token, add it to the vector.
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        // skip delimiters.  Note the "not_of"
        lastPos = str.find_first_not_of(delimiters, pos);
        // find next "non-delimiter"
        pos = str.find_first_of(delimiters, lastPos);
    }
    return tokens;
}

std::vector<std::string> split_any(const std::string& str, const std::string& delims)
{
    std::vector<std::string> out;
    split_any(std::back_inserter(out), str, delims);
    return out;
}

} // namespace MetNoFimex
