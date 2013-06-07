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
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem.hpp>


namespace MetNoFimex
{

int round(double num) {
    return static_cast<int>(num < 0.0 ? std::ceil(num - 0.5) : std::floor(num + 0.5));
}

std::string trim(const std::string& str) {
	int pos1 = str.find_first_not_of(" ");
	int pos2 = str.find_last_not_of(" ");
	return str.substr(pos1, pos2+1);
}

std::string string2lowerCase(const std::string& str)
{
	std::string s(str);
	for (unsigned int i = 0; i < s.length(); i++) {
		s[i] = std::tolower(s[i]);
	}
	return s;
}

/**
 * specialization for high prececision
 */
template<>
std::string type2string<double>(double in)
{
    std::ostringstream buffer;
    buffer << std::setprecision(24) << in;
    return buffer.str();
}

// internal implementation of scanFiles
static void scanFiles_(std::vector<std::string>& files, const boost::filesystem::path& dir, int depth, const boost::regex& regexp, bool matchFileOnly, std::string currentRelDir = "", int depthCount = 0)
{
    using namespace std;
    using namespace boost::filesystem;
    LOG4FIMEX(getLogger("fimex.Utils"), Logger::DEBUG, "scanning directory " + dir.string());
    if (depthCount > 1000) {
        throw CDMException("possible circular reference: more than 1000 subdirectories found at " + dir.string());
    }
    using namespace boost::filesystem;
    vector<path> entries;
    copy(directory_iterator(dir), directory_iterator(), back_inserter(entries));
    sort(entries.begin(), entries.end());

    for (vector<path>::iterator e = entries.begin(); e != entries.end(); ++e) {
        file_status lstat(symlink_status(*e));
        if (lstat.type() == directory_file) {
            if (depth != 0) {
                if (!matchFileOnly) {
                    // remember the directory behind start-directory
#if BOOST_FILESYSTEM_VERSION == 3
                    currentRelDir += e->filename().string();
#else
                    currentRelDir += e->leaf();
#endif
                    currentRelDir += "/";
                }
                scanFiles_(files, *e, depth-1, regexp, matchFileOnly, currentRelDir, depthCount+1);
            }
        } else if (lstat.type() == regular_file) {
            string filename = matchFileOnly ? "" : currentRelDir;
#if BOOST_FILESYSTEM_VERSION == 3
            filename += e->filename().string();
#else
            filename += e->leaf();
#endif
            if (boost::regex_match(filename, regexp)) {
#if BOOST_FILESYSTEM_VERSION == 3
                files.push_back(e->string());
#else
                files.push_back(e->file_string());
#endif
            }
        }
    }
}
void scanFiles(std::vector<std::string>& files, const std::string& dir, int depth, const boost::regex& regexp, bool matchFileOnly)
{
    scanFiles_(files, boost::filesystem::path(dir), depth, regexp, matchFileOnly);
}
void globFiles(std::vector<std::string>& files, const std::string& glob)
{
    std::string reg = glob;

    // get the initial directory = everything without (*,?) until /
    boost::regex initialPathRegexp("^([^*?]*/)(.*)");
    std::string dir = ".";
    boost::smatch what;
    if (boost::regex_match(glob, what, initialPathRegexp)) {
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
    std::stringstream output;
    output << "\\Q";
    for (size_t i = 0; i < reg.size(); i++) {
        std::string charI = reg.substr(i,1);
        if (charI == "?") {
            output << "\\E[^/]?\\Q";
        } else if (charI == "*") {
            if (i < (reg.size()+1) && reg.substr(i+1,1) == "*") {
                i++;
                // two ** match also new directories
                output << "\\E.*\\Q";
            } else {
                output << "\\E[^/]*\\Q";
            }
        } else {
            output << charI;
        }
    }
    output << "\\E";
    boost::regex globReg(output.str());
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

static boost::posix_time::ptime epochBase(boost::gregorian::date(1970, boost::date_time::Jan, 1));
epoch_seconds posixTime2epochTime(const boost::posix_time::ptime& time)
{
    return (time - epochBase).total_seconds();
}

}
