/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

#include "fimex/StringUtils.h"

#include "fimex/Logger.h"

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <iomanip>

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.StringUtils");

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

std::vector<std::string> tokenize(const std::string& str, const std::string& delimiters)
{
    std::vector<std::string> tokens;
    // skip delimiters at beginning.
    std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // find first "non-delimiter".
    std::string::size_type pos = str.find_first_of(delimiters, lastPos);

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
