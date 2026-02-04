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

#ifndef FIMEX_STRINGUTILS_H_
#define FIMEX_STRINGUTILS_H_

#include "fimex/CDMException.h"

#include <regex>
#include <sstream>
#include <vector>

namespace MetNoFimex {

/**
 * Remove leading and trailing spaces.
 * @param str string to trim
 */
std::string trim(const std::string& str, const std::string& ws = " \t\n\r");

template <class T>
struct DefaultFormatter
{
    void operator()(std::ostream& out, const T& t) const { out << t; }
};

template <class InputIterator, class Formatter>
std::string join_formatted(InputIterator start, InputIterator end, const Formatter& fmt, const std::string& delim = ",")
{
    if (start == end)
        return "";
    std::ostringstream buffer;
    InputIterator current = start++;
    while (start != end) {
        fmt(buffer, *current);
        buffer << delim;
        current = start++;
    }
    fmt(buffer, *current);
    return buffer.str();
}

/**
 * Join values from an iterator to a string, using delimiter as separator.
 *
 * @param start
 * @param end
 * @param delim separator, default to ","
 */
template <class InputIterator>
std::string join(InputIterator start, InputIterator end, const std::string& delim = ",")
{
    return join_formatted(start, end, DefaultFormatter<typename std::iterator_traits<InputIterator>::value_type>(), delim);
}

/**
 * Join values from an iterator of pointers to a string, using delimiter as separator.
 *
 * @param start
 * @param end
 * @param delim separator, default to ","
 */
template <class InputIterator>
std::string joinPtr(InputIterator start, InputIterator end, std::string delim = ",")
{
    if (start == end)
        return "";
    std::ostringstream buffer;
    InputIterator current = start++;
    while (start != end) {
        buffer << **current << delim;
        current = start++;
    }
    buffer << **current;
    return buffer.str();
}

/**
 * Tokenize a string by a delimiter. This function will automaticall remove empty strings
 * at the beginning or anywhere inside the string.
 *
 * This function has been derived from http://www.oopweb.com/CPP/Documents/CPPHOWTO/Volume/C++Programming-HOWTO-7.html
 * @param str the string to tokenize
 * @param delimiters the delimiters between the tokens. That can be multiple delimiters, i.e. whitespace is " \t\n\r"
 * @return vector of tokens
 */
std::vector<std::string> tokenize(const std::string& str, const std::string& delimiters = " ");

template <class OutputIterator>
void split_any(OutputIterator out, const std::string& str, const std::string& delims)
{
    std::size_t previous = 0, current;
    while ((current = str.find_first_of(delims, previous)) != std::string::npos) {
        *(out++) = str.substr(previous, current - previous);
        previous = current + 1;
    }
    *(out++) = str.substr(previous);
}

std::vector<std::string> split_any(const std::string& str, const std::string& delims);

/**
 * convert a string to lowercase
 */
std::string string2lowerCase(const std::string& str);

bool starts_with(const std::string& txt, const std::string& start);
bool ends_with(const std::string& txt, const std::string& end);

std::string replace_all_copy(const std::string& in, char thys, char that);

/**
 * @brief Escape char for std::regex::extended
 */
void regex_escape(std::ostream& out, char ch);

/**
 * @brief Escape all chars from string for std::regex::extended
 */
void regex_escape(std::ostream& out, const std::string& s);

/**
 * @brief Escape all chars from string for std::regex::extended
 */
std::string regex_escape(const std::string& s);

} // namespace MetNoFimex

#endif // FIMEX_STRINGUTILS_H_
