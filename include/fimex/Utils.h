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

#ifndef UTILS_H_
#define UTILS_H_

#include "fimex/CDMException.h"
#include "fimex/UnitsConverter.h"

#include <cmath>
#include <iterator>
#include <limits>
#include <regex>
#include <sstream>
#include <utility>
#include <vector>

namespace MetNoFimex {

/**
 * convert a type (i.e. int, float) to string representation
 */
template <typename T>
std::ostream& type2stream(std::ostream& out, T in)
{
    out << in;
    return out;
}

//! specialization for high prececision
template <>
std::ostream& type2stream<double>(std::ostream& out, double in);

//! convert char from digits
template <>
inline std::ostream& type2stream<char>(std::ostream& out, char in)
{
    return type2stream(out, static_cast<int>(in));
}

//! convert unsigned char from digits
template <>
inline std::ostream& type2stream<unsigned char>(std::ostream& out, unsigned char in)
{
    return type2stream(out, static_cast<unsigned int>(in));
}

/**
 * convert a type (i.e. int, float) to string representation
 */
template <typename T>
std::string type2string(T in)
{
    std::ostringstream buffer;
    type2stream(buffer, in);
    return buffer.str();
}

//! no conversion for std::string
template <>
inline std::string type2string<std::string>(std::string in)
{
    return in;
}

template <typename T>
T string2type(const std::string& s)
{
    T retVal;
    bool ok = !s.empty();

    if (std::is_arithmetic<T>() && ok) {
        const char c = s[0];
        ok = (c == '-') || (std::is_floating_point<T>() && c == '.') || std::isdigit(c);
    }
    if (ok) {
        std::istringstream buffer(s);
        buffer >> retVal;
        ok = buffer.eof() && !buffer.fail();
    }
    if (!ok)
        throw std::runtime_error("could not convert '" + s + "'");
    return retVal;
}

//! convert char to digits, not bytes
template <>
inline char string2type(const std::string& s)
{
    return static_cast<char>(string2type<int>(s));
}

//! convert unsigned char to digits, not bytes
template <>
inline unsigned char string2type(const std::string& s)
{
    return static_cast<unsigned char>(string2type<unsigned int>(s));
}

//! no conversion for std::string
template <>
inline std::string string2type<std::string>(const std::string& s)
{
    return s;
}

/**
 * Round a double to integer.
 */
inline int round(double num)
{
    return ::lround(num);
}

/**
 * Round a float to integer.
 */
inline int round(float num)
{
    return ::lroundf(num);
}

/** Cast with rounding as functor.
 *
 * Rounding is used if destination type (OUT) is integer and original type (IN) is not.
 */
template<typename OUT, typename IN, bool R>
struct data_rounder;

template<typename OUT, typename IN>
struct data_rounder<OUT, IN, true> {
    inline OUT operator()(const IN& in) const { return static_cast<OUT>(round(in)); }
};

template<typename OUT, typename IN>
struct data_rounder<OUT, IN, false> {
    inline OUT operator()(const IN& in) const { return static_cast<OUT>(in); }
};

/** Type cast as a functor.
 *
 * Uses data_rounder for type conversion.
 */
template<typename OUT, typename IN>
struct data_caster {
    inline OUT operator()(const IN& in) const;
};

template<typename OUT, typename IN>
OUT data_caster<OUT, IN>::operator()(const IN& in) const
{
    return data_rounder<OUT, IN, std::numeric_limits<OUT>::is_integer && !std::numeric_limits<IN>::is_integer>()(in);
}

template<typename INOUT>
struct data_caster<INOUT, INOUT> {
    INOUT operator()(const INOUT& in) { return in; }
};

template<typename IN>
struct data_caster<std::string, IN> {
    std::string operator()(const IN& in) { return type2string(in); }
};

template<typename OUT>
struct data_caster<OUT, std::string> {
    OUT operator()(const std::string& in) { return string2type<OUT>(in); }
};

/** Round a double orr float to integer, and if the value is outside a range, replace with an "invalid" value.
 */
struct RoundAndClamp {
  int mini, maxi, invalid;

  RoundAndClamp(int vb, int ve, int inv)
    : mini(vb), maxi(ve), invalid(inv) { }

  int operator()(double d) const;
  int operator()(float f) const;

  int clamped(int r) const;
};

/**
 * Remove leading and trailing spaces.
 * @param str string to trim
 */
std::string trim(const std::string& str, const std::string& ws = " \t\n\r");

template<class T>
struct DefaultFormatter {
    void operator()(std::ostream& out, const T& t) const
      { out << t; }
};

template<class InputIterator, class Formatter>
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
template<class InputIterator>
std::string join(InputIterator start, InputIterator end, const std::string& delim = ",")
{
    return join_formatted(start, end, DefaultFormatter<typename std::iterator_traits<InputIterator>::value_type>(), delim);
}

/**
 * Find closest distinct elements in an unordered list. The order of elements is not defined.
 *
 * Except for the case where all elements are equal, it is always ensured that the neighbors
 * are distinct.

 * @param start
 * @param end
 * @param x
 * @return pair of the positions of a and b, with a closer than b
 */
template<typename InputIterator>
std::pair<typename std::iterator_traits<InputIterator>::difference_type, typename std::iterator_traits<InputIterator>::difference_type>
find_closest_distinct_elements(InputIterator start, InputIterator end, double x)
{
    using namespace std;
    typename iterator_traits<InputIterator>::difference_type retVal1 = 0;
    typename iterator_traits<InputIterator>::difference_type retVal2 = 0;
    InputIterator cur = start;
    typename iterator_traits<InputIterator>::value_type v1;
    double v1Diff, v2Diff;
    if (start != end) {
        v1 = *start;
        v1Diff = abs(x-*start);
        v2Diff = v1Diff;
    }
    while (cur != end) {
        double vDiff = fabs(x-*cur);
        if (vDiff <= v2Diff) {
            if (vDiff < v1Diff) {
                retVal2 = retVal1;
                v2Diff = v1Diff;
                v1 = *cur;
                retVal1 = distance(start, cur);
                v1Diff = vDiff;
            } else if (*cur != v1) {
                retVal2 = distance(start, cur);
                v2Diff = vDiff;
            }
        } // else nothing to be done
        cur++;
    }
    return make_pair(retVal1, retVal2);
}

/**
 * Find closest distinct neighbor elements in an unordered list, with a <= x < b
 * It might extrapolate if x is smaller than all elements (or x > all elements) and
 * fall back to find_closest_distinct_elements()
 *
 * Except for the case where all elements are equal, it is always ensured that the neighbors
 * are distinct.

 * @param start
 * @param end
 * @param x
 * @return pair of the positions of a and b, with a closer than b
 */
template<typename InputIterator>
std::pair<typename std::iterator_traits<InputIterator>::difference_type, typename std::iterator_traits<InputIterator>::difference_type>
find_closest_neighbor_distinct_elements(InputIterator start, InputIterator end, double x)
{
    using namespace std;
    if (start == end)
        return std::make_pair(0, 0);

    InputIterator lowest = start;
    InputIterator highest = start;
    InputIterator cur = start;
    double lowDiff = x - *cur;
    double highDiff = *cur -x;
    double maxDiff = std::numeric_limits<double>::max();
    if (lowDiff < 0)
        lowDiff = maxDiff;
    if (highDiff < 0)
        highDiff = maxDiff;
    while (++cur != end) {
        if (*cur <= x) {
            double diff = x - *cur;
            if (diff < lowDiff) {
                lowDiff = diff;
                lowest = cur;
            }
        } else {
            double diff = *cur - x;
            if (diff < highDiff) {
                highDiff = diff;
                highest = cur;
            }
        }
    }
    if (lowDiff == maxDiff || highDiff == maxDiff) {
        // extrapolating
        return find_closest_distinct_elements(start, end, x);
    }

    return std::make_pair(distance(start, lowest), distance(start, highest));
}

/**
 * Join values from an iterator of pointers to a string, using delimiter as separator.
 *
 * @param start
 * @param end
 * @param delim separator, default to ","
 */
template<class InputIterator>
std::string joinPtr(InputIterator start, InputIterator end, std::string delim = ",")
{
    if (start == end) return "";
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
 * normalize Longitude to be within [-180:180]
 * @param in longitude in degree
 * @return longitude in degree within [-180:180]
 */
template<typename T>
T normalizeLongitude180(T in) {
    while (in < -180) {
        in += 360;
    }
    while (in > 180) {
        in -= 360;
    }
    return in;
}

/**
 * convert a string with dots to a vector with type T
 * @param str f.e. 3.5,4.5,...,17.5
 * @param delimiter optional delimiter, defaults to ,
 */
template<typename T>
std::vector<T> tokenizeDotted(const std::string& str, const std::string& delimiter = ",")
{
    std::vector<std::string> tokens = tokenize(str, delimiter);
    std::vector<T> vals;
    for (std::vector<std::string>::iterator tok = tokens.begin(); tok != tokens.end(); ++tok) {
        std::string current = trim(*tok);
        if (current == "...") {
            size_t currentPos = vals.size();
            if (currentPos < 2) {
                throw CDMException("tokenizeDotted: cannot use ... expansion at position " + type2string(currentPos-1) +", need at least two values before");
            }
            T last = vals[currentPos-1];
            T dist = last - vals[currentPos-2];
            T curVal = last + dist;
            // positive if values get larger, negative if curVal gets samller
            double direction = (dist > 0) ? 1 : -1;
            if (++tok != tokens.end()) {
                T afterDotVal = string2type<T>(*tok);
                // expand the dots until before the afterDotVal, compare against rounding error
                double roundError = direction*dist*-1.e-5;
                while ((curVal - afterDotVal)*direction < roundError) {
                    vals.push_back(curVal);
                    curVal += dist;
                }
                // add the afterDotVal
                vals.push_back(afterDotVal);
            }
        } else {
            T val = string2type<T>(current);
            vals.push_back(val);
        }
    }
    return vals;
}

/**
 * template to declare isnan function in c++
 * @param x
 * @return same as C99 isnan pragma
 */
template<typename C>
int mifi_isnan(C x) {
    return std::isnan(x);
}

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

/**
 * Scan the filesystem for files matching the regexp. Can be used similar to 'glob'
 * or 'find' commands. The files will be sorted alphabetically.
 *
 * @param files output list of files
 * @param dir the input directory
 * @param depth the maximum number of directories to search (-1 is indefinite)
 * @param regexp the regular expression to match the file or complete path
 * @param matchFileOnly if true, the regexp will match the file-part only, if false,
 *        the complete path (behind dir) will be matched.
 */
void scanFiles(std::vector<std::string>& files, const std::string& dir, int depth, const std::regex& regexp, bool matchFileOnly);
/**
 * Similar to scanFiles, but uses glob instead, with * matches everything within a file or directory-name, ? matches exactly one character (not /),
 * and ** match everything even across multiple directories.
 *
 * @param files output list of files
 * @param glob the file/directory glob to match, glob-wildcards are *, ** and ?
 */
void globFiles(std::vector<std::string>& files, const std::string& glob);

/**
 * Scale a value using fill, offset and scale
 */
template<typename IN, typename OUT>
class ScaleValue : public std::unary_function<IN, OUT>
{
private:
    IN oldFill_;
    double oldScaleNewScaleInv_;
    double oldOffsetMinusNewOffsetNewScaleInv_;
    OUT newFill_;
public:
    ScaleValue(double oldFill, double oldScale, double oldOffset, double newFill, double newScale, double newOffset) :
        oldFill_(static_cast<IN>(oldFill)), oldScaleNewScaleInv_(oldScale/newScale),
        oldOffsetMinusNewOffsetNewScaleInv_((oldOffset-newOffset)/newScale),
        newFill_(static_cast<OUT>(newFill)) {}
    OUT operator()(const IN& in) const {
        return (in == oldFill_ || mifi_isnan<IN>(in))
            ? newFill_
            : data_caster<OUT, double>()(oldScaleNewScaleInv_*in + oldOffsetMinusNewOffsetNewScaleInv_);
            //(((oldScale_*in + oldOffset_)-newOffset_)/newScale_);
            // => ((oldScale_*in + oldOffsetMinusNewOffset_)*newScaleInv_);
            // => oldScaleNewScale_ * in + oldOffsetMinusNewOffsetNewScale_
    }
};

/**
 * Scale a value using fill, offset and scale, and a units-converter
 */
template<typename IN, typename OUT>
class ScaleValueUnits : public std::unary_function<IN, OUT>
{
private:
    IN oldFill_;
    double oldScale_;
    double oldOffset_;
    UnitsConverter_p uconv_;
    OUT newFill_;
    double newScaleInv_;
    double newOffset_;
public:
    ScaleValueUnits(double oldFill, double oldScale, double oldOffset, UnitsConverter_p uconv, double newFill, double newScale, double newOffset)
        : oldFill_(static_cast<IN>(oldFill))
        , oldScale_(oldScale)
        , oldOffset_(oldOffset)
        , uconv_(uconv)
        , newFill_(static_cast<OUT>(newFill))
        , newScaleInv_(1 / newScale)
        , newOffset_(newOffset)
    {
    }
    OUT operator()(const IN& in) const {
        return (in == oldFill_ || mifi_isnan<IN>(in))
                ? newFill_
                : data_caster<OUT, double>()((uconv_->convert(oldScale_*in + oldOffset_)-newOffset_)*newScaleInv_);
    }
};


/**
 * Change the missing value
 */
template<typename IN, typename OUT>
class ChangeMissingValue : public std::unary_function<IN, OUT>
{
private:
    IN oldFill_;
    OUT newFill_;
public:
    ChangeMissingValue(double oldFill, double newFill) :
        oldFill_(static_cast<IN>(oldFill)), newFill_(static_cast<OUT>(newFill)) {}
    OUT operator()(const IN& in) const {
        return (in == oldFill_ || mifi_isnan(in))
                ? newFill_
                : static_cast<OUT>(in);
    }
};

}

#endif /*UTILS_H_*/
