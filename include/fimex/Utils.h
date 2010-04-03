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

#include <vector>
#include <sstream>
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include "fimex/CDMException.h"

namespace MetNoFimex
{
/**
 * Round a double to integer.
 */
int round(double num);

/**
 * Remove leading and trailing spaces.
 * @param str string to trim
 */
std::string trim(const std::string& str);

/**
 * Join values from an iterator to a string, using delimiter as separator.
 *
 * @param start
 * @param end
 * @param delim separator, default to ","
 */
template<class InputIterator>
std::string join(InputIterator start, InputIterator end, std::string delim = ",")
{
    if (start == end) return "";
    std::ostringstream buffer;
    InputIterator current = start++;
    while (start != end) {
        buffer << *current << delim;
        current = start++;
    }
    buffer << *current;
    return buffer.str();
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
 * @param delimiters the delimiters between the tokens
 * @return vector of tokens
 */
std::vector<std::string> tokenize(const std::string& str, const std::string& delimiters = " ");


/**
 * convert a string to lowercase
 */
std::string string2lowerCase(const std::string& str);

/**
 * convert a type (i.e. int, float) to string representation
 */
template<typename T>
std::string type2string(T in) {
	std::ostringstream buffer;
	buffer << in;
	return buffer.str();
}

/**
 * specialization for high prececision
 */
template<>
std::string type2string<double>(double in);


template<typename T>
T string2type(std::string s) {
	T retVal;
	std::stringstream buffer;
	buffer << s;
	buffer >> retVal;
	return retVal;
}

typedef long epoch_seconds;
/**
 * convert a posixTime to seconds sinc 1970-01-01
 * @param time time to convert
 */
epoch_seconds posixTime2epochTime(const boost::posix_time::ptime& time);


/**
 * convert a string with dots to a vector with type T
 * @param str f.e. 3.5,4.5,...,17.5
 * @param delimiter optional delimiter, defaults to ,
 */
template<typename T>
std::vector<T> tokenizeDotted(const std::string& str, const std::string& delimiter = ",") throw(CDMException)
{
	std::vector<std::string> tokens = tokenize(str, delimiter);
    std::vector<T> vals;
	bool pricks = false;
	for (std::vector<std::string>::iterator tok = tokens.begin(); tok != tokens.end(); ++tok) {
		std::string current = trim(*tok);
		if (current == "...") {
			pricks = true;
		} else {
			T val = string2type<T>(current);
			if (pricks == true) {
				pricks = false;
				size_t end = vals.size();
				if (end < 2) {
					throw CDMException("tokenizeDotted: cannot use ... expansion at position " + type2string(end-1) +", need at least two values before");
				}
				T last = vals[end-1];
				T dist = last - vals[end-2];
				T curVal = last + dist;
				while (curVal < val) {
					vals.push_back(curVal);
					curVal += dist;
				}
			}
			vals.push_back(val);
		}
	}
	return vals;
}

/** static_cast as a functor */
template<typename OUT>
struct staticCast {
    template<typename IN>
    OUT operator()(const IN& in) { return static_cast<OUT>(in); }
};


/**
 * Scale a value using fill, offset and scale
 */
template<typename IN, typename OUT>
class ScaleValue : public std::unary_function<IN, OUT>
{
private:
    IN oldFill_;
    double oldScale_;
    double oldOffset_;
    OUT newFill_;
    double newScale_;
    double newOffset_;
public:
    ScaleValue(double oldFill, double oldScale, double oldOffset, double newFill, double newScale, double newOffset) :
        oldFill_(static_cast<IN>(oldFill)), oldScale_(oldScale), oldOffset_(oldOffset),
        newFill_(static_cast<OUT>(newFill)), newScale_(newScale), newOffset_(newOffset) {}
    OUT operator()(const IN& in) const {
        if (in == oldFill_ || isinf(static_cast<double>(in))) {
            return newFill_;
        } else {
            return static_cast<OUT>(((oldScale_*in + oldOffset_)-newOffset_)/newScale_);
        }
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
        if (in == oldFill_ || isinf(static_cast<double>(in))) {
            return newFill_;
        } else {
            return static_cast<OUT>(in);
        }
    }
};

}

#endif /*UTILS_H_*/
