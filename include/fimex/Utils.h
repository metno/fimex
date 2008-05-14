#ifndef UTILS_H_
#define UTILS_H_

#include <vector>
#include <sstream>

namespace MetNoFimex
{


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

template<typename T>
T string2type(std::string s) {
	T retVal;
	std::stringstream buffer;
	buffer << s;
	buffer >> retVal;
	return retVal;
}


}

#endif /*UTILS_H_*/
