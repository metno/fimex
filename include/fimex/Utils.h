#ifndef UTILS_H_
#define UTILS_H_

#include <vector>
#include <sstream>

namespace MetNoUtplukk
{

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

}

#endif /*UTILS_H_*/
