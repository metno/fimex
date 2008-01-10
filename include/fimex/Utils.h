#ifndef UTILS_H_
#define UTILS_H_

#include "CDMAttribute.h"
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

/**
 * @brief convert a proj4 string to a list of CDMAttributes usable for CF-1.0 projection variable
 * 
 * currently, projStrings of the form +proj=[stere] +lat_0=? +lon_0=? +lat_ts=?
 */
std::vector<CDMAttribute> projStringToAttributes(std::string projStr);


}

#endif /*UTILS_H_*/
