#include "Utils.h"
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <exception>

namespace MetNoUtplukk
{
std::string string2lowerCase(const std::string& str)
{
	std::string s(str);
	for (unsigned int i = 0; i < s.length(); i++) {
		s[i] = std::tolower(s[i]);
	}
	return s;
}

}
