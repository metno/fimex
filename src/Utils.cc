#include "Utils.h"
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <exception>

namespace MetNoFimex
{
std::string string2lowerCase(const std::string& str)
{
	std::string s(str);
	for (unsigned int i = 0; i < s.length(); i++) {
		s[i] = std::tolower(s[i]);
	}
	return s;
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


}
