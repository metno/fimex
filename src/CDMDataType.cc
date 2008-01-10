#include "CDMDataType.h"
#include "Utils.h"

namespace MetNoUtplukk
{

CDMDataType string2datatype(const std::string& s) {
	std::string str(string2lowerCase(s));
	if (str == "double") { return CDM_DOUBLE; }
	else if (str == "float") { return CDM_FLOAT; }
	else if (str == "int") { return CDM_INT; }
	else if (str == "short") { return CDM_SHORT; }
	else if (str == "char") { return CDM_CHAR; }
	else if (str == "string") { return CDM_STRING; }
	else { return CDM_NAT; }
}

std::string datatype2string(CDMDataType type) {
	switch(type) {
	   case CDM_DOUBLE: return "double";
	   case CDM_FLOAT: return "float";
	   case CDM_INT: return "int";
	   case CDM_SHORT: return "short";
	   case CDM_CHAR: return "char";
	   case CDM_STRING: return "String";
	   default: return "NAT"; // not a type
	}
}


}