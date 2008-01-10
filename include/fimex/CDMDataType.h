#ifndef CDMDATATYPE_H_
#define CDMDATATYPE_H_
#include <string>
namespace MetNoUtplukk
{

enum CDMDataType {
	CDM_NAT = 0,
	CDM_CHAR,
	CDM_SHORT,
	CDM_INT,
	CDM_FLOAT,
	CDM_DOUBLE,
	CDM_STRING
};

/// translate float/string/... to the appropriate CDMDataType
CDMDataType string2datatype(const std::string& s);
std::string datatype2string(CDMDataType type);

}
#endif /*CDMDATATYPE_H_*/
