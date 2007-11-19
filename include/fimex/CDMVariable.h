#ifndef CDMVARIABLE_H_
#define CDMVARIABLE_H_

#include <string>
#include "CDMDimension.h"

namespace MetNoUtplukk
{

enum CDMType {
	CDM_NAN = 0,
	CDM_CHAR,
	CDM_SHORT,
	CDM_INT,
	CDM_FLOAT,
	CDM_DOUBLE
};

class CDMVariable
{
public:
	CDMVariable();
	virtual ~CDMVariable();
private:
	int variableId; 
	string name;
	CDMType datatype;
	vector CDMDimension;
};

}

#endif /*CDMVARIABLE_H_*/
