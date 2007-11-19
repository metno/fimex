#ifndef CDMATTRIBUTE_H_
#define CDMATTRIBUTE_H_

#include<string>
#include<boost::shared_ptr>
#include "Data.h"

namespace MetNoUtplukk
{

class CDMAttribute
{
public:
	CDMAttribute();
	virtual ~CDMAttribute();
};
private:
	int variableId;
	string name;
	boost::shared_ptr<Data> data;

}

#endif /*CDMATTRIBUTE_H_*/
