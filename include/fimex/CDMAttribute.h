#ifndef CDMATTRIBUTE_H_
#define CDMATTRIBUTE_H_

#include<string>
#include<boost/shared_ptr.hpp>
#include "Data.h"
#include "CDMDataType.h"

namespace MetNoUtplukk
{

class CDMAttribute
{
	
public:
	CDMAttribute();
	explicit CDMAttribute(std::string name, std::string value);
	virtual ~CDMAttribute();
	const std::string& getName() const {return name;}
	const std::string getStringValue() const {return data->asString();}
private:
	std::string variableName;
	std::string name;
	CDMDataType datatype;
	boost::shared_ptr<Data> data;
};

}

#endif /*CDMATTRIBUTE_H_*/
