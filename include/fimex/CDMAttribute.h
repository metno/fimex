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
	/// create a string attribute
	explicit CDMAttribute(std::string name, std::string value);
	/// create a int attribute with a int array of length 1
	explicit CDMAttribute(std::string name, int value);
	/// create a double attribute with a double array of length 1
	explicit CDMAttribute(std::string name, double value);
	virtual ~CDMAttribute();
	/// retrieve the name of the attribute
	const std::string& getName() const {return name;}
	/// retrieve the stringified value of the attribute
	const std::string getStringValue() const {return data->asString();}
	/// retrieve the data-pointer of the attribute
	const boost::shared_ptr<Data> getData() const {return data;}
	/// retrieve the datatype of the attribute
	const CDMDataType getDataType() const {return datatype;}
private:
	std::string variableName;
	std::string name;
	CDMDataType datatype;
	boost::shared_ptr<Data> data;
};

}

#endif /*CDMATTRIBUTE_H_*/
