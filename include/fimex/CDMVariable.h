#ifndef CDMVARIABLE_H_
#define CDMVARIABLE_H_

#include <string>
#include <vector>
#include "CDMDimension.h"
#include "CDMDataType.h"

namespace MetNoUtplukk
{

class CDMVariable
{
public:
	explicit CDMVariable(std::string name, CDMDataType datatype, std::vector<CDMDimension> shape);
	virtual ~CDMVariable();
	const std::string& getName() const {return name;}
	const CDMDataType& getDataType() const {return datatype;}
	const std::vector<CDMDimension>& getShape() const {return shape;}	
	
private:
	std::string name;
	CDMDataType datatype;
	std::vector<CDMDimension> shape;
};

}

#endif /*CDMVARIABLE_H_*/
