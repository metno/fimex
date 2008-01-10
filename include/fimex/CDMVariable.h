#ifndef CDMVARIABLE_H_
#define CDMVARIABLE_H_

#include <string>
#include <vector>
#include <ostream>
#include <map>
#include "CDMAttribute.h"
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
	/// print a xml representation to the stream without attributes
	void toXMLStream(std::ostream& out) const;
	/// print a xml representation to the stream with attributes
	void toXMLStream(std::ostream& out, const std::map<std::string, CDMAttribute>& attrs) const;
private:
	std::string name;
	CDMDataType datatype;
	std::vector<CDMDimension> shape;
};

}

#endif /*CDMVARIABLE_H_*/
