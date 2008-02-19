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
	explicit CDMVariable(std::string name, CDMDataType datatype, std::vector<std::string> shape);
	virtual ~CDMVariable();
	const std::string& getName() const {return name;}
	CDMDataType getDataType() const {return datatype;}
	const std::vector<std::string>& getShape() const {return shape;}
	/// print a xml representation to the stream without attributes
	void toXMLStream(std::ostream& out) const;
	/// print a xml representation to the stream with attributes
	void toXMLStream(std::ostream& out, const std::map<std::string, CDMAttribute>& attrs) const;
	/// add data to the variable
	void setData(boost::shared_ptr<Data> data) {this->data = data;}
	/**
	 * @brief retrieve data from this variable
	 * 
	 * retrieve data, but only if it has been set previously by {@link setData()}
	 * this method will not try to read data from the disk
	 */ 
	const boost::shared_ptr<Data> getData() const {return data;}
	/// check if real data has been set with @link{setData()} (null-pointer reference returns false)
	int hasData() const {return (data.get() != 0);}
private:
	std::string name;
	CDMDataType datatype;
	std::vector<std::string> shape;
	void shapeToXMLStream(std::ostream& out) const;
	boost::shared_ptr<Data> data;
};

}

#endif /*CDMVARIABLE_H_*/
