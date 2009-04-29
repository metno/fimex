/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

#ifndef CDMATTRIBUTE_H_
#define CDMATTRIBUTE_H_

#include<string>
#include<ostream>
#include<boost/shared_ptr.hpp>
#include "fimex/Data.h"
#include "fimex/CDMDataType.h"
#include "fimex/CDMNamedEntity.h"

namespace MetNoFimex
{

class CDMAttribute : public CDMNamedEntity
{

public:
	CDMAttribute();
	/// create a string attribute
	explicit CDMAttribute(std::string name, std::string value);
	/// create a char attribute with a char array of length 1
	explicit CDMAttribute(std::string name, char value);
	/// create a int attribute with a int array of length 1
	explicit CDMAttribute(std::string name, int value);
	/// create a short attribute with a short array of length 1
	explicit CDMAttribute(std::string name, short value);
	/// create a float attribute with a float array of length 1
	explicit CDMAttribute(std::string name, float value);
	/// create a double attribute with a double array of length 1
	explicit CDMAttribute(std::string name, double value);
	/// create a attribute with the low level information
	explicit CDMAttribute(std::string name, CDMDataType datatype, boost::shared_ptr<Data> data);
	/// create a attribute from a string representation
	explicit CDMAttribute(const std::string& name, const std::string& datatype, const std::string& value) throw(CDMException);
	virtual ~CDMAttribute();
	/// retrieve the name of the attribute
	const std::string& getName() const {return name;}
    /// set the name of the attribute
    void setName(std::string newName) {name = newName;}
	/// retrieve the stringified value of the attribute
	const std::string getStringValue() const {return data->asString();}
	/// retrieve the data-pointer of the attribute
	const boost::shared_ptr<Data> getData() const {return data;}
	/// set the data for this attribute
	void setData(boost::shared_ptr<Data> data) {this->data = data;}
	/// retrieve the datatype of the attribute
	const CDMDataType getDataType() const {return datatype;}
	void toXMLStream(std::ostream& out) const;
private:
	std::string name;
	CDMDataType datatype;
	boost::shared_ptr<Data> data;
};

/**
 * @brief convert a proj4 string to a list of CDMAttributes usable for CF-1.0 projection variable
 *
 * currently, projStrings of the form +proj=[stere] +lat_0=? +lon_0=? +lat_ts=?
 */
std::vector<CDMAttribute> projStringToAttributes(std::string projStr);
/**
 * @brief convert attributes of a projection-variable to a projString
 *
 * @param attrs attributes of the projection variable
 * @return proj4 string
 */
std::string attributesToProjString(const std::vector<CDMAttribute>& attrs);


}

#endif /*CDMATTRIBUTE_H_*/
