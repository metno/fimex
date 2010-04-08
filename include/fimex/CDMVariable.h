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

#ifndef CDMVARIABLE_H_
#define CDMVARIABLE_H_

#include <string>
#include <vector>
#include <ostream>
#include "fimex/CDMAttribute.h"
#include "fimex/CDMDataType.h"
#include "fimex/CDMNamedEntity.h"

namespace MetNoFimex
{

/**
 * @headerfile "fimex/CDMVariable.h"
 */

class CDMVariable : public CDMNamedEntity
{
public:
	explicit CDMVariable(std::string name, CDMDataType datatype, std::vector<std::string> shape);
	virtual ~CDMVariable();
	const std::string& getName() const {return name;}
	void setName(std::string newName) {name = newName;}
	CDMDataType getDataType() const {return datatype;}
	void setDataType(CDMDataType type) {datatype = type;}
	const std::vector<std::string>& getShape() const {return shape;}
	void setShape(std::vector<std::string> newShape) {shape = newShape;}
	/**
	 * Declare this variable to be part of a spatial vector, e.g. (x-wind, y-wind)
	 *
	 * @param counterpart name of the other variable being part of this vector
	 * @param direction comma-separated list of possible directions for this vector, e.g. "x,longitude"
	 */
	void setAsSpatialVector(const std::string& counterpart, const std::string& direction);
	/**
	 * test if this variable has been declared to be a spatial vector
	 */
	bool isSpatialVector() const {return "" != spatialVectorCounterpart;}
	/// get the spatial counterpart of this vector
	const std::string& getSpatialVectorCounterpart() const {return spatialVectorCounterpart;}
	/// get the possible directions of this spatial vector (comma-separated string)
	const std::string& getSpatialVectorDirection() const {return spatialVectorDirection;}
	/**
	 * check the dimension of a variable
	 *
	 * @param dimension the dimension to check for
	 */
	bool checkDimension(const std::string& dimension) const;
	/// print a xml representation to the stream without attributes
	void toXMLStream(std::ostream& out) const;
	/// print a xml representation to the stream with attributes
	void toXMLStream(std::ostream& out, const std::vector<CDMAttribute>& attrs) const;
	/// add data to the variable
	void setData(boost::shared_ptr<Data> data) {this->data = data;}
	/**
	 * @brief retrieve volatile data from this variable
	 *
	 * Retrieve data, but only if it has been set previously by setData()
	 * this method will not try to read data from the disk. Use CDMReader::getData(const std::string& varName)
	 * to get the data from memory or from disk.
	 */
	const boost::shared_ptr<Data> getData() const {return data;}
	/// check if real data has been set with setData() (null-pointer reference returns false)
	int hasData() const {return (data.get() != 0);}
private:
	std::string name;
	CDMDataType datatype;
	std::vector<std::string> shape;
	void shapeToXMLStream(std::ostream& out) const;
	boost::shared_ptr<Data> data;
	std::string spatialVectorCounterpart;
	std::string spatialVectorDirection;
};

}

#endif /*CDMVARIABLE_H_*/
