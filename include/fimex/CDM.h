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

#ifndef CDM_H_
#define CDM_H_

#include <map>
#include <vector>
#include <string>
#include <ostream>
#include <boost/regex_fwd.hpp>
#include "fimex/CDMAttribute.h"
#include "fimex/CDMVariable.h"
#include "fimex/CDMDimension.h"
#include "fimex/CDMException.h"
#include "fimex/CDMconstants.h"
#include "fimex/coordSys/Projection.h"

namespace MetNoFimex
{
//forward declaration
struct CDMImpl;

/**
 * @headerfile "fimex/CDM.h"
 */

/**
 * @brief Data structure of the Common %Data Model
 *
 * This class implements the data-structure of the Common %Data Model version 1
 * http://www.unidata.ucar.edu/software/netcdf-java/CDM.html
 */
class CDM
{
public:
	typedef std::vector<CDMAttribute> AttrVec;
	typedef std::map<std::string, AttrVec> StrAttrVecMap;
	typedef std::vector<CDMDimension> DimVec;
	typedef std::vector<CDMVariable> VarVec;
	CDM();
	CDM(const CDM& rhs);
	virtual ~CDM();
	CDM& operator=(const CDM& rhs);
	/**
	 * @brief add variable to cdm
	 *
	 * @param var the variable to add
	 * @throw CDMException if var.varName() already exists
	 */
	void addVariable(const CDMVariable& var) throw(CDMException);
	/**
	 * @brief get a reference of a variable
	 *
	 * @param varName name of the variable
	 * @throw CDMException if varName doesn't exist
	 */
	CDMVariable& getVariable(const std::string& varName) throw(CDMException);
	/**
	 * @brief get a reference of a variable
	 *
	 * this is a constant version of @link{CDMVariable::getVariable}
	 *
	 * @param varName name of the variable
	 * @throw CDMException if varName doesn't exist
	 */
	const CDMVariable& getVariable(const std::string& varName) const throw(CDMException);
	/**
	 * @brief test if variable exists
	 *
	 * @param varName name of variable
	 */
	bool hasVariable(const std::string& varName) const;
	/**
	 * @brief search for variable with certain attribute-value
	 *
	 * @param attrName name of the attribute
	 * @param attrValueRegExp regular expression the 'string'-value needs to match
	 * @return variable names of the variable with attributes matching
	 */
	std::vector<std::string> findVariables(const std::string& attrName, const std::string& attrValueRegExp) const;
	/**
	 * @brief search for variable with attribute-values and dimensions
	 *
	 * And AND search for attributes and dimensions.
	 *
	 * @param findAttributes map with (attribute => string-value regExp) pairs
	 * @param findDimensions vector with dimensions contained in variable
	 * @return variable names of the variable with attributes matching the request and containing all dimensions
	 */
	std::vector<std::string> findVariables(const std::map<std::string, std::string>& findAttributes, const std::vector<std::string>& findDimensions) const;
	/**
	 * @brief rename a variable
	 *
	 * @param oldName the old name of the variable
	 * @param newName the new name of the variable
	 * @return 1 on success (oldName exists), 0 on failure
	 *
	 * @warning this will not change the spatialVectorCounterPart of all other variables
	 */
	bool renameVariable(const std::string& oldName, const std::string& newName);
	/**
	 * check if a variable contains a attributes with a matching string-value
	 *
	 * @param varName variable
	 * @param attribute the attribute name
	 * @param attrValue the regexp the string-value of the attribute will match against
	 */
	bool checkVariableAttribute(const std::string& varName, const std::string& attribute, const boost::regex& attrValue) const;
	/**
	 * @brief remove a variable and corresponding attributes
	 *
	 * @param variableName the variable to remove
	 */
	void removeVariable(const std::string& variableName);

	/**
	 *  @brief add a dimension to cdm
	 *
	 *  @param dim the dimension
	 *  @throw CDMException if dim-name already exists
	 */
	void addDimension(const CDMDimension& dim) throw(CDMException);
	/**
	 * check if the dimension exists
	 * @param dimName name of the dimension
	 */
	bool hasDimension(const std::string& dimName) const;

	/**
	 * @brief get a reference to a dimension
	 *
	 * @param dimName name of the dimension
	 * @throw CDMException if dimension doesn't exist
	 */
	CDMDimension& getDimension(const std::string& dimName) throw(CDMException);
	const CDMDimension& getDimension(const std::string& dimName) const throw(CDMException);

	/**
	 * @brief test if a dimension is actively in use
	 *
	 * @param name dimensionName
	 */
	bool testDimensionInUse(const std::string& name) const;

	/**
	 * @brief rename a dimension
	 *
	 * Rename a dimension.
	 *
	 * @return false if the original name does not exist.
	 * @throw CDMException if newName already in use in a variable but for a different dimension
	 */
	bool renameDimension(const std::string& oldName, const std::string& newName) throw(CDMException);

	/**
	 * @brief remove a dimension
	 *
	 * Remove a dimension, if it is not in use by a variable.
	 *
	 * @return true if dimension existed, false otherwise
	 * @throw CDMException if dimension in us in a variable
	 */
	bool removeDimension(const std::string& name) throw(CDMException);

	/**
	 * @brief retrieve the unlimited dimension
	 * @return unLimDim pointer with the unlimited dimension, the pointer will be deleted with the CDM
	 */
	const CDMDimension* getUnlimitedDim() const;
	/**
	 * @brief test if a variable contains the unlimited dim
	 * @return true/false
	 */
	bool hasUnlimitedDim(const CDMVariable& var) const;

	/**
	 * add an attribute to cdm
	 *
	 * @param varName name of the variablt the attribute belongs to
	 * @param attr the CDMAttribute
	 * @throw CDMException if varName doesn't exist, or attr.getName() already exists
	 */
	void addAttribute(const std::string& varName, const CDMAttribute& attr) throw(CDMException);
	/**
	 * add or replace an attribute of the cdm
	 *
	 * @param varName name of variable the attribute belongs to
	 * @param attr the CDMAttribute
	 * @throw CDMException if vaName doesn't exist
	 */
	void addOrReplaceAttribute(const std::string& varName, const CDMAttribute& attr) throw(CDMException);
	/**
	 * remove an attribute from the cdm
	 *
	 * @param varName name of variable the attribute belongs to
	 * @param attr the CDMAttribute
	 */
	void removeAttribute(const std::string& varName, const std::string& attrName);


	/// @brief print a xml representation to the stream
	void toXMLStream(std::ostream& os) const;
	/// @brief the namespace for global attributes
	const static std::string& globalAttributeNS() {const static std::string global("_GLOBAL"); return global;}

	/// @brief get the dimension
	const DimVec& getDimensions() const;
	/// @brief get the variables
	const VarVec& getVariables() const;
	/**
	 *  @brief get the attributes
	 *  @return map of type <variableName <attributeName, attribute>>
	 */
	const StrAttrVecMap& getAttributes() const;
	/**
	 * @brief get the attributes of an variable
	 * @param varName name of variable
	 */
	std::vector<CDMAttribute> getAttributes(const std::string& varName) const;


	/**
	 * @brief get an attribute
	 *
	 * @param varName name of variable
	 * @param attrName name of attribute
	 * @throw CDMException if varName attrName combination doesn't exists
	 */
	CDMAttribute& getAttribute(const std::string& varName, const std::string& attrName) throw(CDMException);
	/**
	 * @brief get a const. attribute
	 * @param varName name of variable
	 * @param attrName name of attribute
	 * @throw CDMException if varName attrName combination doesn't exists
	 */
	const CDMAttribute& getAttribute(const std::string& varName, const std::string& attrName) const throw(CDMException);
	/**
	 * @brief get an attribute without throwing an error
	 *
	 * This method will search for an attribute in the cdm. It will return
	 * true on success and return the attribute.
	 *
	 * @param varName name of variable
	 * @param attrName name of attribute
	 * @param retAttribute returns the attribute if found
	 * @return true when attribute has been found and set
	 */
	bool getAttribute(const std::string& varName, const std::string& attrName, CDMAttribute& retAttribute) const;
	/**
	 * get the fill value of an variable (_FillValue attribute)
	 *
	 * @return value of _FillValue attribute, or MIFI_UNDEFINED_F
	 */
	double getFillValue(const std::string& varName) const;
	/**
	 * get the value of the "units" attribute
	 * @return unitsString or ""
	 */
	std::string getUnits(const std::string& varName) const;
	/**
	 * @brief generate the projection coordinates (usually named "lat lon")
	 *
	 * @param projectionVariable the variable containing the projection information
	 * @param xDim the x dimension (the corresponding variable needs to contain data and units)
	 * @param yDim the y dimension (the corresponding variable needs to contain data and units)
	 * @param lonDim name of the longitude variable
	 * @param latDim name of the latitude variable
	 * @throw CDMException if any information is missing
	 */
	void generateProjectionCoordinates(const std::string& projectionVariable, const std::string& xDim, const std::string& yDim, const std::string& lonDim, const std::string& latDim) throw(CDMException);
	/**
	 * @brief extract the names of the projection-variable and the corresponding projection-axes
	 *
	 * @param projectionName output of the projection variables name
	 * @param xAxis output of the spatial x axis
	 * @param yAxis output of the spation y axis
	 * @param xAxisUnit output of unit for x axis
	 * @param yAxisUnit output of unit for y axis
	 * @return true if unique result, false (and print warning) if results are not unique
	 * @throw CDMException if no projection with corresponding axes can be found
	 */
	DEPRECATED(bool getProjectionAndAxesUnits(std::string& projectionName, std::string& xAxis, std::string& yAxis, std::string& xAxisUnits, std::string& yAxisUnits) const throw(CDMException));


	/**
	 * @brief get the projection attributes (as of CF-1.0) of a variable
	 *
	 * @param varName name of variable
	 * @return vector of attributes of the projection, an empty vector if no projection found
	 * @deprecated use the getProjectionOf() method
	 */
	DEPRECATED(AttrVec getProjection(std::string varName) const);
    /**
     * @brief get the projection of a variable
     *
     * This is the same as using the CoordinateSystem::getProjection().
     *
     * @param varName name of variable
     * @return projection
     */
	boost::shared_ptr<const Projection> getProjectionOf(std::string varName) const;

	/**
	 * @brief get the x-(lon) axis of the variable
	 *
	 * This is the same as using the CoordinateSystem::getGeoXAxis().
	 *
	 * @param varName name of variable
	 * @return name of x-axis dimension (or "" if not defined)
	 */
	std::string getHorizontalXAxis(std::string varName) const;
	/**
	 * @brief get the y-(lat) axis of the variable
     *
     * This is the same as using the CoordinateSystem::getGeoYAxis().
     *
	 * @param varName name of variable
	 * @return name of y-axis dimension (or "" if not defined)
	 */
	std::string getHorizontalYAxis(std::string varName) const;
	/**
	 * @brief detect the latitude and longitude coordinates of the variable
	 *
     * This is the same as using the CoordinateSystem::findAxisOfType() with CoordinateAxis::Lon and CoordinateAxis::Lat.
     *
	 * @param varName name of variable
	 * @param latitude return value of the latitude
	 * @param longitude return value of the longitude
	 * @return true if latitude and longitude have been found
	 */
	bool getLatitudeLongitude(std::string varName, std::string& latitude, std::string& longitude) const;
	/**
	 * @brief get the time axis of the variable
	 *
     * This is the same as using the CoordinateSystem::getTimeAxis().
     *
	 * @param varName name of variable
	 * @return name of time dimension (or "" if not defined)
	 */
	std::string getTimeAxis(std::string varName) const;
	/**
	 * @brief get the vertical axis of the variable
	 *
     * This is the same as using the CoordinateSystem::getGeoZAxis().
     *
	 * @param varName name of variable
	 * @return name of vertical dimension (or "" if not defined)
	 */
	std::string getVerticalAxis(std::string varName) const;

private:
	CDMImpl* pimpl_;
};

}

#endif /*CDM_H_*/
