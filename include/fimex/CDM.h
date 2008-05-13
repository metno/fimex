#ifndef CDM_H_
#define CDM_H_

#include <map>
#include <vector>
#include <string>
#include <ostream>
#include <boost/regex.hpp>
#include "CDMAttribute.h"
#include "CDMVariable.h"
#include "CDMDimension.h"
#include "CDMException.h"

namespace MetNoFimex
{
const int EARTH_RADIUS_M = 6371000;

/**
 * @brief Data structure of the Common %Data Model
 * 
 * This class implements the data-structure of the Common %Data Model version 1
 * http://www.unidata.ucar.edu/software/netcdf-java/CDM.html
 */
class CDM
{
public:
	typedef std::map<std::string, CDMAttribute> StrAttrMap;
	typedef std::map<std::string, StrAttrMap> StrStrAttrMap;
	typedef std::map<std::string, CDMDimension> StrDimMap;
	typedef std::map<std::string, CDMVariable> StrVarMap;
	CDM();
	virtual ~CDM();
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
	 * @brief search for variable with certain attribute-value
	 * 
	 * @param attrName name of the attribute
	 * @param attrValueRegExp regular expression the 'string'-value needs to match
	 * @return copies of the attributes matching the request 
	 */
	std::vector<std::string> findVariables(const std::string& attrName, const std::string& attrValueRegExp) const;
	/**
	 * @brief search for variable with attribute-values and dimensions
	 * 
	 * And AND search for attributes and dimensions.
	 * 
	 * @param findAttributes map with (attribute => string-value regExp) pairs
	 * @param findDimensions vector with dimensions contained in variable
	 * @return copies of the attributes matching the request 
	 */	
	std::vector<std::string> findVariables(const std::map<std::string, std::string>& findAttributes, const std::vector<std::string>& findDimensions) const;
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
	 * @brief get a reference to a dimension
	 * 
	 * @param dimName name of the dimension
	 * @throw CDMException if dimension doesn't exist
	 */
	CDMDimension& getDimension(const std::string& dimName) throw(CDMException);
	const CDMDimension& getDimension(const std::string& dimName) const throw(CDMException);

	
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
	const StrDimMap& getDimensions() const {return dimensions;}
	/// @brief get the variables
	const StrVarMap& getVariables() const {return variables;}
	/**
	 *  @brief get the attributes
	 *  @return map of type <variableName <attributeName, attribute>>
	 */
	const StrStrAttrMap& getAttributes() const {return attributes;}
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
	 */
	CDMAttribute& getAttribute(const std::string& varName, const std::string& attrName) throw(CDMException);
	/**
	 * @brief get an const attribute
	 * @param varName name of variable
	 * @param attrName name of attribute
	 */
	const CDMAttribute& getAttribute(const std::string& varName, const std::string& attrName) const throw(CDMException);
	/**
	 * get the fill value of an variable (_FillValue attribute)
	 * 
	 * @return value of _FillValue attribute, or MIFI_UNDEFINED_F
	 */
	double getFillValue(const std::string& varName) const;

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
	bool getProjectionAndAxesUnits(std::string& projectionName, std::string& xAxis, std::string& yAxis, std::string& xAxisUnits, std::string& yAxisUnits) const throw(CDMException);


private:
	StrStrAttrMap attributes;
	StrVarMap variables;
	StrDimMap dimensions;
};

}

#endif /*CDM_H_*/
