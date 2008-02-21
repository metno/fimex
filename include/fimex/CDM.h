#ifndef CDM_H_
#define CDM_H_

#include <map>
#include <vector>
#include <string>
#include <ostream>
#include "CDMAttribute.h"
#include "CDMVariable.h"
#include "CDMDimension.h"
#include "CDMException.h"

namespace MetNoUtplukk
{
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
	std::vector<std::string> findVariable(const std::string& attrName, const std::string& attrValueRegExp) const;
	/**
	 * @brief remove a variable and corresponding attributes
	 * 
	 * @param variableName the variable to remove
	 * @throw CDMException if variable or its dimensions don't exist
	 */
	void removeVariable(const std::string& variableName) throw(CDMException);

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
	
	CDMAttribute& getAttribute(std::string varName, std::string attrName) throw(CDMException);
private:
	StrStrAttrMap attributes;
	StrVarMap variables;
	StrDimMap dimensions;
};

}

#endif /*CDM_H_*/
