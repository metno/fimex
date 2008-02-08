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
	void addVariable(CDMVariable var) throw(CDMException);
	void addDimension(CDMDimension dim) throw(CDMException);
	void addAttribute(std::string varName, CDMAttribute attr) throw(CDMException);
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
