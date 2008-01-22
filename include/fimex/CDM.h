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

class CDM
{
public:
	typedef std::map<std::string, std::map<std::string, CDMAttribute> > StrStrAttrMap;
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
private:
	StrStrAttrMap attributes;
	StrVarMap variables;
	StrDimMap dimensions;
};

}

#endif /*CDM_H_*/
