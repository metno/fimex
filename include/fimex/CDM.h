#ifndef CDM_H_
#define CDM_H_

#include <map>
#include <vector>
#include <string>
#include "CDMAttribute.h"
#include "CDMVariable.h"
#include "CDMDimension.h"
#include "CDMException.h"

namespace MetNoUtplukk
{

class CDM
{
public:
	CDM();
	virtual ~CDM();
	void addVariable(CDMVariable var) throw(CDMException);
	void addAttribute(std::string varName, CDMAttribute attr) throw(CDMException);
	/// @brief the namespace for global attributes
	const static std::string& globalAttributeNS() {const static std::string global("_GLOBAL"); return global;} 
	
private:
	std::map<std::string, std::map<std::string, CDMAttribute> > attributes;
	std::map<std::string, CDMVariable> variables;
	std::map<std::string, CDMDimension> dimensions;
};

inline std::vector<CDMAttribute> projStringToAttributes(std::string projStr) {return std::vector<CDMAttribute>();}// TODO: implement

}

#endif /*CDM_H_*/
