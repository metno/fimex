#include "CDM.h"

namespace MetNoUtplukk
{

CDM::CDM()
{
}

CDM::~CDM()
{
}

void CDM::addVariable(CDMVariable var) throw(CDMException)
{
	if (variables.find(var.getName()) == variables.end()) {
		variables.insert(std::pair<std::string, CDMVariable>(var.getName(), var));
	} else {
		throw CDMException("cannot add variable: " + var.getName() + " already exists");
	}
}
void CDM::addAttribute(std::string varName, CDMAttribute attr) throw(CDMException)
{
	if ((varName != globalAttributeNS ()) && (variables.find(varName) == variables.end())) {
		throw CDMException("cannot add attribute: variable " + varName + " does not exist");
	} else {
		std::map<std::string, CDMAttribute>& attrMap = attributes[varName];
		if (attrMap.find(attr.getName()) == attrMap.end()) {
			attrMap.insert(std::pair<std::string, CDMAttribute>(attr.getName(),attr));
		} else {
			throw CDMException("cannot add attribute: attribute " + varName + "." + attr.getName() + " already exists");
		}
	}  	
}


}
