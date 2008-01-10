#include "CDMVariable.h"

namespace MetNoUtplukk
{

CDMVariable::CDMVariable(std::string name, CDMDataType datatype, std::vector<CDMDimension> shape)
: name(name), datatype(datatype), shape(shape)
{
}

CDMVariable::~CDMVariable()
{
}
void CDMVariable::toXMLStream(std::ostream& out, const std::map<std::string, CDMAttribute>& attrs) const
{
	out << "<variable name=\"" << getName() << "\" type=\"" << datatype2string(getDataType()) << "\">" << std::endl;
	for (std::map<std::string, CDMAttribute>::const_iterator it = attrs.begin(); it != attrs.end(); ++it) {
		it->second.toXMLStream(out);
	}
	out << "</variable>" << std::endl;
}

void CDMVariable::toXMLStream(std::ostream& out) const
{
	out << "<variable name=\"" << getName() << "\" type=\"" << datatype2string(getDataType()) << "\" />" << std::endl;
}
}
