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

void CDMVariable::shapeToXMLStream(std::ostream& out) const
{
	if (shape.size() > 0) {
		out << "shape=\"";
		for (unsigned int i = 0; i < shape.size(); i++) {
			out << shape[i].getName();
			if (i < (shape.size()-1)) {
				out << " ";
			} else {
				out << "\" ";
			}
		}
	}	
}
void CDMVariable::toXMLStream(std::ostream& out, const std::map<std::string, CDMAttribute>& attrs) const
{
	out << "<variable name=\"" << getName() << "\" type=\"" << datatype2string(getDataType()) << "\" ";
	shapeToXMLStream(out);
	out << ">" << std::endl;
	for (std::map<std::string, CDMAttribute>::const_iterator it = attrs.begin(); it != attrs.end(); ++it) {
		it->second.toXMLStream(out);
	}
	out << "</variable>" << std::endl;
}

void CDMVariable::toXMLStream(std::ostream& out) const
{
	out << "<variable name=\"" << getName() << "\" type=\"" << datatype2string(getDataType()) << "\"";
	shapeToXMLStream(out);
	out << "/>" << std::endl;
}

int CDMVariable::hasUnlimitedDim() const
{
	return getUnlimitedDim() != 0;
}
const CDMDimension* CDMVariable::getUnlimitedDim() const {
	for (std::vector<CDMDimension>::const_iterator it = shape.begin(); it != shape.end(); ++it) {
		if (it->isUnlimited()) {
			return &(*it);
		}
	}
	return 0;
}
}
