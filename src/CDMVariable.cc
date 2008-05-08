#include "CDMVariable.h"

namespace MetNoUtplukk
{

CDMVariable::CDMVariable(std::string name, CDMDataType datatype, std::vector<std::string> shape)
: name(name), datatype(datatype), shape(shape)
{
}

CDMVariable::~CDMVariable()
{
}

bool CDMVariable::checkDimension(const std::string& dimension) const {
	const std::vector<std::string>& shape = getShape();
	if (std::find(shape.begin(), shape.end(), dimension) != shape.end()) {
		return true;
	}
	return false;
}

void CDMVariable::setAsSpatialVector(const std::string& counterpart, const std::string& direction) {
	spatialVectorCounterpart = counterpart;
	spatialVectorDirection = direction;
}


void CDMVariable::shapeToXMLStream(std::ostream& out) const
{
	if (shape.size() > 0) {
		out << " shape=\"";
		for (unsigned int i = 0; i < shape.size(); i++) {
			out << shape[i];
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

}
