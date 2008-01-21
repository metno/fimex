#include "CDMDimension.h"

namespace MetNoUtplukk
{

CDMDimension::CDMDimension()
: name("_null"), length(0), unlimited(0)
{
}


CDMDimension::CDMDimension(std::string name, long length)
: name(name), length(length), unlimited(0)
{
}

CDMDimension::~CDMDimension()
{
}

void CDMDimension::toXMLStream(std::ostream& out) const
{
	out << "<dimension name=\"" << getName() << "\" length=\"" << getLength() << "\" ";
	if (isUnlimited()) {
		out << "isUnlimited=\"true\" ";
	}
	out << "/>" << std::endl;
}

}
