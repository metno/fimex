#include "CDMDimension.h"

namespace MetNoUtplukk
{

CDMDimension::CDMDimension()
: name("_null"), length(0)
{
}


CDMDimension::CDMDimension(std::string name, long length)
: name(name), length(length)
{
}

CDMDimension::~CDMDimension()
{
}

void CDMDimension::toXMLStream(std::ostream& out) const
{
	if (getLength() == CDM_UNLIMITED_DIMENSION) {
		out << "<dimension name=\"" << getName() << "\" isUnlimited=\"true\" />" << std::endl;
	} else {
		out << "<dimension name=\"" << getName() << "\" length=\"" << getLength() << "\" />" << std::endl;
	}
}

}
