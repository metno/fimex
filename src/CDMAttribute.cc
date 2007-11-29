#include "CDMAttribute.h"
#include <cstring>

namespace MetNoUtplukk
{

CDMAttribute::CDMAttribute()
{
}

CDMAttribute::CDMAttribute(std::string name, std::string value)
: name(name), datatype(CDM_STRING)
{
	boost::shared_array<char> cstr(new char [value.size()+1]);
	std::strcpy (cstr.get(), value.c_str());
	data = boost::shared_ptr<Data>(new DataImpl<char>(cstr, value.size()+1));
}


CDMAttribute::~CDMAttribute()
{
}


}
