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

CDMAttribute::CDMAttribute(std::string name, double value)
: name(name), datatype(CDM_DOUBLE)
{
	boost::shared_array<double> dvalue(new double[1]);
	dvalue[0] = value;
	data = boost::shared_ptr<Data>(new DataImpl<double>(dvalue, 1));
}

CDMAttribute::CDMAttribute(std::string name, int value)
: name(name), datatype(CDM_INT)
{
	boost::shared_array<int> ivalue(new int[1]);
	ivalue[0] = value;
	data = boost::shared_ptr<Data>(new DataImpl<int>(ivalue, 1));
}


CDMAttribute::~CDMAttribute()
{
}


}
