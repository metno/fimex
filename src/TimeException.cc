#include "TimeException.h"

namespace MetNoUtplukk
{

TimeException::TimeException()
{
}

TimeException::TimeException(const string& msg)
: msg(msg)
{
}

TimeException::~TimeException() throw()
{
}

const char* TimeException::what() const throw()
{
	return msg.c_str();
}
}
