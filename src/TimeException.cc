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

TimeException::TimeException(int uterrorcode)
: msg(uterror2string(uterrorcode))
{
}


TimeException::~TimeException() throw()
{
}

const char* TimeException::what() const throw()
{
	return msg.c_str();
}

string uterror2string(int errorcode) {
	string s;
	switch (errorcode) {
		case 0 : s = "success"; break;
		case UT_EOF : s = "end-of-file encountered"; break;
		case UT_ENOFILE : s = "no units-file"; break;
		case UT_ESYNTAX : s = "syntax error"; break;
		case UT_EUNKNOWN : s = "unknown specification"; break;
		case UT_EIO      : s = "I/O error"; break;
		case UT_EINVALID : s = "invalid unit-structure"; break;
		case UT_ENOINIT  : s = "package not initialized"; break;
		case UT_ECONVERT : s = "two units are not convertable"; break;
		case UT_EALLOC   : s = "memory allocation failure"; break;
		case UT_ENOROOM  : s = "insufficient room supplied"; break;
		case UT_ENOTTIME : s = "not a unit of time"; break;
		case UT_DUP      : s = "duplicate unit"; break;
		default : s = "unknown uterrorcode " + errorcode; break;
	}
	return "udunits-error: " + s;
}


}
