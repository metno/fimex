#ifndef TIMEEXCEPTION_H_
#define TIMEEXCEPTION_H_

#include <exception>
#include <string>
extern "C" {
#include <udunits.h>
}

namespace MetNoUtplukk
{

using namespace std;

class TimeException : public std::exception
{
private:
	std::string msg;
public:
	TimeException();
	explicit TimeException (const std::string& msg);
	explicit TimeException(int uterrorcode);
	virtual ~TimeException() throw();
    virtual const char* what() const throw();
};

string uterror2string(int errorcode);

}

#endif /*TIMEEXCEPTION_H_*/
