#ifndef TIMEEXCEPTION_H_
#define TIMEEXCEPTION_H_

#include <exception>
#include <string>

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
	virtual ~TimeException() throw();
    virtual const char* what() const throw();
};

}

#endif /*TIMEEXCEPTION_H_*/
