#ifndef CDMEXCEPTION_H_
#define CDMEXCEPTION_H_

#include <exception>
#include <string>

namespace MetNoUtplukk
{

class CDMException : public std::exception
{
public:
	CDMException() {}
	explicit CDMException (const std::string& msg) : msg(msg) {}
	virtual ~CDMException() throw() {}
    virtual const char* what() const throw() {return msg.c_str();}
private:
	std::string msg;	
};

}

#endif /*CDMEXCEPTION_H_*/
