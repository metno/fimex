#ifndef CDMEXCEPTION_H_
#define CDMEXCEPTION_H_

#include <exception>
#include <string>

namespace MetNoFimex
{

class CDMException : public std::exception
{
public:
	CDMException() {}
	explicit CDMException (const std::string& msg) : msg(msg) {}
	CDMException(const CDMException& rhs) throw() : msg(rhs.msg) {}
	CDMException& operator=(const CDMException& rhs) throw() {if (this == &rhs) return *this; msg = rhs.msg; return *this;}
	virtual ~CDMException() throw() {}
    virtual const char* what() const throw() {return msg.c_str();}
private:
	std::string msg;	
};

}

#endif /*CDMEXCEPTION_H_*/
