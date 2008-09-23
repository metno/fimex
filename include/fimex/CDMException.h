/*
 * Fimex
 * 
 * (C) Copyright 2008, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

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
