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

#ifndef FELT_FILE_ERROR_H_
#define FELT_FILE_ERROR_H_

#include <exception>
#include <string>

namespace MetNoFelt {
	
using namespace std;

class Felt_File_Error : public std::exception
{
private:
	std::string message;
public:
	explicit Felt_File_Error(const std::string& message);
	virtual ~Felt_File_Error() throw();
	virtual const char* what() const throw();
};

} // end namespace MetNoFelt

#endif /*FELT_FILE_ERROR_H_*/
