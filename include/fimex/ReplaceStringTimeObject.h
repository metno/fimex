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

#ifndef REPLACESTRINGTIMEOBJECT_H_
#define REPLACESTRINGTIMEOBJECT_H_

#include "ReplaceStringObject.h"
#include <ctime>

namespace MetNoFimex
{

class ReplaceStringTimeObject : public MetNoFimex::ReplaceStringObject
{
	std::time_t myTime;
	std::string myFormat;
public:
	ReplaceStringTimeObject() {}
	ReplaceStringTimeObject(std::time_t time, std::string format = "%Y-%m-%d %H:%M:%S%F%Q") : myTime(time), myFormat(format) {} 
	virtual ~ReplaceStringTimeObject() {}
	friend std::ostream& operator<<(std::ostream& s, const ReplaceStringTimeObject& rsto);
	virtual std::ostream& put(std::ostream& s) const { s << *this; return s;}
	/**
	 *  set the formatting String for this object
	 * 
	 * @param format: format string of strftime http://www.cplusplus.com/reference/clibrary/ctime/strftime.html
	 */
	virtual void setFormatString(std::string format) {myFormat = format;}

};

}

#endif /*REPLACESTRINGTIMEOBJECT_H_*/
