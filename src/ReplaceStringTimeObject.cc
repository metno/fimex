/*
 * Fimex
 * 
 * (C) Copyright 2008-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

#include "fimex/ReplaceStringTimeObject.h"

#include "fimex/String2Type.h"

#include <ostream>

namespace MetNoFimex {

ReplaceStringTimeObject::~ReplaceStringTimeObject() {}

std::ostream& operator<<(std::ostream& s, const ReplaceStringTimeObject& rsto)
{
	using namespace std;
	struct tm * timeinfo;
	char buffer [80];

	time_t newTime = rsto.myTime + rsto.offset;
	
	timeinfo = gmtime( &newTime );

	strftime(buffer, 80, rsto.myFormat.c_str(), timeinfo);
	s << buffer;
	return s;
}

void ReplaceStringTimeObject::setFormatStringAndOptions(const std::string& format, const std::vector<std::string>& options)
{
	myFormat = format;
	if (options.size() > 0) {
		offset = string2type<time_t>(options[0]);
	}
}

} // namespace MetNoFimex
