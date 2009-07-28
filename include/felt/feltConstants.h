/*
 wdb

 Copyright (C) 2007 met.no

 Contact information:
 Norwegian Meteorological Institute
 Box 43 Blindern
 0313 OSLO
 NORWAY
 E-mail: wdb@met.no

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA
 */

#ifndef FELTCONSTANTS_H_
#define FELTCONSTANTS_H_

#include <algorithm>
#include <boost/static_assert.hpp>


namespace felt
{

/// A felt block "word" - 2 bytes
typedef short int word;

const size_t blockWords = 1024;
const size_t blockSize = blockWords * sizeof(word);
const size_t offsetToContentDefinition = 2;

/**
 * Is the given word a "missing" value? This means that there exist no valid
 * data for the given point
 */
inline bool isUndefined(word w)
{
	return -32767 == w;
}

}

BOOST_STATIC_ASSERT(sizeof(felt::word) == 2);


#endif /*FELTCONSTANTS_H_*/
