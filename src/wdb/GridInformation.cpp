/*
 fimex

 Copyright (C) 2011 met.no

 Contact information:
 Norwegian Meteorological Institute
 Box 43 Blindern
 0313 OSLO
 NORWAY
 E-mail: post@met.no

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

#include "GridInformation.h"
#include <boost/lexical_cast.hpp>


namespace MetNoFimex
{

namespace wdb
{

GridInformation::GridInformation(const std::string & projDefinition, unsigned numberX, unsigned numberY ) :
	projDefinition_(projDefinition), numberX_(numberX), numberY_(numberY)
{
}


GridInformation::~GridInformation()
{
}

std::string GridInformation::query(const std::string & gridName)
{
	return "SELECT NumberX, NumberY, ProjDefinition FROM wci.getplaceregulargrid('" + gridName + "')";
}

namespace
{
/// Indices for extracting fields from a wci.read tuple. This must exactly match the query above.
enum ReadIdx
{
	NumberX, NumberY, ProjDefinition
};
#define GET(idx) PQgetvalue(result, row, idx)
#define GETUINT(idx)boost::lexical_cast<unsigned>(PQgetvalue(result, row, idx))
}

//	parameter_ = Parameter(GET(ValueParameterName), GET(ValueParameterUnit));
//	level_ = Level(GET(LevelParameterName), GET(LevelUnitName), GETFLOAT(LevelFrom), GETFLOAT(LevelTo));
//	version_ = GETINT32(DataVersion);
//	validTo_ = GETTIME(ValidTimeTo);
//	gridIdentifier_ = GETINT64(Value);

GridInformation::GridInformation(PGresult * result, int row)
{
	projDefinition_ = GET(ProjDefinition);
	numberX_ = GETUINT(NumberX);
	numberY_ = GETUINT(NumberY);
}

}

}
