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
#include <fimex/coordSys/Projection.h>
#include <boost/lexical_cast.hpp>


namespace MetNoFimex
{

namespace wdb
{

GridInformation::GridInformation(const std::string & projDefinition, unsigned numberX, unsigned numberY ) :
	numberX_(numberX), numberY_(numberY)
{
	projection_ = Projection::createByProj4(projDefinition);
}


GridInformation::~GridInformation()
{
}

std::string GridInformation::getProjectionName() const
{
	return "projection_" + projection_->getName();
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

GridInformation::GridInformation(PGresult * result, int row)
{
	projection_ = Projection::createByProj4(GET(ProjDefinition));
	numberX_ = GETUINT(NumberX);
	numberY_ = GETUINT(NumberY);
}

}

}
