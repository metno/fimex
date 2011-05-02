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
#include "LatLonGridInformation.h"
#include "RotatedLatLonGridInformation.h"
#include "MetricGridInformation.h"
#include "GridInformation.h"
#include "DataSanitizer.h"
#include <fimex/coordSys/Projection.h>
#include <fimex/Data.h>
#include <fimex/CDM.h>
#include <fimex/CDMVariable.h>
#include <boost/lexical_cast.hpp>
#include <boost/assign/list_of.hpp>
#include <boost/foreach.hpp>


namespace MetNoFimex
{

namespace wdb
{

std::string GridInformation::query(const std::string & gridName, const DataSanitizer & sanitizer)
{
	return "SELECT NumberX, NumberY, IncrementX, IncrementY, StartX, StartY, ProjDefinition FROM wci.getplaceregulargrid('" + gridName + "')";
}


namespace
{
/// Indices for extracting fields from a wci.read tuple. This must exactly match the query above.
enum ReadIdx
{
	NumberX, NumberY, IncrementX, IncrementY, StartX, StartY, ProjDefinition
};
#define GET(idx) PQgetvalue(result, row, idx)
#define GETUINT(idx) boost::lexical_cast<unsigned>(PQgetvalue(result, row, idx))
#define GETFLOAT(idx) boost::lexical_cast<float>(PQgetvalue(result, row, idx))
}



GridInformation::Ptr GridInformation::get(PGresult * result, int row)
{
	boost::shared_ptr<Projection> projection = Projection::createByProj4(GET(ProjDefinition));
	const std::string & projectionName = projection->getName();

	if ( projectionName == "latitude_longitude" )
		return Ptr(new LatLonGridInformation(result, row));
	else if ( projectionName == "rotated_latitude_longitude" )
		return Ptr(new RotatedLatLonGridInformation(result, row));
	else if ( not projection->isDegree() )
		return Ptr(new MetricGridInformation(result, row));

	throw CDMException("Unrecognized gird format: " + projection->getName());
}

GridInformation::Ptr GridInformation::get(const std::string & projDefinition, unsigned numberX, unsigned numberY)
{
	boost::shared_ptr<Projection> projection = Projection::createByProj4(projDefinition);
	const std::string & projectionName = projection->getName();

	if ( projectionName == "latitude_longitude" )
		return Ptr(new LatLonGridInformation(projection, numberX, numberY));
	else if ( projectionName == "rotated_latitude_longitude" )
		return Ptr(new RotatedLatLonGridInformation(projection, numberX, numberY));
	else if ( not projection->isDegree() )
		return Ptr(new MetricGridInformation(projection, numberX, numberY));

	throw CDMException("Unrecognized gird format: " + projection->getName());
}

GridInformation::GridInformation(PGresult * result, int row)
{
	projection_ = Projection::createByProj4(GET(ProjDefinition));
	incrementX_ = GETFLOAT(IncrementX);
	incrementY_ = GETFLOAT(IncrementY);
	numberX_ = GETUINT(NumberX);
	numberY_ = GETUINT(NumberY);
	startX_ = GETFLOAT(StartX);
	startY_ = GETFLOAT(StartY);
}

GridInformation::GridInformation(const boost::shared_ptr<Projection> & projection, unsigned numberX, unsigned numberY ) :
	projection_(projection),
	numberX_(numberX),
	numberY_(numberY),
	incrementX_(1),
	incrementY_(1),
	startX_(0),
	startY_(0)
{
}


GridInformation::~GridInformation()
{
}

std::string GridInformation::getProjectionName() const
{
	return "projection_" + projection_->getName();
}
boost::shared_ptr<Data> GridInformation::getField(const CDMVariable & variable) const
{
	boost::shared_ptr<Data> ret;

	std::string name = variable.getName();
	if ( name.substr(0, 11) == "projection_" )
	{
		ret = createData(variable.getDataType(), 0);
	}

	return ret;
}

}

}
