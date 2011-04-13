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

#include "MetricGridInformation.h"
#include <fimex/CDM.h>
#include <fimex/CDMDimension.h>
#include <fimex/CDMVariable.h>
#include <fimex/CDMAttribute.h>
#include <fimex/Data.h>
#include <boost/assign/list_of.hpp>

namespace MetNoFimex
{

namespace wdb
{

MetricGridInformation::MetricGridInformation(PGresult * result, int row) :
		GridInformation(result, row)
{}

MetricGridInformation::MetricGridInformation(const boost::shared_ptr<Projection> & projection, unsigned numberX, unsigned numberY) :
		GridInformation(projection, numberX, numberY)
{}

MetricGridInformation::~MetricGridInformation()
{
}

void MetricGridInformation::addToCdm(CDM & cdm) const
{
	cdm.addDimension(CDMDimension("xc", numberX()));
	cdm.addDimension(CDMDimension("yc", numberY()));

	cdm.addVariable(CDMVariable("xc", CDM_FLOAT, std::vector<std::string>(1, "xc")));
	cdm.addAttribute("xc", CDMAttribute("standard_name", "projection_x_coordinate"));
	cdm.addAttribute("xc", CDMAttribute("units", "m"));

	cdm.addVariable(CDMVariable("yc", CDM_FLOAT, std::vector<std::string>(1, "yc")));
	cdm.addAttribute("yc", CDMAttribute("standard_name", "projection_y_coordinate"));
	cdm.addAttribute("yc", CDMAttribute("units", "m"));

	std::vector<std::string> dimensions = boost::assign::list_of("yc")("xc");

	cdm.addVariable(CDMVariable("lon1", CDM_FLOAT, dimensions));
	cdm.addAttribute("lon1", CDMAttribute("units", "degree_east"));
	cdm.addAttribute("lon1", CDMAttribute("long_name", "longitude"));
	cdm.addAttribute("lon1", CDMAttribute("standard_name", "longitude"));

	cdm.addVariable(CDMVariable("lat1", CDM_FLOAT, dimensions));
	cdm.addAttribute("lat1", CDMAttribute("units", "degree_north"));
	cdm.addAttribute("lat1", CDMAttribute("long_name", "latitude"));
	cdm.addAttribute("lat1", CDMAttribute("standard_name", "latitude"));
}

boost::shared_ptr<Data> MetricGridInformation::getField(const CDMVariable & variable) const
{
	boost::shared_ptr<Data> ret;

	const std::string & variableName = variable.getName();
	if ( variableName == "xc" )
	{
		ret = createData(variable.getDataType(), numberX());
		for ( unsigned i = 0; i < numberX(); ++ i )
			ret->setValue(i, startX() + (incrementX() * i));
	}
	else if ( variableName == "yc" )
	{
		ret = createData(variable.getDataType(), numberY());
		for ( unsigned i = 0; i < numberY(); ++ i )
			ret->setValue(i, startY() + (incrementY() * i));
	}
	else if ( variableName == "lon1" )
	{
		ret = createData(variable.getDataType(), numberX() * numberY());
		convertLatLon();
		double * output = reinterpret_cast<double *>(ret->getDataPtr());
		std::copy(longitudes_.begin(), longitudes_.end(), output);
	}
	else if ( variableName == "lat1" )
	{
		ret = createData(variable.getDataType(), numberX() * numberY());
		convertLatLon();
		double * output = reinterpret_cast<double *>(ret->getDataPtr());
		std::copy(longitudes_.begin(), longitudes_.end(), output);
	}
	else
		ret = GridInformation::getField(variable);

	return ret;
}

void MetricGridInformation::addSpatialDimensions(std::vector<std::string> & out) const
{
	out.push_back("xc");
	out.push_back("yc");
}

void MetricGridInformation::convertLatLon() const
{
	if ( latitudes_.empty() )
	{
		longitudes_.reserve(numberX() * numberY() );
		latitudes_.reserve(numberX() * numberY() );


		for ( unsigned i = 0; i < numberX(); ++ i )
			for ( unsigned j = 0; j < numberY(); ++ j)
				longitudes_.push_back(startX() + (incrementX() * i));

		for ( unsigned i = 0; i < numberX(); ++ i )
			for ( unsigned j = 0; j < numberY(); ++ j)
				latitudes_.push_back(startY() + (incrementY() * j));

		projection_->convertToLonLat(longitudes_, latitudes_);
	}
}


}

}
