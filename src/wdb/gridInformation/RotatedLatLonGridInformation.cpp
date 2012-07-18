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

#include "RotatedLatLonGridInformation.h"
#include <fimex/CDM.h>
#include <fimex/CDMDimension.h>
#include <fimex/CDMVariable.h>
#include <fimex/CDMAttribute.h>
#include <fimex/Data.h>
#include <boost/assign/list_of.hpp>
#include <set>


namespace MetNoFimex
{

namespace wdb
{

RotatedLatLonGridInformation::RotatedLatLonGridInformation(PGresult * result, int row) :
		GridInformation(result, row)
{}

RotatedLatLonGridInformation::RotatedLatLonGridInformation(const boost::shared_ptr<Projection> & projection, unsigned numberX, unsigned numberY) :
		GridInformation(projection, numberX, numberY)
{}

RotatedLatLonGridInformation::~RotatedLatLonGridInformation()
{
}

void RotatedLatLonGridInformation::addToCdm(CDM & cdm) const
{
	cdm.addDimension(CDMDimension("rlon", numberX()));
	cdm.addDimension(CDMDimension("rlat", numberY()));

	CDMVariable rlon("rlon", CDM_FLOAT, std::vector<std::string>(1, "rlon"));
	cdm.addVariable(rlon);
	cdm.addAttribute("rlon", CDMAttribute("long_name", "rotated longitude"));
	cdm.addAttribute("rlon", CDMAttribute("standard_name", "grid_longitude"));
	cdm.addAttribute("rlon", CDMAttribute("units", "degrees"));
	cdm.addAttribute("rlon", CDMAttribute("axis", "X"));


	CDMVariable rlat("rlat", CDM_FLOAT, std::vector<std::string>(1, "rlat"));
	cdm.addVariable(rlat);
	cdm.addAttribute("rlat", CDMAttribute("long_name", "rotated latitude"));
	cdm.addAttribute("rlat", CDMAttribute("standard_name", "grid_latitude"));
	cdm.addAttribute("rlat", CDMAttribute("units", "degrees"));
	cdm.addAttribute("rlat", CDMAttribute("axis", "Y"));

	std::vector<std::string> dimensions = boost::assign::list_of("rlon")("rlat");

	cdm.addVariable(CDMVariable("longitude", CDM_DOUBLE, dimensions));
	cdm.addAttribute("longitude", CDMAttribute("units", "degree_east"));
	cdm.addAttribute("longitude", CDMAttribute("long_name", "longitude"));
	cdm.addAttribute("longitude", CDMAttribute("standard_name", "longitude"));

	cdm.addVariable(CDMVariable("latitude", CDM_DOUBLE, dimensions));
	cdm.addAttribute("latitude", CDMAttribute("units", "degree_north"));
	cdm.addAttribute("latitude", CDMAttribute("long_name", "latitude"));
	cdm.addAttribute("latitude", CDMAttribute("standard_name", "latitude"));
}

std::string RotatedLatLonGridInformation::getCoordinatesAttribute() const
{
	return "longitude latitude";
}

DataPtr RotatedLatLonGridInformation::getField(const CDMVariable & variable) const
{
	DataPtr ret;

	const std::string & variableName = variable.getName();
	if ( variableName == "rlon" )
	{
		ret = createData(variable.getDataType(), numberX());
		for ( unsigned i = 0; i < numberX(); ++ i )
			ret->setValue(i, startX() + (incrementX() * i));
	}
	else if ( variableName == "rlat" )
	{
		ret = createData(variable.getDataType(), numberY());
		for ( unsigned i = 0; i < numberY(); ++ i )
			ret->setValue(i, startY() + (incrementY() * i));
	}
	else if ( variableName == "longitude" )
	{
		ret = createData(variable.getDataType(), numberX() * numberY());
		convertLatLon();
		double * output = reinterpret_cast<double *>(ret->getDataPtr());
		std::copy(longitudes_.begin(), longitudes_.end(), output);
	}
	else if ( variableName == "latitude" )
	{
		ret = createData(variable.getDataType(), numberX() * numberY());
		convertLatLon();
		double * output = reinterpret_cast<double *>(ret->getDataPtr());
		std::copy(latitudes_.begin(), latitudes_.end(), output);
	}
	else
		ret = GridInformation::getField(variable);

	return ret;
}

bool RotatedLatLonGridInformation::canHandle(const std::string & name) const
{
	std::set<std::string> names = boost::assign::list_of("rlon")("rlat")("longitude")("latitude");

	return names.find(name) != names.end() or GridInformation::canHandle(name);
}

void RotatedLatLonGridInformation::addSpatialDimensions(std::vector<std::string> & out) const
{
	out.push_back("rlon");
	out.push_back("rlat");
}

void RotatedLatLonGridInformation::convertLatLon() const
{
	if ( latitudes_.empty() )
	{
		longitudes_.reserve(numberX() * numberY() );
		latitudes_.reserve(numberX() * numberY() );


		for ( unsigned y = 0; y < numberY(); ++ y)
			for ( unsigned x = 0; x < numberX(); ++ x )
			{
				longitudes_.push_back(startX() + (incrementX() * x));
				latitudes_.push_back(startY() + (incrementY() * y));
			}

		projection_->convertToLonLat(longitudes_, latitudes_);
	}
}


}
}
