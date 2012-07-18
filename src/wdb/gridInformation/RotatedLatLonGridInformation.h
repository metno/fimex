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

#ifndef ROTATEDLATLONGRIDINFORMATION_H_
#define ROTATEDLATLONGRIDINFORMATION_H_

#include "GridInformation.h"

namespace MetNoFimex
{

namespace wdb
{

/**
 * A Grid where x and y corresponds directly to longitude and altitude, except
 * that the grid has been rotated in some manner.
 */
class RotatedLatLonGridInformation: public GridInformation
{
public:
	RotatedLatLonGridInformation(PGresult * result, int row);
	RotatedLatLonGridInformation(const boost::shared_ptr<Projection> & projection, unsigned numberX, unsigned numberY);
	virtual ~RotatedLatLonGridInformation();

	virtual void addToCdm(CDM & cdm) const;
	virtual std::string getCoordinatesAttribute() const;
	virtual DataPtr getField(const CDMVariable & variable) const;
	bool canHandle(const std::string & name) const;
	virtual void addSpatialDimensions(std::vector<std::string> & out) const;

private:
	void convertLatLon() const;
	mutable std::vector<double> longitudes_;
	mutable std::vector<double> latitudes_;
};

}

}

#endif /* ROTATEDLATLONGRIDINFORMATION_H_ */
