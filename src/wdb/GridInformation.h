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

#ifndef GRIDINFORMATION_H_
#define GRIDINFORMATION_H_

#include <string>
#include <libpq-fe.h>
#include <boost/shared_ptr.hpp>

namespace MetNoFimex
{
class Projection;

namespace wdb
{

class GridInformation
{
public:
	~GridInformation();

	//const std::string & projDefinition() const { return projDefinition_; };
	const boost::shared_ptr<Projection> & getProjection() const { return projection_; }

	unsigned numberX() const { return numberX_; };
	unsigned numberY() const { return numberY_; };
	float incrementX() const { return incrementX_; }
	float incrementY() const { return incrementY_; }
	float startX() const { return startX_; }
	float startY() const { return startY_; }

	std::string getProjectionName() const;


	GridInformation(const std::string & projDefinition, unsigned numberX, unsigned numberY);


private:
	boost::shared_ptr<Projection> projection_;


	unsigned numberX_;
	unsigned numberY_;
	float incrementX_;
	float incrementY_;
	float startX_;
	float startY_;

	friend class WdbConnection;
	static std::string query(const std::string & gridName);
	GridInformation(PGresult * result, int row);
};

}

}

#endif /* GRIDINFORMATION_H_ */
