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
#include <vector>
#include <libpq-fe.h>
#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>

namespace MetNoFimex
{
class Projection;
class Data;
class CDMVariable;
class CDM;

namespace wdb
{

class GridInformation : boost::noncopyable
{
public:

	typedef boost::shared_ptr<GridInformation> Ptr;

	static Ptr get(PGresult * result, int row);
	static Ptr get(const std::string & projDefinition, unsigned numberX, unsigned numberY);

	virtual ~GridInformation();


	const boost::shared_ptr<Projection> & getProjection() const { return projection_; }

	unsigned numberX() const { return numberX_; };
	unsigned numberY() const { return numberY_; };
	float incrementX() const { return incrementX_; }
	float incrementY() const { return incrementY_; }
	float startX() const { return startX_; }
	float startY() const { return startY_; }

	std::string getProjectionName() const;

	virtual void addToCdm(CDM & cdm) const =0;
	virtual boost::shared_ptr<Data> getField(const CDMVariable & variable) const;

	virtual void addSpatialDimensions(std::vector<std::string> & out) const =0;

	static std::string query(const std::string & gridName);


protected:
	GridInformation(PGresult * result, int row);
	GridInformation(const boost::shared_ptr<Projection> & projection, unsigned numberX, unsigned numberY);

	boost::shared_ptr<Projection> projection_;

private:
	unsigned numberX_;
	unsigned numberY_;
	float incrementX_;
	float incrementY_;
	float startX_;
	float startY_;

};

}

}

#endif /* GRIDINFORMATION_H_ */
