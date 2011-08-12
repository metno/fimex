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


#ifndef DATAHANDLER_H_
#define DATAHANDLER_H_

#include <boost/shared_ptr.hpp>
#include <string>

namespace MetNoFimex
{
class CDM;
class CDMVariable;
class Data;

namespace wdb
{
class WdbIndex;

/**
 * Handling data for wdb/fimex. Objects of this class may be used to add time
 * dimensions and -variables to CDM objects, and to get values for the
 * same variables.
 */
class DataHandler
{
public:
	virtual ~DataHandler() {}

	/**
	 * Add relevant dimensions and -variables to the given CDM object.
	 */
	virtual void addToCdm(CDM & cdm) const =0;

	/**
	 * Get data for the given variable name and unlimited dimension
	 */
	virtual boost::shared_ptr<Data> getData(const CDMVariable & variable, size_t unLimDimPos) const =0;

	/**
	 * Does the given cdm variable name refer to anything that this object can
	 * handle via the getData method?
	 */
	virtual bool canHandle(const std::string & cfName) const =0;

	/**
	 * Override this if subclass creates variables that needs entries in the coordinates attribute
	 */
	virtual void addToCoordinatesAttribute(std::string & coordinates, const std::string & wdbName) const {}

	typedef boost::shared_ptr<DataHandler> Ptr;
};

}

}


#endif /* DATAHANDLER_H_ */
