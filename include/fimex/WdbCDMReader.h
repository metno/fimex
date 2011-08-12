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

#ifndef WDBCDMREADER_H_
#define WDBCDMREADER_H_

#include "fimex/CDMReader.h"
#include <string>




namespace MetNoFimex
{

/**
 * CDM reader for wdb databases.
 *
 * Since wdb is a database system, a configuration file is used in place of a
 * "real" data file. This configuration file tells how to connect to a wdb
 * database, and what query to perform on it.
 *
 * Information about database specification files are given in the example
 * configuration file example.wdb.conf
 *
 * Syntax for global configuration is given in wdb_conf.xsd.
 *
 * Since there is much freedom in wdb, the generated CDMs from different wdb
 * instances can be very different from each other. In general all dimensions
 * of size one will be skipped in the resulting dimensions and variables.
 */
class WdbCDMReader: public CDMReader
{
public:

	/**
	 * Specifications of where to find the database, and what query to run on
	 * it is given in the file with name source. Generic specifications are
	 * given in the file with name configfilename.
	 */
	WdbCDMReader(const std::string& source, const std::string& configfilename);

	virtual ~WdbCDMReader();

	virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos);

	virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, const SliceBuilder& sb);

private:
	std::size_t getXSize(const CDMVariable& variable) const;
	std::size_t getYSize(const CDMVariable& variable) const;
	std::size_t getGridSize(const CDMVariable& variable) const;
	boost::shared_ptr<Data> extractDataFromField(const CDMVariable& variable, const std::vector<long long> & fieldIdentifiers) const;

	boost::shared_ptr<Data> cutGrid(const boost::shared_ptr<Data> & d, const CDMVariable& variable, const SliceBuilder & sb) const;

	boost::shared_ptr<Data> getDatabaseFields(const CDMVariable& variable, size_t unLimDimPos) const;
	boost::shared_ptr<Data> getDatabaseFields(const CDMVariable& variable, const SliceBuilder & sb) const;

	class InternalData;
	InternalData * d_;
};


}

#endif /* WDBCDMREADER_H_ */
