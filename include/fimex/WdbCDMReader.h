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

#ifndef GXWDBCDMREADER_H_
#define GXWDBCDMREADER_H_

#include "fimex/CDMReader.h"

#include <map>
#include <vector>
#include <string>
#include <iosfwd>

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/noncopyable.hpp>

#include <libpq-fe.h>




namespace MetNoFimex
{
namespace wdb
{
class WdbConnection;
class Level;
}


class GxWdbCDMReader: public CDMReader, boost::noncopyable
{
public:
	GxWdbCDMReader(const std::string& source, const std::string& configfilename);
	virtual ~GxWdbCDMReader();

	virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException);

	std::ostream & indexSummary(std::ostream & s) const;

private:

	wdb::WdbConnection * wdbConnection_;


	void setupIndex_();
	void setupCDM_();

	// referencetime -> parameter -> level -> version -> validtime -> gid

	typedef long long gid;
	typedef boost::posix_time::ptime Time;
	typedef std::map<Time, gid> TimeEntry;
	typedef std::map<int, TimeEntry> VersionEntry;
	typedef std::map<wdb::Level, VersionEntry> LevelEntry;
	typedef std::string Parameter;
	typedef std::map<Parameter, LevelEntry> ParameterEntry;

	ParameterEntry data_;
};


}

#endif /* GXWDBCDMREADER_H_ */
