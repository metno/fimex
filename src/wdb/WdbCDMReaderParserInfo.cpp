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

#include "WdbCDMReaderParserInfo.h"
#include "WdbCDMReaderParser.h"
#include <sstream>

namespace MetNoFimex
{

namespace wdb
{

WdbCDMReaderParserInfo::WdbCDMReaderParserInfo()
    : wdbPort_(5432)
{
}

std::string WdbCDMReaderParserInfo::databaseConnectString() const
{
	std::ostringstream ret;

	ret << "dbname=" << wdbName();
	if ( not wdbHost().empty() )
		ret << " host=" << wdbHost();
	ret << " port=" << wdbPort();
	if ( not wdbUser().empty() )
		ret << " user=" << wdbUser();

	return ret.str();
}

std::string WdbCDMReaderParserInfo::wdbHost() const
{
    return wdbHost_;
}

std::string WdbCDMReaderParserInfo::wdbName() const
{
    return wdbName_;
}

std::string WdbCDMReaderParserInfo::wdbUser() const
{
    return wdbUser_;
}

std::string WdbCDMReaderParserInfo::wciUser() const
{
    return wciUser_;
}

std::string WdbCDMReaderParserInfo::configFileName() const
{
    return configFileName_;
}

std::string WdbCDMReaderParserInfo::provider() const
{
    return provider_;
}

std::string WdbCDMReaderParserInfo::place() const
{
    return place_;
}

std::string WdbCDMReaderParserInfo::referenceTime() const
{
    return referenceTime_;
}

unsigned short WdbCDMReaderParserInfo::wdbPort() const
{
    return wdbPort_;
}


}

}
