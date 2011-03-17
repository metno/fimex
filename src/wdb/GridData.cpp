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

#include "GridData.h"

namespace MetNoFimex
{

namespace wdb
{

std::string GridData::query(const std::string & dataProvider)
{
	return "SELECT "
		"ValueParameterName, "
		"LevelParameterName, LevelFrom, LevelTo, "
		"DataVersion, "
		"extract(epoch from ValidTimeFrom), "
		"extract(epoch from ValidTimeTo), "
		"value"
		" FROM "
		"wci.read('{" + dataProvider + "}',NULL, NULL,NULL, NULL,NULL, NULL,NULL::wci.returngid)";
}

namespace
{
/// Indices for extracting fields from a wci.read tuple. This must exactly match the query above.
enum ReadIdx
{
	ValueParameterName,
	LevelParameterName,
	LevelFrom,
	LevelTo,
	DataVersion,
	ValidTimeFrom,
	ValidTimeTo,
	Value
};
#define GET(idx) PQgetvalue(result, row, idx)
#define GETFLOAT(idx) boost::lexical_cast<float>(PQgetvalue(result, row, idx))
#define GETTIME(idx) boost::posix_time::from_time_t(boost::lexical_cast<long long>(GET(idx)))
#define GETINT32(idx)boost::lexical_cast<int>(PQgetvalue(result, row, idx))
#define GETINT64(idx)boost::lexical_cast<long long>(PQgetvalue(result, row, idx))
}

GridData::GridData(PGresult * result, int row)
{
	parameter_ = GET(ValueParameterName);
	level_ = Level(GET(LevelParameterName), GETFLOAT(LevelFrom),
			GETFLOAT(LevelTo));
	version_ = GETINT32(DataVersion);
	validTo_ = GETTIME(ValidTimeTo);
	gridIdentifier_ = GETINT64(Value);
}

GridData::~GridData()
{
}

}

}
