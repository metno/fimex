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
#include "DataSanitizer.h"
#include "../WdbCDMReaderParserInfo.h"

namespace MetNoFimex
{

namespace wdb
{

GridData::GridData(const Parameter & param, const Level & lvl, int version, const Time & validTo, gid gridId) :
		parameter_(param),
		level_(lvl),
		version_(version),
		validTo_(validTo),
		placeName_("test grid"),
		gridIdentifier_(gridId)
{
}

namespace
{
std::string sqlString(const std::string & input)
{
	if ( input.empty() )
		return "NULL";
	return "'" + input + "'";
}
std::string array(const std::string & input)
{
	if ( input.empty() )
		return "NULL";
	return sqlString("{" + input + "}");
}
}


std::string GridData::query(const WdbCDMReaderParserInfo & querySpec, const DataSanitizer & sanitizer)
{
	const std::string dataProvider = array(sanitizer(querySpec.provider()));
	const std::string placeName = sqlString(sanitizer(querySpec.place()));
	const std::string referenceTime = sqlString(sanitizer(querySpec.referenceTime()));


	return "SELECT "
		"ValueParameterName, "
		"ValueParameterUnit, "
		"LevelParameterName, "
		"LevelUnitName, "
		"LevelFrom, LevelTo, "
		"DataVersion, "
		"extract(epoch from ReferenceTime),"
		"extract(epoch from ValidTimeFrom), "
		"extract(epoch from ValidTimeTo), "
		"PlaceName, "
		"value"
		" FROM "
		"wci.read(" + dataProvider + ", " + placeName + ", " + referenceTime + ",NULL, NULL,NULL, '{0}',NULL::wci.returngid)";
}

namespace
{
/// Indices for extracting fields from a wci.read tuple. This must exactly match the query above.
enum ReadIdx
{
	ValueParameterName,
	ValueParameterUnit,
	LevelParameterName,
	LevelUnitName,
	LevelFrom,
	LevelTo,
	DataVersion,
	ReferenceTime,
	ValidTimeFrom,
	ValidTimeTo,
	PlaceName,
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
	parameter_ = Parameter(GET(ValueParameterName), GET(ValueParameterUnit));
	level_ = Level(GET(LevelParameterName), GET(LevelUnitName), GETFLOAT(LevelFrom), GETFLOAT(LevelTo));
	version_ = GETINT32(DataVersion);
	referenceTime_ = GETTIME(ReferenceTime);
	validTo_ = GETTIME(ValidTimeTo);
	placeName_ = GET(PlaceName);
	gridIdentifier_ = GETINT64(Value);
}

GridData::~GridData()
{
}

}

}
