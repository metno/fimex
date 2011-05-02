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

#include "WdbConnection.h"
#include "DataSanitizer.h"
#include "gridInformation/GridInformation.h"
#include "WdbCDMReaderParser.h"
#include <boost/scoped_array.hpp>

namespace MetNoFimex
{
namespace wdb
{

WdbConnection::WdbConnection(const WdbCDMReaderParserInfo & connectionSpec)
{
	std::string connectString = connectionSpec.databaseConnectString();
	connection_ = PQconnectdb(connectString.c_str());
	if ( !isConnected() )
		throw WdbException(connection_);

	std::ostringstream begin;
	begin << "SELECT wci.begin('" << DataSanitizer(connection_)(connectionSpec.wciUser()) << "')";

	PQclear(call_(begin.str()));
}

WdbConnection::~WdbConnection()
{
	if (connection_)
		PQfinish(connection_);
}

bool WdbConnection::isConnected()
{
    return ( CONNECTION_OK == PQstatus(connection_) ) ;
}

namespace
{
/// Automatic freeing of PGresult objects
class Scoped_PGresult : boost::noncopyable
{
	PGresult * result_;
public:
	explicit Scoped_PGresult(PGresult * result) : result_(result) {}
	~Scoped_PGresult()
	{
		PQclear(result_);
	}
	PGresult * get()
	{
		return result_;
	}
};
}

void WdbConnection::readGid(std::vector<GridData> & out, const WdbCDMReaderParserInfo & connectionSpec)
{
	std::string query = GridData::query(connectionSpec, DataSanitizer(connection_));
	std::cout << query << std::endl;
	Scoped_PGresult result(call_(query));

	int tuples = PQntuples(result.get());
	for ( int i = 0; i < tuples; ++ i )
	{
		GridData gridData(result.get(), i);
		out.push_back(gridData);
	}

	// Add grid information to data
	for ( int i = out.size() - tuples; i < out.size(); ++ i )
	{
		GridData & gridData = out[i];
		GridInformationPtr gridInfo = readGridInformation(gridData.placeName());
		gridData.setGridInformation(gridInfo);
	}
}

WdbConnection::GridInformationPtr WdbConnection::readGridInformation(const std::string & gridName)
{
	GridList::const_iterator find = gridsInUse_.find(gridName);
	if ( find == gridsInUse_.end() )
	{
		Scoped_PGresult result(call_(GridInformation::query(gridName, DataSanitizer(connection_))));

		int tuples = PQntuples(result.get());
		if ( tuples == 0 )
			throw WdbException("Unknown grid name: " + gridName);
		else if ( tuples > 1 )
			throw WdbException("Ambiguous grid name: " + gridName);

		GridInformationPtr ret = GridInformation::get(result.get(), 0);
		find = gridsInUse_.insert(std::make_pair(gridName, ret)).first;
	}
	return find->second;
}


float * WdbConnection::getGrid(float * buffer, GridData::gid gridIdentifier)
{
	std::ostringstream query;
	query << "SELECT grid::bytea FROM wci.fetch(" << gridIdentifier << ", NULL::wci.grid)";

	Scoped_PGresult result(call_(query.str(), BinaryResult));

	int tuples = PQntuples(result.get());
	if ( tuples == 0 )
		throw WdbException("Unknown grid id");
	if ( tuples > 1 )
		throw WdbException("Internal error (got more than one grid from query");

	float * data = (float *) PQgetvalue(result.get(), 0, 0);
	int dataLength = PQgetlength(result.get(), 0, 0);

	if ( dataLength % sizeof(float) )
		throw WdbException("Invalid field size");

	float * ret = std::copy(data, data + (dataLength / 4), buffer);

	return ret;
}


PGresult * WdbConnection::call_(const std::string & query, QueryResultFormat resultFormat)
{

	PGresult * result = PQexecParams(connection_, query.c_str(), 0, NULL, NULL, NULL, NULL, resultFormat == BinaryResult ? 1 : 0);
	//PGresult * result = PQexec(connection_, query.c_str());
	if ( PQresultStatus(result) != PGRES_TUPLES_OK )
	{
		PQclear(result);
		throw WdbException(connection_);
	}
	return result;
}


WdbException::WdbException(PGconn * connection) :
		CDMException(PQerrorMessage(connection))
{}
WdbException::WdbException(const std::string & msg) :
		CDMException(msg)
{}


}
}
